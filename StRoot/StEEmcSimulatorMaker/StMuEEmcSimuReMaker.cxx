/////////////////////////////////////////////////////////////////////////////
//
// StMuEEmcSimuReMaker
//
// Author: Jason C. Webb <jwebb@iucf.indiana.edu>
//
// StMuEEmcSimuReMaker takes the geant energy stored in the micro-dst and
// converts it into an ADC response based on eemc database information.
// In other words, it "remakes" the micro-dst before further processing.
// 
// For now, the code simply inverts energy = (adc-ped)/gain.  Future 
// versions will improve on this... i.e. realistic pedestals, simulated
// photostatistics, simulated attenuation along fibers, etc...
//
/////////////////////////////////////////////////////////////////////////////

#include "StMuEEmcSimuReMaker.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"

#include "TMath.h"

#include "StEEmcFastMaker.h"

ClassImp(StMuEEmcSimuReMaker);

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

StMuEEmcSimuReMaker::StMuEEmcSimuReMaker( const Char_t *myName ) 
  : StMaker(myName) 
{
  // Constructor

  mDbName = "eemcDb";

  StEEmcFastMaker *fast = new StEEmcFastMaker("quickie");
 
  //-- Initialize gains used in MC fill of StEvent (see 
  //-- StEEmcFastMaker).  Also sampling fraction, max
  //-- ADC and max E_T.
  mSampFrac    = fast -> getSamplingFraction();
  mMaxAdc      = fast -> getMaxAdc();
  mMaxET       = fast -> getMaxET();
  Float_t *tmp = fast -> getTowerGains();
  for ( Int_t i = 0; i < 12; i ++ ) 
    mTowerGains[i] = tmp[i];

  delete fast; //-- Only want to copy values, do not want to run it
  

}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

Int_t StMuEEmcSimuReMaker::Init()
{
  // Initializes the maker.

  //-- Get a pointer to the EEMC database maker
  mEEmcDb = (StEEmcDbMaker *)GetMaker( mDbName );
  return StMaker::Init();

}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

Int_t StMuEEmcSimuReMaker::Make()
{
  // Process one event.

  //-- Obtain a pointer to the muDst, and the emc collection
  StMuDstMaker *muDstMaker = (StMuDstMaker *)GetMaker("MuDst");
  StMuEmcCollection *emcCollection = muDstMaker -> muDst() -> emcCollection();

  MakeTowers( emcCollection );
  //$$$MakePreshower( emcCollection );
  MakeSmd( emcCollection );
  
  return kStOK;

}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

void StMuEEmcSimuReMaker::MakeTowers ( StMuEmcCollection *emc )
{
  // Loop over all tower hits, and determine a new ADC value based
  // on tower's MC energy response, but using database info for gains
  // and pedestals.

  //-- Loop over all 720 towers
  for ( Int_t ihit = 0; ihit < emc -> getNEndcapTowerADC(); ihit++ ) {
  
    Int_t adc, sec, sub, eta;
    emc -> getEndcapTowerADC ( ihit, adc, sec, sub, eta );    
    
    sec--; // Counting from 1 is insane in c++!  It ends here.
    sub--; // Everything in my classes assume indexes from 0.
    eta--; // Deal with it.

    assert( sec >= 0 && sec < 12 ); // Indexing errors detected
    assert( sub >= 0 && sub < 5  ); // Indexing errors detected
    assert( eta >= 0 && eta < 12 ); // Indexing errors detected

    //--
    //-- Convert tower ADC to (geant) energy using MC fast simu gain
    //--
    Float_t energy = adc / mTowerGains[eta];

    
    //-- Convert energy back to ADC value based on pedestal and
    //--   gain from database.  DB expects sectors, etc... to be
    //--   numbered from 1, not the more sensible 0.
    
    static const Char_t subsec[] = { 'A','B','C','D','E' };
    const EEmcDbItem *x = mEEmcDb -> getTile( sec+1, subsec[sub], eta+1, 'T' );

    Float_t gain = x -> gain;
    Float_t ped  = x -> ped;

    //-- Convert back to adc
    adc = (Int_t)( energy * gain / mSampFrac + ped );
    adc = (Int_t)((Float_t)adc *1.2); 
    //-- KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE KLUDGE --
    //-- ratio of 0.5 / 0.4 sampling fractions 
    
   
    //-- Negative ADC in the database is usually an error
    //-- condition... i.e. that the ADC hasn't been determined.
    if ( adc < 0 ) adc = 0;



    //-- And set this as the new ADC in the muDst.  Note that
    //-- setTowerADC expects ihit indexed from 1, while 
    //-- getEndcapTowerADC expects ihit indexed from 0.
    emc -> setTowerADC ( ihit+1, adc, eemc );

    //$$$ Int_t mySec, mySub, myEta, myAdc;
    //$$$ emc -> getEndcapTowerADC ( ihit, myAdc, mySec, mySub, myEta );

  }


}

void StMuEEmcSimuReMaker::MakeSmd( StMuEmcCollection *emc ) 
{
  // Loop over all SMD hits and determine a new ADC value based
  // on each strip's MC energy response, but using database info for gains
  // and pedestals.

  //-- Loop over all SMD hits
  Char_t cpl[] = { 'U','V' };

  for ( Int_t plane = 0; plane < 2; plane++ ) 

    for ( Int_t ihit = 0; ihit < emc -> getNEndcapSmdHits(cpl[plane]); ihit++ ) {
      
      Int_t sec, strip, adc;
      StMuEmcHit *hit = emc -> getEndcapSmdHit( cpl[plane], ihit, sec, strip );
      adc = hit -> getAdc();

      sec--;   // Counting from 1 is insane in c++!  It ends here.
      strip--; // Eveerything in my classes assume indexes from 0.

      assert( sec >= 0 && sec < 12 );      // Indexing errors detected
      assert( strip >= 0 && strip < 288 ); // Indexing errors detected


      //-- Get the database entry for this strip 
      const EEmcDbItem *x = mEEmcDb -> getByStrip ( sec+1, cpl[plane], strip+1 );

      //-- Energy deposited in the strip [GeV]
      Float_t energy = hit -> getEnergy();

      //-- Equivalent number of MIPs
      Float_t nmips = energy / 1.75E-3; // 1 Mip \approx 1.75 MeV
      Float_t gain = x -> gain;
      Float_t ped  = x -> ped;

      adc = (Int_t)( gain * nmips + ped );
      hit -> setAdc(adc);
         
    }//--<<


}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StMuEEmcSimuReMaker.cxx,v $
// Revision 1.1  2004/05/26 21:28:37  jwebb
// o Changes to StEEmcFastMaker to provide methods to get sampling fraction,
//   gains, etc...
//
// o StMuEEmcSimuMaker is now just a shell of its former self
//
// o Added StMuEEmcSimuReMaker.  This maker takes a muDst as input, and uses
//   the database maker to "massage" the ADC response, to better simulate
//   the calorimeter as installed.  For now, it simply uses the geant
//   energy response, combined with a single sampling fraction and the
//   database gains and pedestals to come up with a new ADC response.
//
// Revision 1.1  2004/05/03 21:36:03  jwebb
// StMuEEmcSimuReMaker -- maker to override the ADC values stored in Monte Carlo
// MuDsts with values calculated from gains in an instance of the StEEmcDbMaker.
//
