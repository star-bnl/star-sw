/////////////////////////////////////////////////////////////////////////////
//
// StMuEEmcSimuMaker
//
// Author: Jason C. Webb <jwebb@iucf.indiana.edu>
//
// StMuEEmcSimuMaker takes the geant energy stored in the micro-dst and
// converts it into an ADC response based on eemc database information.
// 
// For now, the code simply inverts energy = (adc-ped)/gain.  Future 
// versions will improve on this... i.e. realistic pedestals, simulated
// photostatistics, simulated attenuation along fibers, etc...
//
/////////////////////////////////////////////////////////////////////////////

#include "StMuEEmcSimuMaker.h"

#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcHit.h"

#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"

#include "TMath.h"

ClassImp(StMuEEmcSimuMaker);

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

StMuEEmcSimuMaker::StMuEEmcSimuMaker( const Char_t *myName ) 
  : StMaker(myName) 
{
  // Constructor

  mDbName = "eemcDb";

  //-- Initialize gains used in MC fill of StEvent
  mSampFrac = 0.05;
  const int maxAdc=4095;
  const int maxEtot=60;  // in GeV
  const float feta[kEEmcNumEtas]= {
    1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115
  };
  for ( Int_t i = 0; i < kEEmcNumEtas; i++ ) {    
    mTowerGains[i] = maxAdc/maxEtot/TMath::CosH(feta[i]);
  }

}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

Int_t StMuEEmcSimuMaker::Init()
{
  // Initializes the maker.

  //-- Get a pointer to the EEMC database maker
  mEEmcDb = (StEEmcDbMaker *)GetMaker( mDbName );
  return StMaker::Init();
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

Int_t StMuEEmcSimuMaker::Make()
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

void StMuEEmcSimuMaker::MakeTowers ( StMuEmcCollection *emc )
{
  // Loop over all tower hits, and determine a new ADC value based
  // on tower's MC energy response, but using database info for gains
  // and pedestals.

  //-- Loop over all 720 towers
  for ( Int_t ihit = 0; ihit < emc -> getNEndcapTowerADC(); ihit++ ) {
  
    Int_t adc, sec, sub, eta;
    emc -> getEndcapTowerADC ( ihit, adc, sec, sub, eta );
    
    sec--; // Counting from 1 is insane in c++!  It ends here.
    sub--; // Eveerything in my classes assume indexes from 0.
    eta--; //

    assert( sec >= 0 && sec < 12 ); // Indexing errors detected
    assert( sub >= 0 && sub < 5  ); // Indexing errors detected
    assert( eta >= 0 && eta < 12 ); // Indexing errors detected

    //--
    //-- Convert tower ADC to energy using MC fast simu gain
    //--
    Float_t energy = adc / mTowerGains[eta];



    //--
    //-- Convert energy back to ADC value based on pedestal and
    //--   gain from database.  DB expects sectors, etc... to be
    //--   numbered from 1, not the more sensible 0.
    //--
    static const Char_t subsec[] = { 'A','B','C','D','E' };
    const EEmcDbItem *x = mEEmcDb -> getTile( sec+1, subsec[sub], eta+1, 'T' );

    Float_t gain = x -> gain;
    Float_t ped  = x -> ped;



    //-- Convert back to adc
    adc = (Int_t)( energy * gain + ped );
    adc = (Int_t)((Float_t)adc *1.2); // ratio of 0.5 / 0.4 sampling fractions (kludge)

    if ( adc < 0 ) adc = 0;



    //-- And set this as the new ADC in the muDst... note that the
    //-- set method expects an id counted from 1, while we get it out
    //-- of the muDst with an id counted from 0.  AARRRGGGGGHHHHHH!
    emc -> setTowerADC ( ihit+1, adc, eemc );

    Int_t mySec, mySub, myEta, myAdc;
    emc -> getEndcapTowerADC ( ihit, myAdc, mySec, mySub, myEta );

  }


}

void StMuEEmcSimuMaker::MakeSmd( StMuEmcCollection *emc ) 
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

// $Log: StMuEEmcSimuMaker.cxx,v $
// Revision 1.1  2004/05/03 21:36:03  jwebb
// StMuEEmcSimuMaker -- maker to override the ADC values stored in Monte Carlo
// MuDsts with values calculated from gains in an instance of the StEEmcDbMaker.
//
