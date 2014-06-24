/**
 * \class StEEmcA2EMaker
 * \brief EEmc ADC --> energy maker
 *
 * This maker performs pedestal subtraction and gain corrections
 * to the EEMC data.  It currently supports MuDst and StEvent files
 * as input. 
 *
 * \author Jason C. Webb
 * $Date: 2014/06/24 21:11:20 $
 * $Revision: 1.8 $
 *
 */


#include "StEEmcA2EMaker.h"

//#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

//#include "StEventMaker/StEventMaker.h"

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"


/// stevent stuff
#include "StEvent/StEvent.h"
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcModule.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcCollection.h"

#include "StMessMgr.h"

#include <iostream>

ClassImp(StEEmcA2EMaker);

// ----------------------------------------------------------------------------
StEEmcA2EMaker::StEEmcA2EMaker(const Char_t *name) : StMaker(name)
{

  scale(1.0);

  mEEgeom=new EEmcGeomSimple();

  //$$$  std::cout << "StEEmcA2EMaker(" << name << ")" << std::endl;
  /// Clear all towers and init index
  for ( Int_t tower=0; tower < 720; tower++ ) {
    for ( Int_t layer=0; layer < 4; layer++ ) {
      mTowers[tower][layer].Clear("");
      mTowers[tower][layer].layer(layer);
      mTowers[tower][layer].index(tower);
    }
  }

  

  for ( Int_t i=0;i<12;i++ )
      for ( Int_t j=0;j<6;j++ )
      {
	  mEnergy[i][j] = 0.;
	  mHits[i][j]   = 0;
      } 
  


  /// Now set pointers to all neighboring towers
  for ( Int_t tower=0; tower < 720; tower++ ) 
    {
    for ( Int_t layer=0; layer < 4; layer++ ) 
      {

	Int_t phibin=mTowers[tower][layer].phibin();
	Int_t etabin=mTowers[tower][layer].etabin();

	//$$$std::cout << "tower " << mTowers[tower][layer].name()  << ":";
	  	
	for ( Int_t phi=phibin-1;phi<=phibin+1;phi++ ) 
	  for ( Int_t eta=etabin-1;eta<=etabin+1;eta++ ) 
	    {

	      /// no pointers to self
	      if ( phi==phibin && eta==etabin ) continue;

	      /// off edge of endcap
	      if ( eta<0 || eta>11) continue;

	      /// cyclical phi
	      Int_t kphi=phi;
	      if ( kphi<0 ) kphi+=60;
	      if ( kphi>59 ) kphi-=60;

	      Int_t neighbor=12*kphi+eta;
	      	      
	      if ((neighbor>=0) && (neighbor<720)) {
	        /// add pointer to neighboring tower
	        mTowers[tower][layer].neighbor( &mTowers[neighbor][layer] );
	      }
	    }


      }
    }

  /// Set high tower pointer
  mHighTower[0]=&mTowers[0][0];
  mHighTower[1]=&mTowers[0][1];
  mHighTower[2]=&mTowers[0][2];
  mHighTower[3]=&mTowers[0][3];

  /// Clear and init all strips
  for ( Int_t sec=0; sec<12; sec++ ) {
    for ( Int_t plane=0; plane<2; plane++ ) {
      for ( Int_t strip=0; strip<288; strip++ ) {       
	mStrips[sec][plane][strip].Clear("");
	mStrips[sec][plane][strip].sector(sec);
	mStrips[sec][plane][strip].plane(plane);
	mStrips[sec][plane][strip].index(strip);
      }
    }
  }

  /// Nullify pointers to makers
  mDbMaker = 0;

  /// Initialize default thresholds, nsigma above ped
  threshold(3.0,0); /// towers
  threshold(3.0,1); /// pre1
  threshold(3.0,2); /// pre2
  threshold(3.0,3); /// post
  threshold(3.0,4); /// smd-u
  threshold(3.0,5); /// smd-v

  /// Create storage banks for hit towers and strips.
  /// THESE SHOULD NOT BE CLEARED!

  /// Vector of towers, one per layer (0=T,1=P,2=Q,3=R)
  StEEmcTowerVec_t t;
  for ( Int_t i = 0; i < 4; i++ ) mHitTowers.push_back(t);

  /// Vector of strips, one per plane (0=U, 1=V)
  StEEmcStripVec_t s;
  std::vector< StEEmcStripVec_t > plane;
  plane.push_back(s); // U plane
  plane.push_back(s); // V plane
  for ( Int_t i=0; i < 12; i++ ) mHitStrips.push_back(plane);



}

// ----------------------------------------------------------------------------
StEEmcA2EMaker::~StEEmcA2EMaker()
{
    if (mEEgeom) delete mEEgeom;
    mEEgeom = 0;
}

// ----------------------------------------------------------------------------
Int_t StEEmcA2EMaker::Init()
{
  /// Get pointer to our database maker
  mDbMaker = (const StEEmcDb*)this->GetDataSet("StEEmcDb");
  return StMaker::Init();
}

// ----------------------------------------------------------------------------
Int_t StEEmcA2EMaker::Make()
{
  /// Lack of EEMC data, issue a warning
  if ( !readData() ) return kStWarn;
  return kStOK;
}

// ----------------------------------------------------------------------------
Bool_t StEEmcA2EMaker::readData()
{
  /// Verify that we have a pointer to input data
  const StMuDst* muDst = (const StMuDst*)GetInputDS("MuDst");
  if (muDst) {
	const StMuEmcCollection *emc = muDst->muEmcCollection();
	if (emc) {
	    {LOG_DEBUG << "Reading from MuDst..." << endm;}
	    return fillFromMuDst(emc);
	}
  }
  const StEvent *event = (const StEvent*)GetInputDS("StEvent");
  if (event) {
	const StEmcCollection *emc = event->emcCollection();
	if (emc) {
	    {LOG_DEBUG << "Reading from StEvent..." << endm;}
	    return fillFromSt(emc);
	}
	{LOG_WARN << "Cannot find emc in the event" << endm;}
	return false;
  } else {
    {LOG_WARN << "Cannot find neither event or mudst" << endm;}
  }
  return false;
}

// ----------------------------------------------------------------------------
Bool_t StEEmcA2EMaker::fillFromMuDst(const StMuEmcCollection *emc) 
{
  if (!emc) {
    {LOG_WARN << "Cannot find StMuEmcCollection" << endm;}
    return false;
  }

  LOG_DEBUG<<GetName()<<"::fillFromMuDst() N tower ADC = "<<emc -> getNEndcapTowerADC()<<endm;
  
  /// Loop over all towers
  for ( Int_t ihit = 0; ihit < emc -> getNEndcapTowerADC(); ihit++ ) {

    Int_t adc, sec, sub, eta;
    emc -> getEndcapTowerADC ( ihit, adc, sec, sub, eta );

    sec--; // Counting from 1 is insane in c++!  It ends here.
    sub--; // Everything in my classes assume indexes from 0.
    eta--; //

    if ((sec >= 0) && (sec < 12) && (sub >= 0) && (sub < 5) && (eta >= 0) && (eta < 12)) {
	addTowerHit(sec,sub,eta,adc,0);
    } else {
	// Indexing errors detected
    }
  }

  LOG_DEBUG<<GetName()<<"::fillFromMuDst() N pre/post ADC = "<<emc -> getNEndcapPrsHits()<<endm;

  /// Loop over all pre/postshower elements
  for ( Int_t ihit = 0; ihit < emc -> getNEndcapPrsHits(); ihit++ ) {

    Int_t adc, sec, sub, eta, det;
    const StMuEmcHit *hit = emc -> getEndcapPrsHit(ihit, sec, sub, eta, det);
    if (!hit) continue;

    adc = hit -> getAdc();

    sec--; // Counting from 1 is insane in c++!  It ends here.
    sub--; // Everything in my classes assume indexes from 0.
    eta--; //

    if ((sec >= 0) && (sec < 12) && (sub >= 0) && (sub < 5) && (eta >= 0) && (eta < 12) && (det >= 1) && (det < 4)) {
	addTowerHit(sec,sub,eta,(Float_t)adc,det);
    } else {
	// Indexing errors detected
    }
  }

  /// Loop over all smd hits
  Char_t cpl[] = { 'U','V' };
  for ( Int_t plane = 0; plane < 2; plane++ )
    for ( Int_t ihit = 0; ihit < emc -> getNEndcapSmdHits(cpl[plane]); ihit++ ) {

      Int_t sec, strip, adc;
      const StMuEmcHit *hit = emc -> getEndcapSmdHit( cpl[plane], ihit, sec, strip );
      adc = hit -> getAdc();

      sec--;   // Counting from 1 is insane in c++!  It ends here.
      strip--; // Everything in my classes assume indexes from 0.

      if ((sec >= 0) && (sec < 12) && (strip >= 0) && (strip < 288)) {
        addSmdHit(sec,plane,strip,(Float_t)adc);
      } else {
	// Indexing errors detected
      }
    }  
  return true;
}

// ----------------------------------------------------------------------------
Float_t StEEmcA2EMaker::addTowerHit( Int_t sec, Int_t sub, Int_t eta, Float_t adc, Int_t layer )
{
  Float_t energy = 0;
  if (!((sec>=0 && sec<12) && (sub>=0 && sub<5) && (eta>=0 && eta<12) && (layer>=0 && layer < 4))) {
    return energy;
  }
  Int_t index=60*sec+12*sub+eta;
  if (!((0<=index) && (index<720))) {
    // Just can't stay in bounds today
    return energy;
  }

  static const Char_t subsectors[] = { 'A','B','C','D','E' };
  static const Char_t detectors[] = { 'T', 'P', 'Q', 'R' };
  
  /// Get the database entry for this detector note, DB expects 
  /// sectors, eta counted from 1 not 0 for conformity with Star's ... 
  /// conventions.
    
  const EEmcDbItem *dbitem = mDbMaker ? mDbMaker -> getTile( sec+1,subsectors[sub],eta+1, detectors[layer] ) : 0;
  if (!dbitem) {
    return energy;
  }
  
  /// Copy fail and status bits
  mTowers[index][layer].fail( dbitem -> fail );
  mTowers[index][layer].stat( dbitem -> stat );

  /// Raw ADC should always be here
  mTowers[index][layer].raw(adc);

  /// Abort if the db has this marked as a bad or questionable channel
  if ( dbitem -> fail ) return energy; 
  //if ( dbitem -> stat ) return energy;

  Float_t ped       = dbitem -> ped;
  Float_t gain      = dbitem -> gain;
  Float_t threshold = ped + dbitem -> sigPed * mSigmaPed[layer];

  /// Ignore all ADC below a user-specified threshold
  if ( adc < threshold ) return energy;
  mHits[sec][layer]++; 

  /// Raw and ped subtracted adc
  mTowers[index][layer].adc(adc-ped);

  /// Make sure gain is positive, nonzero
  if ( gain <= 0. ) return energy;
  /// Determine energy
  energy = ( adc - ped + 0.5 ) / gain;
  
  /// And set detector response
  mTowers[index][layer].energy( energy * mScale );

  /// Determine tower center and set E_T
  UInt_t s=(UInt_t)mTowers[index][layer].sector();
  UInt_t ss=(UInt_t)mTowers[index][layer].subsector();
  UInt_t eb=(UInt_t)mTowers[index][layer].etabin();
  TVector3 momentum=mEEgeom -> getTowerCenter( s,ss,eb );
  momentum=momentum.Unit();
  momentum*=energy;
  mTowers[index][layer].et( (Float_t)momentum.Perp() );

  if ( !mTowers[index][layer].fail() )
      if ( adc - ped > mHighTower[layer]->adc() ) {
	  mHighTower[layer] = &mTowers[index][layer];
      }

  /// Push back list of all hit towers, preshower, postshower

  mHitTowers[layer].push_back( mTowers[index][layer] );
  mEnergy[sec][layer] += mTowers[index][layer].energy(); 

#if 0
  mTowers[index][layer].print();
#endif  
  return energy;
}

// ----------------------------------------------------------------------------
Float_t StEEmcA2EMaker::addSmdHit( Int_t sec, Int_t plane, Int_t strip, Float_t adc )
{
  Float_t energy = 0;
  if (!((0 <= sec) && (sec < 12) && (0 <= plane) && (plane < 2) && (0 <= strip) && (strip < 288))) {
    return energy;
  }

  /// Access the database.  Note that the DB counts from 1, not zero.
  static const Char_t planes[] = { 'U', 'V' };
  const EEmcDbItem *dbitem = mDbMaker ? mDbMaker -> getByStrip ( sec+1, planes[plane], strip+1 ) : 0;

  /// Check if null
  if ( dbitem == 0 ) return energy;

  /// Copy fail and status bits
  mStrips[sec][plane][strip].fail( dbitem -> fail );
  mStrips[sec][plane][strip].stat( dbitem -> stat );

  /// Raw ADC should always be here
  mStrips[sec][plane][strip].raw(adc);

  /// Abort if the db has this marked as bad or questionable 
  if ( dbitem -> fail ) return energy;
  //if ( dbitem -> stat ) return energy;

  Float_t ped       = dbitem -> ped;
  Float_t gain      = dbitem -> gain;
  Float_t threshold = ped + dbitem -> sigPed * mSigmaPed[4+plane];

  /// Ignore all ADC below a user-specified threshold
  if ( adc < threshold ) return energy;
  mHits[sec][plane+4]++; 

  /// Set raw and ped subtracted ADC
  mStrips[sec][plane][strip].adc(adc-ped);

 /// Make sure gain is positive, nonzero
  if ( gain <= 0. ) return energy;

  /// Determine energy
  energy = ( adc - ped + 0.5 ) / gain;

  mStrips[sec][plane][strip].energy(energy);

  mHitStrips[sec][plane].push_back( mStrips[sec][plane][strip] );

  mEnergy[sec][plane+4] += energy;
  return energy;
}

// ----------------------------------------------------------------------------
void StEEmcA2EMaker::Clear(Option_t *opts)
{
  StMaker::Clear(opts);  
  /// Clear hit towers and strips
  for ( Int_t layer = 0; layer < 4; layer++ ) 
    mHitTowers[layer].clear();
  
  for ( Int_t sector=0; sector<12; sector++ )
    for ( Int_t plane = 0; plane < 2; plane++ ) 
      mHitStrips[sector][plane].clear();


  /// Clear all towers and strips (for speed, we can
  /// clear only those which were "hit"... but KISS
  /// for now.)

  for ( Int_t i = 0; i < 720; i++ ) 
    for ( Int_t j = 0; j < 4; j++ ) 
      mTowers[i][j].Clear("");
  
  for ( Int_t i = 0; i < 12; i++ ) 
    for ( Int_t j = 0; j < 2; j++ ) 
      for ( Int_t k = 0; k < 288; k++ )
	mStrips[i][j][k].Clear("");

  /// Clear sector energy sums
  for ( Int_t i = 0; i < 12; i++ ) 
    for ( Int_t j = 0; j < 6; j++ ) { 
      mEnergy[i][j] = 0.; 
      mHits[i][j]=0;
    }
}

// ----------------------------------------------------------------------------
Bool_t StEEmcA2EMaker::fillFromSt(const StEmcCollection *emc)
{
  ///
  /// Set StEmcRawHit pointers in StEEmcTower, towers
  ///
  const StEmcDetector *detector=emc->detector(kEndcapEmcTowerId);
  if ( !detector )
    {
      Warning("fillFromSt","\n**********\nemc->detector() NULL for eemc towers.  MAJOR PROBLEM, trying to procede.\n**********\n\n");
      return false;
    }

  for ( UInt_t sec = 0; sec < detector -> numberOfModules(); sec++ )
    {

      /// Remember to watch out for fortran holdovers in
      /// the numbering scheme
      const StEmcModule *sector = detector -> module( sec+1 );
      if (!sector) {
	continue;
      }
      const StSPtrVecEmcRawHit &hits = sector->hits();

      /// Loop over all raw hits in this sector
      for ( UInt_t ihit=0; ihit<hits.size(); ihit++ )
        {

          if (!hits[ihit]) {
	    continue;
	  }

          /// these are returned indexed from 1, offset 
	  /// into local scheme (indexed from 0)
          Int_t isector    = hits[ihit]->module() - 1;
          Int_t isubsector = hits[ihit]->sub() - 1;
          Int_t ietabin    = hits[ihit]->eta() - 1;
	  Int_t adc        = hits[ihit]->adc();
	  if (!((isector >= 0) && (isector < 12) && (isubsector >= 0) && (isubsector < 5) && (ietabin >= 0) && (ietabin < 12))) {
	    continue;
	  }

	  Int_t iphibin = 5 * isector + isubsector;
	  Int_t index = 12 * iphibin + ietabin;
	  if (!((iphibin >= 0) && (iphibin < 60) && (index >= 0) && (index < 720))) {
	    continue;
	  }

#if 0
	  std::cout << "isector=" << isector 
		    << " isubsector=" << isubsector
		    << " ietabin=" << ietabin 
		    << " " << hits[ihit]->module() 
		    << " " << hits[ihit]->sub()
		    << " " << hits[ihit]->eta()
		    << " " << hits[ihit]->adc()
		    << std::endl;
#endif

	  /// add raw hit to our tower
	  mTowers[index][0].stemc( hits[ihit] );


	  /// add this hit to our list of hit towers,
	  /// calculate energy, etc...  Note that addTowerHit
	  /// must come after any operation on mTowers[0][ii]
	  /// for changes to tower to be propagated to the
	  /// array of hit towers.
	  Float_t energy = addTowerHit(isector,isubsector,ietabin,(Float_t)adc,0);
	  hits[ihit]->setEnergy(energy);
	}

    }

#if 0
  ///---------------<<<<<<<<<<<<< test >>>>>>>>>>>>------------------------
  for ( UInt_t ii=0; ii < mHitTowers.size(); ii++ ) 
    {
      std::cout << "mHitTowers[0][" << ii << "] " 
		<< mHitTowers[0][ii].sector() << " "
		<< mHitTowers[0][ii].subsector() << " "
		<< mHitTowers[0][ii].etabin() << " "
		<< mHitTowers[0][ii].adc() << " "
		<<std::endl << "    ====> "
		<< mHitTowers[0][ii].stemc()
		<< std::endl;
    }
  ///---
#endif

  ///
  /// Set StEmcRawHit pointers in StEEmcTower, pre + post
  ///

  detector=emc->detector(kEndcapEmcPreShowerId);

  if ( !detector )
  {
      Warning("fillFromSt","\n**********\nemc->detector() NULL for eemc pre/post.  MAJOR PROBLEM, trying to procede.\n**********\n\n");
  } else {
    for ( UInt_t sec = 0; sec < detector -> numberOfModules(); sec++ ) {

      /// Remember to watch out for fortran holdovers in
      /// the numbering scheme
      const StEmcModule *sector = detector -> module( sec+1 );
      const StSPtrVecEmcRawHit &hits = sector->hits();

      /// Loop over all raw hits in this sector
      for ( UInt_t ihit=0; ihit<hits.size(); ihit++ )
        {

	  if (!hits[ihit]) {
	    continue;
	  }

          /// This encoding is a bit obnoxious, but necessary
          /// to stuff eemc data into barrel structures. 
          Int_t isector    =  hits[ihit]->module() - 1;
          Int_t isubsector = (hits[ihit]->sub() - 1) % 5;
          Int_t ietabin    =  hits[ihit]->eta() - 1;
	  Int_t adc        =  hits[ihit]->adc();

          Int_t ilayer     = (hits[ihit]->sub() - 1) / 5 + 1;
          if (!((ilayer >= 1) && (ilayer < 4))) {
	    continue;
	  }

 	  Int_t iphibin = 5*isector+isubsector;
	  Int_t index = 12*iphibin + ietabin;

	  mTowers[index][ilayer].stemc( hits[ihit] );
	  
	  /// add this hit to our tower/layer.  See comments
	  /// in tower loop.
	  Float_t energy = addTowerHit( isector, isubsector, ietabin, (Float_t)adc, ilayer );
	  hits[ihit]->setEnergy(energy);
	}

    }
  }

  ///
  /// Set StEmcRawHit pointers in StEEmcStrip
  ///
  StDetectorId ids[] = { kEndcapSmdUStripId, kEndcapSmdVStripId };
  /// Loop over U & V planes
  for ( Int_t iplane=0; iplane<2; iplane++ )
    {

      /// Get the eemc smd collections
      detector=emc->detector( ids[iplane] );
      if ( !detector ) {
	  Warning("fillFromSt","\n**********\nemc->detector() NULL for esmd plane.  MAJOR PROBLEM, trying to procede.\n**********\n\n");
      } else {
	for ( UInt_t sec = 0; sec < detector -> numberOfModules(); sec++ )
	{
	  
	  /// Remember to watch out for fortran holdovers in
	  /// the numbering scheme
	  const StEmcModule *sector = detector -> module( sec+1 );
	  const StSPtrVecEmcRawHit &hits = sector->hits();
	  
	  /// Loop over all raw hits in this sector
	  for ( UInt_t ihit=0; ihit<hits.size(); ihit++ )
	    {
	      Int_t isector=hits[ihit]->module()-1;
	      Int_t istrip=hits[ihit]->eta()-1;
	      Int_t adc   =hits[ihit]->adc();
	      if (!((isector >= 0) && (isector < 12) && (istrip >= 0) && (istrip < 288))) {
		continue;
	      }
	      
	      mStrips[isector][iplane][istrip].stemc( hits[ihit] );

	      /// sett comments in tower loop.
	      Float_t energy = addSmdHit( isector, iplane, istrip, (Float_t)adc);
	      hits[ihit]->setEnergy(energy);
	    }

	}
      }
    }
  return true;
}

// ----------------------------------------------------------------------------
Float_t StEEmcA2EMaker::energy(Int_t layer) const
{
    Float_t sum=0.;
    for ( Int_t sector=0;sector<12;sector++ ) sum+= energy(sector,layer); 
    return sum;
} 

// ----------------------------------------------------------------------------
StEEmcTower *StEEmcA2EMaker::tower(TVector3 &r, Int_t layer) {
  Int_t sec=-1,sub=-1,eta=-1;
  if ( !mEEgeom->getTower(r,sec,sub,eta) ) return NULL;
  return &mTowers[ index(sec,sub,eta) ][layer];
}

// ----------------------------------------------------------------------------
const StEEmcTower *StEEmcA2EMaker::tower(TVector3 &r, Int_t layer) const {
  Int_t sec=-1,sub=-1,eta=-1;
  if ( !mEEgeom->getTower(r,sec,sub,eta) ) return NULL;
  return &mTowers[ index(sec,sub,eta) ][layer];
}
