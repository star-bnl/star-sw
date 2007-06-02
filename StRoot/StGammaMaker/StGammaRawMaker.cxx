#include "StGammaRawMaker.h"

#include "StMessMgr.h"

#include "StEEmcPool/StEEmcA2EMaker/StEEmcA2EMaker.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomDefs.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

#include "StEventTypes.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/StEmcRawMaker.h"

#include "StGammaEvent.h"
#include "StGammaEventMaker.h"

#define __GLOBAL_TRACKS__
#define __HACK_VERTEX__

ClassImp(StGammaRawMaker);

// ----------------------------------------------------------------------------
// constructor
StGammaRawMaker::StGammaRawMaker( const Char_t *fname ):StMaker(fname)
{
  Clear();
  SetTowerCutoff(0.1);
  SetTrackCutoff(0.1);
}

void StGammaRawMaker::SetTowerCutoff(Float_t t){ mTowerCutoff=t; }
void StGammaRawMaker::SetTrackCutoff(Float_t t){ mTrackCutoff=t; }


// ----------------------------------------------------------------------------
// initialization
Int_t StGammaRawMaker::Init()
{
  mEEmcGeometry=new EEmcGeomSimple();
  tables=new StBemcTables();
  mCorrupt=false;

  return StMaker::Init();
}


// ----------------------------------------------------------------------------
// event processing
Int_t StGammaRawMaker::Make()
{

  StMuDstMaker *mumk=(StMuDstMaker*)GetMaker("MuDst");
  if ( !mumk )
    {
      LOG_WARN<<"No MuDst, you fool!"<<endm;
      return kStWarn;
    }

#ifndef __HACK_VERTEX__
  StMuPrimaryVertex *pv=mumk->muDst()->primaryVertex();
  if ( !pv ) 
    {
      return kStOK;
    }
#endif

  GetTracks();

  GetBarrel();

  GetEndcap();

  return kStOk;
}
// ----------------------------------------------------------------------------
// clear for next event
void StGammaRawMaker::Clear(Option_t *opts)
{
  mTracks.clear();
  mTowers.clear();
  mStrips.clear();
  mPreshower1.clear();
  mPreshower2.clear();
  mPostshower.clear();

  // for efficiency, we could clear only those elements which have been "hit"
  for ( Int_t index=0;index<kEEmcNumSectors*kEEmcNumSubSectors*kEEmcNumEtas;index++ )
    for ( Int_t layer=0;layer<4;layer++ )
      mEEtowers[index][layer]=0;

  for ( Int_t sec=0;sec<kEEmcNumSectors;sec++ )
    for ( Int_t plane=0;plane<kEEmcNumSmdUVs;plane++ )
      for ( Int_t strip=0;strip<kEEmcNumStrips;strip++ )
	mEEstrips[sec][plane][strip]=0;

  // clear BTOW and BPRS arrays of pointers
  memset(mBarrelEmcTower, 0, sizeof(mBarrelEmcTower));
  memset(mBarrelEmcPreshower, 0, sizeof(mBarrelEmcPreshower));

  // clear BSMDE and BSMDP maps
  mBarrelSmdEtaStrip.clear();
  mBarrelSmdPhiStrip.clear();
}


// ----------------------------------------------------------------------------
// copy barrel towers and strips
void StGammaRawMaker::GetBarrel(){


  tables->loadTables(this);

  // get MuDst maker
  StMuDstMaker *mMuDstMaker=(StMuDstMaker*)GetMaker("MuDst");
  if ( !mMuDstMaker ) { 
    return;
  }


  // get gamma event
  StGammaEvent *gevent = 0;
  if ( !GetMaker("gemaker") ) 
    {
      LOG_DEBUG<<" StGammaEventMaker not in chain, no tree for you"<<endm;
    }
  else 
    {
      StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMaker("gemaker");
      gevent = gemaker -> event();
    }


  
  StMuDst* uDst = mMuDstMaker->muDst();
  assert(uDst);
  
  StEmcGeom* geom = StEmcGeom::instance("bemc"); 
  assert(geom);
  
  StEmcCollection* emc = 0;
  StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (event) {
    emc = event->emcCollection();
  }
  else {
    emc = uDst->emcCollection();
  }
  assert(emc);

  StEmcDetector* btow = emc->detector(kBarrelEmcTowerId);  //TOWERs
  StEmcDetector* bprs = emc->detector(StDetectorId(kBarrelEmcTowerId+1));  //BPRS
  StEmcDetector* smde = emc->detector(StDetectorId(kBarrelEmcTowerId+2));  //SMDe
  StEmcDetector* smdp = emc->detector(StDetectorId(kBarrelEmcTowerId+3));   //SMDp
  
  //corruption checking
  mCorrupt = false;

  //if detector==null, this means it's corrupt for pre-October 2004 BEMC code.  
  //However, not all corrupt events give detector==0
  if (!btow) {
    mCorrupt=true;
    return;
  }
    
  //now, check for corruption in post-October 2004 BEMC code (P04k and later)
  //crate corruption key: crateUnknown=0, crateNotPresent=1, crateOK=2, crateHeaderCorrupt=3"
  for(int crate = 1; crate<=MAXCRATES; crate++) {
    StEmcCrateStatus crateStatus = btow->crateStatus(crate);
    if (crateStatus==crateHeaderCorrupt) {
      mCorrupt=true;
      return;
    }
  }
  
  //And now we can implement Alex's new StEmcAdc2EMaker test
  StEmcADCtoEMaker* adc2e = (StEmcADCtoEMaker*)GetMaker("Eread");
  if (!adc2e) {
    cout <<"adc2e in chain"<<endl;
  }
  else {
    mCorrupt = adc2e->isCorrupted();
    if (mCorrupt==true) {
      return;
    }
  }
        
 
  //  StGammaTower btower, pshower;  
  //  StGammaStrip bstrip_p, bstrip_e;
  float pedestal, rms, bEta, bPhi;
  int CAP=0; //this arument matters only for SMD


  //loop over towers and pre-shower
  for(int m = 1; m<=120;m++) { 
    
    if (btow){

      StEmcModule* module = btow->module(m);
      assert(module);
      
      StSPtrVecEmcRawHit& rawHits = module->hits();
      
      for(UInt_t k=0;k<rawHits.size();k++) { //loop on hits in modules
	
	StEmcRawHit* tempRawHit = rawHits[k];
	
	//int m = tempRawHit->module();
	//int e = tempRawHit->eta();
	//int s = abs(tempRawHit->sub());
	int id, status;
	int ADC = tempRawHit->adc(); //not pedestal subtracted!
	double energy = tempRawHit->energy();
	id = tempRawHit->softId(BTOW);
	//geom->getId(m,e,s,id); 
	geom->getEtaPhi(id,bEta,bPhi);
	tables->getStatus(BTOW, id, status);
	tables->getPedestal(BTOW, id, CAP, pedestal, rms);

	// adc cut first 
	double pADC = ADC-pedestal;
	if ( pADC < 2.0 * rms ) continue;	

	// min pT cut next
	if ( energy / TMath::CosH(bEta) < mTowerCutoff ) continue;
	
	StGammaTower *btower = gevent->newTower();

	btower->id     = id;
	btower->layer  = kBEmcTower;
	btower->stat   = status;
	btower->fail   = !status;
	btower->energy = energy;
	btower->eta    = bEta;
	btower->phi    = bPhi;

	LOG_DEBUG <<" tower id="<<id<<" status = "<<status<<" energy="<<energy<<" eta="<<bEta<<" phi="<<bPhi<<" ADC="<<ADC<<endm;

	mTowers.push_back(*btower);
	mBarrelEmcTower[btower->id] = btower;

      }
    }


    
    if (bprs){
      
      StEmcModule*  module = bprs->module(m);
      assert(module);
      StSPtrVecEmcRawHit& rawHits=module->hits();
      //loop over our preshower
      for(UInt_t k=0;k<rawHits.size();k++) { //loop on hits in modules
	
	StEmcRawHit* tempRawHit = rawHits[k];
	assert(tempRawHit);
	
	//int m = tempRawHit->module();
	//int e = tempRawHit->eta();
	//int s = abs(tempRawHit->sub());
	int id, bprs_status;
	int ADC = tempRawHit->adc(); //not pedestal subtracted!
	double energy = tempRawHit->energy();
	id = tempRawHit->softId(BPRS);
	// geom->getId(m,e,s,id);
	geom->getEtaPhi(id,bEta,bPhi);
	tables->getStatus(BPRS, id, bprs_status);
	tables->getPedestal(BPRS, id, CAP, pedestal, rms);

	double pADC = ADC-pedestal;
	if ( pADC < 2.0 * rms ) continue;
	
	StGammaTower *pshower = gevent->newPreshower1();

	pshower->id     = id;
	pshower->layer  = kBEmcPres;
	pshower->stat   = bprs_status;
	pshower->fail   = !(bprs_status);
	pshower->energy = energy;
	pshower->eta    = bEta;
	pshower->phi    = bPhi;

	LOG_DEBUG<<" bprs id="<<id<<" status = "<<bprs_status<<" energy="<<energy<<" eta="<<bEta<<" phi="<<bPhi<<" ADC="<<ADC<<endm;

	mPreshower1.push_back(*pshower);
	mBarrelEmcPreshower[pshower->id] = pshower;

      }
    }  
  }// loop over modules
  

  if (smde) {// loop over SMDe
    
    for (UInt_t i = 1; i <= smde->numberOfModules(); i++) {
      StEmcModule* module = smde->module(i);
      if (module) {
	StSPtrVecEmcRawHit& hits = module->hits();
	for (StEmcRawHitIterator hit = hits.begin(); hit!=hits.end(); ++hit) {
	  Int_t smde_id = 0;
	  Int_t smde_status=0;
	  StEmcGeom* geom = StEmcGeom::instance("bsmde");
	  geom->getId((*hit)->module(),(*hit)->eta(),TMath::Abs((*hit)->sub()),smde_id); 
	  tables->getStatus(BSMDE,smde_id, smde_status);
	
	  StGammaStrip *bstrip = gevent->newStrip();
  
	  bstrip->index = smde_id;
	  bstrip->sector = (*hit)->module();
	  bstrip->plane  = kBEmcSmdEta;
	  bstrip->stat   = smde_status;
	  bstrip->fail   = !(smde_status);
	  bstrip->energy = (*hit)->energy();
	  
	  LOG_DEBUG<<" estrip id="<<smde_id<<" status = "<<smde_status<<" energy="<<(*hit)->energy()<<" module="<<(*hit)->module()<<endm;
	  	  
	  mStrips.push_back(*bstrip);
	  mBarrelSmdEtaStrip[bstrip->index] = bstrip;
	}
      }
    }
  }

  if (smdp) {// loop over SMDp
    
    for (UInt_t i = 1; i <= smdp->numberOfModules(); i++) {
      StEmcModule* module = smdp->module(i);
      if (module) {
	StSPtrVecEmcRawHit& hits = module->hits();
	for (StEmcRawHitIterator hit = hits.begin(); hit!=hits.end(); ++hit) {
	  Int_t smdp_id = 0;
	  Int_t smdp_status=0;
	  StEmcGeom* geom = StEmcGeom::instance("bsmdp");
	  geom->getId((*hit)->module(),(*hit)->eta(),TMath::Abs((*hit)->sub()),smdp_id);
	  tables->getStatus(BSMDP,smdp_id, smdp_status);
	  
	  StGammaStrip *bstrip = gevent->newStrip();

	  bstrip->index = smdp_id;
	  bstrip->sector = (*hit)->module();
	  bstrip->plane  = kBEmcSmdPhi;
	  bstrip->stat   = smdp_status;
	  bstrip->fail   = !(smdp_status);
	  bstrip->energy = (*hit)->energy(); 

	  LOG_DEBUG<<" pstrip id="<<smdp_id<<" status = "<<smdp_status<<" energy="<<(*hit)->energy()<<" module="<<(*hit)->module()<<endm;

	  mStrips.push_back(*bstrip);
	  mBarrelSmdPhiStrip[bstrip->index] = bstrip;
 	}
      }
    }
  } 	
}

// ----------------------------------------------------------------------------
// copy tracks
void StGammaRawMaker::GetTracks(){ 

  if (!GetDataSet("MuDst")) {
    return;
  }

  // get gamma event
  StGammaEvent *gevent = 0;
  if ( !GetMaker("gemaker") ) 
    {
      LOG_DEBUG<<" StGammaEventMaker not in chain, no tree for you"<<endm;
    }
  else 
    {
      StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMaker("gemaker");
      gevent = gemaker -> event();
    }


#ifdef __GLOBAL_TRACKS__
  TIter next(StMuDst::globalTracks()); // do we want global tracks here?????
#else
  TIter next(StMuDst::primaryTracks());
#endif
  while (StMuTrack* track = (StMuTrack*)next()) {
    if (Accept(track)) {
      StGammaTrack *gtrack = gevent->newTrack(track);
      mTracks.push_back( *gtrack );
    }
  }

}

// ----------------------------------------------------------------------------
// copy endcap towers/strips into gamma towers/strips
void StGammaRawMaker::GetEndcap()
{

  StMuDstMaker *mumk=(StMuDstMaker*)GetMaker("MuDst");
  if ( !mumk )
    {
      return;
    }

#ifndef __HACK_VERTEX__
  StMuPrimaryVertex *pv=mumk->muDst()->primaryVertex();
  if ( !pv ) 
    {
      return; // we're going to need tracking, therefore vertex
    }
  Float_t zvertex = pv->position().z();
#endif
#ifdef __HACK_VERTEX__
  Float_t zvertex = 0.0;
#endif

  StEEmcA2EMaker *adc2e=(StEEmcA2EMaker*)GetMaker("mEEanalysis");
  if ( !adc2e )
    {
      LOG_WARN<<"StEEmcA2EMaker not found, no endcap for you"<<endm;
      return;
    }

  // get gamma event
  StGammaEvent *gevent = 0;
  if ( !GetMaker("gemaker") ) 
    {
      LOG_DEBUG<<" StGammaEventMaker not in chain, no tree for you"<<endm;
    }
  else 
    {
      StGammaEventMaker *gemaker = (StGammaEventMaker*)GetMaker("gemaker");
      gevent = gemaker -> event();
    }



  // define the momentum of the tower as pointing from the
  // vertex to the following depths
  const Float_t depths[]={
    kEEmcZSMD,   // towers go to SMD depth
    kEEmcZPRE1,  // pre/post go to their own depth
    kEEmcZPRE2,
    kEEmcZPOST
  };

  Int_t enumerations[]={kEEmcTower,kEEmcPre1, kEEmcPre2, kEEmcPost };

  for ( Int_t layer=0;layer<4;layer++ )
    {
      for ( Int_t ihit=0;ihit<adc2e->numberOfHitTowers(layer);ihit++ )
	{
	  StEEmcTower tower = adc2e->hittower(ihit,layer);	  

	  UInt_t sec=(UInt_t)tower.sector();
	  UInt_t sub=(UInt_t)tower.subsector();
	  UInt_t eta=(UInt_t)tower.etabin();

	  // center of tower at specified depth
	  TVector3 center=mEEmcGeometry->getTowerCenter(sec,sub,eta);
	  Float_t  depth=depths[layer];
	  center.SetMag( depth/center.CosTheta() );

	  Float_t R        = center.Perp();
	  Float_t Z        = depth-zvertex;
	  Float_t TanTheta = R/Z;
	  Float_t Phi      = center.Phi();

	  TVector3 Momentum;
	  Momentum.SetMagThetaPhi( tower.energy(),
				   TMath::ATan( TanTheta ),
				   Phi );

	  StGammaTower  *gtower;

	  if ( layer == 0 ) 
	    gtower = gevent->newTower();
	  else if ( layer == 1 )
	    gtower = gevent->newPreshower1();
	  else if ( layer == 2 )
	    gtower = gevent->newPreshower2();
	  else if ( layer == 3 )
	    gtower = gevent->newPostshower();


	  gtower->id     = tower.index();
	  gtower->layer  = enumerations[ tower.layer() ];
	  gtower->stat   = tower.stat();
	  gtower->fail   = tower.fail();
	  gtower->energy = tower.energy();
	  gtower->eta    = Momentum.Eta();
	  gtower->phi    = Momentum.Phi();

	  // add tower to the list of towers
	  if ( Accept(*gtower) ) 
	    {
	      if ( layer==0 ) {
		if ( gtower->energy / TMath::CosH(gtower->eta) > mTowerCutoff )
		  {
		    mTowers.push_back(*gtower);
		    mEEtowers[ gtower->id ][ gtower->layer ] = gtower;
		  }
	      }
	      else if ( layer==1 ) 
		{
		  mPreshower1.push_back(*gtower);
		  mEEtowers[ gtower->id ][ gtower->layer ] = gtower;
		}
	      else if ( layer==2 )
		{
		  mPreshower2.push_back(*gtower);
		  mEEtowers[ gtower->id ][ gtower->layer ] = gtower;
		}
	      else if ( layer==3 )
		{
		  mPostshower.push_back(*gtower);
		  mEEtowers[ gtower->id ][ gtower->layer ] = gtower;
		}
	      else 
		continue; // yikes?
	    }




	}
    }


  Int_t smdenum[]={kEEmcSmdu, kEEmcSmdv };

  for ( Int_t sector=0;sector<12;sector++ )
    for ( Int_t plane=0;plane<2;plane++ )
      {
	for ( Int_t ihit=0;ihit<adc2e->numberOfHitStrips(sector,plane);ihit++ )
	  {
	    StEEmcStrip strip=adc2e->hitstrip(sector,plane,ihit);

	    StGammaStrip *gstrip = gevent->newStrip();
	    gstrip->index = strip.index();
	    gstrip->sector = strip.sector();
	    gstrip->plane  = smdenum[ strip.plane() ];
	    gstrip->stat   = strip.stat();
	    gstrip->fail   = strip.fail();
	    gstrip->energy = strip.energy(); /// <<<< How do we make this consistent with the barrel?  this is energy deposit in MeV, barrel is ~EM-energy of photon...

	    mEEstrips[ gstrip->sector ][ gstrip->plane ][ gstrip->index ]=gstrip;

	    if ( Accept(*gstrip) )
	      {
		mStrips.push_back(*gstrip);
	      }

	  }
      }
  
}


// ----------------------------------------------------------------------------




// ----------------------------------------------------------------------------
Bool_t StGammaRawMaker::Accept( StGammaTrack &track )
{
  return true; // save all tracks to tree
}


// ----------------------------------------------------------------------------
Bool_t StGammaRawMaker::Accept( StGammaTower &tower )
{

  if ( tower.fail ) return false;

  return true; // save all towers to tree
}

// ----------------------------------------------------------------------------
Bool_t StGammaRawMaker::Accept( StGammaStrip &strip )
{

  if ( strip.fail ) return false;

  return true; // for now save everything.  
}




// ----------------------------------------------------------------------------
Bool_t StGammaRawMaker::Accept( StMuTrack* track )
{
  return (track->flag() > 0        && // Good fit
          track->flag() / 100 != 7 && // FTPC only
          track->flag() / 100 != 8 && // FTPC+primary vertex
          track->flag() / 100 != 9 && // Beam background
          track->nHitsFit() > 5 );
}


// ----------------------------------------------------------------------------
StGammaTower *StGammaRawMaker::tower( Int_t id, Int_t layer )
{
  if ( layer >= kEEmcTower && layer <= kEEmcPost )
    {
      return mEEtowers[id][layer];
    }
  else if (layer == kBEmcTower)
    {
      return mBarrelEmcTower[id];
    }
  else if (layer == kBEmcPres)
    {
      return mBarrelEmcPreshower[id];
    }
  return 0;
};

StGammaStrip *StGammaRawMaker::strip( Int_t sec, Int_t plane, Int_t index )
{
  if ( plane == kEEmcSmdu || plane == kEEmcSmdv )
    {
      return mEEstrips[sec][plane][index];
    }
  else if (plane == kBEmcSmdEta)
    {
      return mBarrelSmdEtaStrip[index];
    }
  else if (plane == kBEmcSmdPhi)
    {
      return mBarrelSmdPhiStrip[index];
    }
  return 0;
};
