/*
 * StEmcOfflineCalibrationMaker.cxx
 * Update Author: J. Kevin Adkins, University of Kentucky
 * June 17, 2014
 *
 * This was previously updated for the 2009 calibration. 
 * For 2012, I updated to use the new setting functions in 
 * the event class. Triggers were updated over the last calibration
 * to have indepenent triggers, similar to what's done in the jet 
 * trees. There are no methods for TOF triggers implemented as of
 * June 2014. This should be changed, and the TOF setting information
 * uncommented if information from TOF is desired.
 */

#include <math.h>
#include <vector>
#include <stdio.h>

#include "TFile.h"
#include "TTree.h"
#include "TH2.h"
#include "TChain.h"
#include "TCanvas.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

#include "StEvent/StTriggerId.h"
#include "StEvent/StEvent.h"
#include "StEventTypes.h"

#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StEvent/StL0Trigger.h"

#include "StDaqLib/TRG/trgStructures2005.h"
#include "StarClassLibrary/StPhysicalHelixD.hh"

//StEmc
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"

#include "StEvent/StBTofHeader.h"

//logger
#include "StMessMgr.h"

//my files
#include "StEmcOfflineCalibrationEvent.h"
#include "StEmcOfflineCalibrationMaker.h"

//use this to get kBarrelEmcTowerId defined
#include "StEnumerations.h"

using namespace std;

ClassImp(StEmcOfflineCalibrationMaker)

StEmcOfflineCalibrationMaker::StEmcOfflineCalibrationMaker(const char* name, const char* file)
{
  filename = file;
  muDstMaker = NULL;
  mEmcPosition = new StEmcPosition();
  
  towerSlopes = NULL;
  smdeSlopes=NULL;
  smdpSlopes=NULL;
  preshowerSlopes = NULL;
  mapcheck = NULL;
  htTriggers.clear();
}

StEmcOfflineCalibrationMaker::~StEmcOfflineCalibrationMaker() { }

Int_t StEmcOfflineCalibrationMaker::Init()
{
  myFile = new TFile(filename,"RECREATE");
  calibTree = new TTree("calibTree","BTOW calibration tree");
  myEvent = new StEmcOfflineCalibrationEvent();
  calibTree->Branch("event_branch","StEmcOfflineCalibrationEvent",&myEvent);
  calibTree->SetAutoSave(1000000000);
	
  muDstMaker = dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
  mADCtoEMaker = dynamic_cast<StEmcADCtoEMaker*>(GetMaker("Eread")); assert(mADCtoEMaker);
  emcTrigSimu = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu")); assert(emcTrigSimu);

  mTables = mADCtoEMaker->getBemcData()->getTables();
  mEmcGeom = StEmcGeom::instance("bemc");
  mSmdEGeom=StEmcGeom::instance("bsmde");
  mSmdPGeom=StEmcGeom::instance("bsmdp");

  return StMaker::Init();
}

Int_t StEmcOfflineCalibrationMaker::InitRun(Int_t run)
{
  mHT0threshold = emcTrigSimu->bemc->barrelHighTowerTh(0);
  mHT1threshold = emcTrigSimu->bemc->barrelHighTowerTh(1);
  mHT2threshold = emcTrigSimu->bemc->barrelHighTowerTh(2);
  mHT3threshold = emcTrigSimu->bemc->barrelHighTowerTh(3);
	
  LOG_INFO << "HT thresholds: " << mHT0threshold << " " << mHT1threshold << " " << mHT2threshold << " " << mHT3threshold << endm;

  //Initialize histograms after TFile initialization
  towerSlopes = new TH2F("towerSlopes","ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);  
  preshowerSlopes = new TH2F("preshowerSlopes","ADC vs. cap vs. towerID",4800,0.5,4800.5,500,-49.5,450.5);
  smdeSlopes=new TH2F("smdeSlopes","ADC vs. stripID",18000,0.5,18000.5,1223,-199.5,1023.5);  
  smdpSlopes=new TH2F("smdpSlopes","ADC vs. stripID",18000,0.5,18000.5,1223,-199.5,1023.5);  
  mapcheck = new TH2F("mapcheck","Track Projection vs EMC hit",4800,0.5,4800.5,4800,0.5,4800.5);

  //pedestals, rms, status
  Float_t pedestal, rms;
  Int_t status;
  Int_t pedstatus;
  for (Int_t id=1; id<=4800; id++){
    mTables->getPedestal(BTOW, id, 0, pedestal, rms);
    mTables->getStatus(BTOW, id, pedstatus, "pedestal");
    mTables->getStatus(BTOW, id, status);
    mPedestal[BTOW-1][id-1] = pedestal;
    mPedRMS[BTOW-1][id-1] = rms;
    mStatus[BTOW-1][id-1] = status*pedstatus;
		
    mTables->getPedestal(BPRS, id, 0, pedestal, rms);
    mTables->getStatus(BPRS, id, status);
    mPedestal[BPRS-1][id-1] = pedestal;
    mPedRMS[BPRS-1][id-1] = rms;
    mStatus[BPRS-1][id-1] = status;		
  }
	
  for(Int_t sid=1;sid<=18000;sid++){
    mTables->getPedestal(BSMDE, sid, 0, pedestal, rms);
    mTables->getStatus(BSMDE, sid, status);
    mPedestalSmd[BSMDE-3][sid-1] = pedestal;
    mPedRMSSmd[BSMDE-3][sid-1]	= rms;
    mStatusSmd[BSMDE-3][sid-1]	= status;
    
    mTables->getPedestal(BSMDP, sid, 0, pedestal, rms);
    mTables->getStatus(BSMDP, sid, status);
    mPedestalSmd[BSMDP-3][sid-1] = pedestal;
    mPedRMSSmd[BSMDP-3][sid-1]	= rms;
    mStatusSmd[BSMDP-3][sid-1]	= status;		
  }

  LOG_INFO << "StEmcOfflineCalibrationMaker::Init() == kStOk" << endm;
  return StMaker::InitRun(run);
}

Int_t StEmcOfflineCalibrationMaker::Make()
{		
  LOG_DEBUG<<"StEmcOfflineCalibrationMaker::Make()"<<endm;
  //get pointers to useful objects
  StMuDst* muDst = muDstMaker->muDst(); assert(muDst);
  StMuEvent* event = muDst->event(); assert(event);
  StRunInfo* runInfo = &(event->runInfo()); assert(runInfo);
	
  LOG_DEBUG << "General pointer assertions okay" << endm;

  // Store basic event parameters
  Bool_t fillQA = (runInfo->beamFillNumber(blue)==runInfo->beamFillNumber(yellow));
  if (fillQA)
    myEvent->setFill((Int_t)runInfo->beamFillNumber(blue));
  else
    myEvent->setFill(0);
  myEvent->setRunNum(event->runNumber());
  myEvent->setEventId(event->eventNumber());
  myEvent->setDate(GetDate());
  myEvent->setTime(GetTime());
  myEvent->setHighTowerTh(0, mHT0threshold);
  myEvent->setHighTowerTh(1, mHT1threshold);
  myEvent->setHighTowerTh(2, mHT2threshold);
  myEvent->setHighTowerTh(3, mHT3threshold);
			
  //fill ADC values from StEmcCollection obtained from MuDst
  mEmcCollection = mADCtoEMaker->getEmcCollection();
  for(Int_t p=0;p<2;p++)
    for(Int_t q=0;q<18000;q++) 
      mADCSmd[p][q]=0;

  getADCs(BTOW);
  getADCs(BPRS);
  getADCs(BSMDP);
  getADCs(BSMDE);
  for(Int_t id=1; id<=4800; id++){
    if((mADC[BTOW-1][id-1] != 0))
      towerSlopes->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]);
    if(mADC[BPRS-1][id-1] != 0 && mCapacitor[id-1]!= CAP1 && mCapacitor[id-1]!=CAP2)
      preshowerSlopes->Fill(id, mADC[BPRS-1][id-1]-mPedestal[BPRS-1][id-1]);
  }
  
  for(Int_t id=1; id<=18000; id++){
    if((mADCSmd[BSMDE-3][id-1] != 0) && mCapacitorSmd[BSMDE-3][id-1] != CAP1 && mCapacitorSmd[BSMDE-3][id-1] != CAP2)
      smdeSlopes->Fill(id,mADCSmd[BSMDE-3][id-1]);
    if(mADCSmd[BSMDP-3][id-1] != 0 && mCapacitorSmd[BSMDP-3][id-1] != CAP1 && mCapacitorSmd[BSMDP-3][id-1] != CAP2)
      smdpSlopes->Fill(id, mADCSmd[BSMDP-3][id-1]);
  }
  
  LOG_DEBUG << "Got ADCs" << endm;

  // Add triggers and set information for HT triggers
  for(UInt_t htTrig = 0; htTrig < htTriggers.size(); ++htTrig){
    Int_t trigId = htTriggers.at(htTrig);

    // Store HT trigger if it fire in hardware or software
    if (event->triggerIdCollection().nominal().isTrigger(trigId) || emcTrigSimu->isTrigger(trigId) > 0){
      StEmcOfflineCalibrationTrigger *myTrigger = myEvent->addTrigger();
      myTrigger->setTrigId(trigId);    

      // Hardware fire
      if (event->triggerIdCollection().nominal().isTrigger(trigId))
	myTrigger->setDidFire(true);
      else
	myTrigger->setDidFire(false);
      
      // Software fire
      if (emcTrigSimu->isTrigger(trigId) > 0)
	myTrigger->setShouldFire(true);
      else
	myTrigger->setShouldFire(false);
    }
    else
      continue;
  }

  const StEventSummary& evtSummary = muDstMaker->muDst()->event()->eventSummary();
  Double_t mField = evtSummary.magneticField()/10;

  //now for the tracks
  vector<Int_t> track_ids; 

  Int_t nPrimVerts = muDst->numberOfPrimaryVertices();
  Int_t vertIndex = 0;
  for(Int_t iVertex=0; iVertex < nPrimVerts; ++iVertex){
    if (iVertex > 9) continue; // Only store 10 vertices
    StMuPrimaryVertex *muVertex = muDst->primaryVertex(iVertex);
    assert(muVertex);
    muDst->setVertexIndex(iVertex);
    if (muVertex->ranking() < 0) continue;
    StEmcOfflineCalibrationVertex *myVertex = myEvent->addVertex();
    myVertex->setRanking(muVertex->ranking());
    myVertex->setX(muVertex->position().x());
    myVertex->setY(muVertex->position().y());
    myVertex->setZ(muVertex->position().z());
 
    const StMuTrack* track;
    const StMuTrack* primarytrack;

    Int_t nPrimTracks = muDst->numberOfPrimaryTracks();
    pair<UInt_t, pair<Float_t,Float_t> > smd_eta_center;
    pair<UInt_t, pair<Float_t,Float_t> > smd_phi_center;

    for(Int_t iTrack = 0; iTrack < nPrimTracks; ++iTrack){
      track = NULL;
      primarytrack = muDst->primaryTracks(iTrack);

      track = primarytrack->globalTrack();
      if(!track)track = primarytrack;

      Double_t p;
      StPhysicalHelixD outerhelix;
      pair<UInt_t, pair<Float_t,Float_t> > center_tower;
      outerhelix = track->outerHelix();
      p = outerhelix.momentum(mField*tesla).mag();
      center_tower = getTrackTower(track, false,1);
      smd_eta_center=getTrackTower(track,false,3);
      smd_phi_center=getTrackTower(track,false,4);
      
      //project track to BEMC      
      Int_t id = center_tower.first;
      Int_t exitid = (getTrackTower(track, true)).first;
      
      if (id == 0) continue; // Don't store tracks that had a bad projection on the EMC
      if (p < 1.) continue;

      StEmcOfflineCalibrationTrack *myTrack = myEvent->addTrack();
      myVertex->addTrack(myTrack);
      myTrack->setFlag(track->flag());
      myTrack->setBad(track->bad());
      myTrack->setCharge(track->charge());
      myTrack->setEta(track->eta());
      myTrack->setPhi(track->phi());
      myTrack->setVertexIndex(vertIndex);
      myTrack->setMomentum(outerhelix.momentum(mField*tesla));
      
      if (id == exitid)track_ids.push_back(id);
      Int_t etaid=smd_eta_center.first;
      Int_t phiid=smd_phi_center.first;
      LOG_DEBUG<<"using track projection to the SMD, we get strips eta: "<<etaid<<" and phi: "<<phiid<<endm;
      if(etaid>0){
	Int_t smdeids[11];
	smdeids[0] = etaid;
	smdeids[1] = mEmcPosition->getNextId(3,etaid,1,0);
	smdeids[2] = mEmcPosition->getNextId(3,etaid,2,0);
	smdeids[3] = mEmcPosition->getNextId(3,etaid,3,0);
	smdeids[4] = mEmcPosition->getNextId(3,etaid,4,0);
	smdeids[5] = mEmcPosition->getNextId(3,etaid,5,0);
	smdeids[6] = mEmcPosition->getNextId(3,etaid,-1,0);
	smdeids[7] = mEmcPosition->getNextId(3,etaid,-2,0);
	smdeids[8] = mEmcPosition->getNextId(3,etaid,-3,0);
	smdeids[9] = mEmcPosition->getNextId(3,etaid,-4,0);
	smdeids[10] = mEmcPosition->getNextId(3,etaid,-5,0);
	
	for(Int_t i = 0; i < 11; i++){
	  myTrack->setSmdeId(i,smdeids[i]);
	  myTrack->setSmdeAdc(i,mADCSmd[0][smdeids[i]-1]);
	  myTrack->setSmdeStatus(i,mStatusSmd[0][smdeids[i]-1]);
	  myTrack->setSmdePedestal(i,mPedestalSmd[0][smdeids[i]-1]);
	  myTrack->setSmdePedestalRms(i,mPedRMSSmd[0][smdeids[i]-1]);
	}
      }else{
	for(Int_t i=0;i<11;i++){
	  myTrack->setSmdeId(i,0);
	  myTrack->setSmdeAdc(i,0);
	  myTrack->setSmdeStatus(i,0);
	  myTrack->setSmdePedestal(i,0.);
	  myTrack->setSmdePedestalRms(i,0.);
	}
      }
      if(phiid>0){
	Int_t smdpids[11];
	smdpids[0] = phiid;
	smdpids[1] = mEmcPosition->getNextId(4,phiid,0,1);
	smdpids[2] = mEmcPosition->getNextId(4,phiid,0,2);
	smdpids[3] = mEmcPosition->getNextId(4,phiid,0,3);
	smdpids[4] = mEmcPosition->getNextId(4,phiid,0,4);
	smdpids[5] = mEmcPosition->getNextId(4,phiid,0,5);
	smdpids[6] = mEmcPosition->getNextId(4,phiid,0,-1);
	smdpids[7] = mEmcPosition->getNextId(4,phiid,0,-2);
	smdpids[8] = mEmcPosition->getNextId(4,phiid,0,-3);
	smdpids[9] = mEmcPosition->getNextId(4,phiid,0,-4);
	smdpids[10] = mEmcPosition->getNextId(4,phiid,0,-5);
	
	for(Int_t i = 0; i < 11; i++){
	  myTrack->setSmdpId(i,smdpids[i]);
	  myTrack->setSmdpAdc(i,mADCSmd[1][smdpids[i]-1]);
	  myTrack->setSmdpStatus(i,mStatusSmd[1][smdpids[i]-1]);
	  myTrack->setSmdpPedestal(i,mPedestalSmd[1][smdpids[i]-1]);
	  myTrack->setSmdpPedestalRms(i,mPedRMSSmd[1][smdpids[i]-1]);
	}
      }else{
	for(Int_t i=0;i<11;i++){
	  myTrack->setSmdpId(i,0);
	  myTrack->setSmdpAdc(i,0);
	  myTrack->setSmdpStatus(i,0);
	  myTrack->setSmdpPedestal(i,0.);
	  myTrack->setSmdpPedestalRms(i,0.);
	}
      }
      
      //find neighboring towers
      Int_t softid[9];
      softid[0] = id;
      softid[1] = mEmcPosition->getNextTowerId(id,-1,-1);
      softid[2] = mEmcPosition->getNextTowerId(id,0,-1);
      softid[3] = mEmcPosition->getNextTowerId(id,1,-1);
      softid[4] = mEmcPosition->getNextTowerId(id,-1,0);
      softid[5] = mEmcPosition->getNextTowerId(id,1,0);
      softid[6] = mEmcPosition->getNextTowerId(id,-1,1);
      softid[7] = mEmcPosition->getNextTowerId(id,0,1);
      softid[8] = mEmcPosition->getNextTowerId(id,1,1);								
      
      //save EMC info for 3x3 tower block around track
      for(Int_t tower = 0; tower < 9; ++tower){
	myTrack->setTowerId(tower, softid[tower]);
	if (tower > 0 && softid[tower] == 0){
	  myTrack->setTowerAdc(tower,0);
	  myTrack->setTowerStatus(tower,0);
	  myTrack->setTowerPedestal(tower,0.);
	  myTrack->setTowerPedestalRms(tower,0.);
	  myTrack->setPreshowerAdc(tower,0);
	  myTrack->setPreshowerStatus(tower,0);
	  myTrack->setPreshowerPedestal(tower,0.);
	  myTrack->setPreshowerPedestalRms(tower,0.);
	  continue;
	}
	myTrack->setTowerAdc(tower, mADC[BTOW-1][softid[tower]-1]);
	myTrack->setTowerPedestal(tower, mPedestal[BTOW-1][softid[tower]-1]);
	myTrack->setTowerPedestalRms(tower, mPedRMS[BTOW-1][softid[tower]-1]);
	myTrack->setTowerStatus(tower, mStatus[BTOW-1][softid[tower]-1]);
	myTrack->setPreshowerAdc(tower, mADC[BPRS-1][softid[tower]-1]);
	myTrack->setPreshowerPedestal(tower, mPedestal[BPRS-1][softid[tower]-1]);
	myTrack->setPreshowerPedestalRms(tower, mPedRMS[BPRS-1][softid[tower]-1]);
	myTrack->setPreshowerStatus(tower, mStatus[BPRS-1][softid[tower]-1]);
	myTrack->setPreshowerCap(tower, mCapacitor[softid[tower]-1]);
      }
				
      myTrack->setP(p);
      myTrack->setDeta(center_tower.second.first);
      myTrack->setDphi(center_tower.second.second);
      myTrack->setTowerExitId((getTrackTower(track, true)).first);
      myTrack->setHighestNeighbor(highestNeighbor(softid[0]));
				
      myTrack->setNSigmaElectron(track->nSigmaElectron());
      myTrack->setNSigmaPion(track->nSigmaPion());
      myTrack->setNSigmaProton(track->nSigmaProton());
      myTrack->setNSigmaKaon(track->nSigmaKaon());
	
      myTrack->setNHits(track->nHits());
      myTrack->setNFitPoints(track->nHitsFit());
      myTrack->setNDedxPoints(track->nHitsDedx());
      myTrack->setNHitsPossible(track->nHitsPoss());
      myTrack->setDeDx(track->dEdx());
	
      /*
      // store TOF information
	
      // save these for track QA cuts
      myTrack->setVpdVz((Float_t)(muDst->btofHeader()->vpdVz(0)));
      myTrack->setDcaGlobal((Float_t)(track->dcaGlobal().mag()));

      StMuBTofPidTraits tofTraits = track->btofPidTraits();
      myTrack->SetTofMatchedFlag((Int_t)tofTraits.matchFlag());
      myTrack->setTofTime(tofTraits.timeOfFlight());// some tof hits pass matchflag  = 1 with -999 time of flight value
      myTrack->setTofBeta(tofTraits.beta());// some tof hits pass matchflag  = 1 with -999 beta value
      myTrack->setTofPathlength(tofTraits.pathLength());
      myTrack->setTofSigmaElectron(tofTraits.sigmaElectron());
      myTrack->setTofProbElectron(tofTraits.probElectron());
      */
    }
    vertIndex++;
  }
  
  for(Int_t i = 0; i < 4800; i++){
    if((mADC[BTOW-1][i]-mPedestal[BTOW-1][i]) < 5 || (mADC[BTOW-1][i]-mPedestal[BTOW-1][i]) > 100)continue;
    for(UInt_t j = 0; j < track_ids.size(); j++){
      mapcheck->Fill(track_ids[j],i+1);
    }
  }
  
 calibTree->Fill();
 myEvent->Clear();
 LOG_DEBUG << "StEmcOfflineCalibrationMaker::Make() == kStOk" << endm;
 return kStOK;
}

void StEmcOfflineCalibrationMaker::Clear(Option_t* option)
{	
  if(myEvent)
    myEvent->Clear();
  for(Int_t i=0; i<2; i++){
    for(Int_t j=0; j<4800; j++){
      mADC[i][j] = 0;
    }
  }
  for(Int_t i=0; i<2; i++){
    for(Int_t j=0; j<18000; j++){
      mADCSmd[i][j] = 0;
    }
  }
}

Int_t StEmcOfflineCalibrationMaker::Finish()
{
  myFile->Write();
  myFile->Close();
  delete myFile;

  return kStOk;
}

void StEmcOfflineCalibrationMaker::getADCs(Int_t det) //1==BTOW, 2==BPRS, 3=BSMDE, 4=BSMDP
{
  det--;
  StDetectorId detectorid = static_cast<StDetectorId>(kBarrelEmcTowerId + det);
  StEmcDetector* detector=mEmcCollection->detector(detectorid);
  if(detector){
    for(Int_t m=1;m<=120;m++){
      StEmcModule* module = detector->module(m);
      if(module){
	StSPtrVecEmcRawHit& rawHit=module->hits();
	for(UInt_t k=0;k<rawHit.size();k++){
	  if(rawHit[k]){
	    Int_t module = rawHit[k]->module();
	    Int_t eta = rawHit[k]->eta(); 
	    Int_t submodule = TMath::Abs(rawHit[k]->sub());
	    Int_t ADC = rawHit[k]->adc();
	    Int_t hitid;
	    Int_t stat=-9;	      
	    if(det<2) stat = mEmcGeom->getId(module,eta,submodule,hitid);
	    else if(det==2) stat=mSmdEGeom->getId(module,eta,submodule,hitid);
	    else if(det==3) stat=mSmdPGeom->getId(module,eta,submodule,hitid);
	    if(det == 0 && stat == 0){
	      Int_t adc6 = emcTrigSimu->bemc->barrelHighTowerAdc(hitid);
	      myEvent->setHighTowerAdc(hitid,adc6);
	    }
	    if(stat==0){
	      if(det<2) mADC[det][hitid-1] = ADC;
	      else{
		mADCSmd[det-2][hitid-1]=ADC;
	      }
	    }
	    if(det==1){
	      UChar_t CAP =  rawHit[k]->calibrationType();
	      if(CAP > 127) CAP -= 128;
	      mCapacitor[hitid-1] = CAP;
	    }
	    if(det>1){
	      UChar_t CAP=rawHit[k]->calibrationType();
	      if(CAP>127) CAP-=128;
	      mCapacitorSmd[det-2][hitid-1]=CAP;
	    }
	  }
	}
      }
      else { LOG_WARN<<"couldn't find StEmcModule "<<m<<" for detector "<<det<<endm; }
    }
  }
  else { LOG_WARN<<"couldn't find StEmcDetector for detector "<<det<<endm; }
}

pair<Int_t, pair<Float_t,Float_t> > StEmcOfflineCalibrationMaker::getTrackTower(const StMuTrack* track, Bool_t  useExitRadius, Int_t det){ //1=BTOW, 3=BSMDE, 4=BSMDP
  pair<Int_t, pair<Float_t,Float_t> > tower;
  tower.first = 0;
  tower.second.first = 1000.;
  tower.second.second = 1000.;  

  StThreeVectorD momentum,position;
  Double_t radius;
  if(det==1) radius = mEmcGeom->Radius();
  if(det==2) radius = mEmcGeom->Radius();
  if(det==3) radius = mSmdEGeom->Radius();
  if(det==4) radius = mSmdPGeom->Radius();

  const StEventSummary& evtSummary = muDstMaker->muDst()->event()->eventSummary();
  Double_t mField = evtSummary.magneticField()/10;
  
  //add 30 cm to radius to find out if track left same tower
  if(useExitRadius) radius += 30.0;

  bool goodProjection = mEmcPosition->trackOnEmc(&position,&momentum,track,mField,radius);
  if(goodProjection){
    Int_t m,e,s,id=0;
    Float_t eta=position.pseudoRapidity();
    Float_t phi=position.phi();
    if(det==1){
      mEmcGeom->getBin(phi,eta,m,e,s);
      s = abs(s);
      
      if(mEmcGeom->getId(m,e,s,id)==0){
	tower.first = id;
	tower.second = getTrackDetaDphi(eta, phi, id, det);
      }
    }
    else if(det==3){
      Int_t check=mSmdEGeom->getBin(phi,eta,m,e,s);
      if(!check){
	s = abs(s);
	if(mSmdEGeom->getId(m,e,s,id)==0){
	  tower.first = id;
	  tower.second = getTrackDetaDphi(eta, phi, id, det);
	}
      }
    }
    else if(det==4){
      Int_t check=mSmdPGeom->getBin(phi,eta,m,e,s);
      s = abs(s);
      if(!check){
	if(mSmdPGeom->getId(m,e,s,id)==0){
	  tower.first = id;
	  tower.second = getTrackDetaDphi(eta, phi, id, det);
	}
      }
    }
  }
  
  return tower;
}

pair<Float_t,Float_t> StEmcOfflineCalibrationMaker::getTrackDetaDphi(Float_t track_eta, Float_t track_phi, Int_t id, Int_t det){
  Float_t eta_tower, phi_tower;
  eta_tower = phi_tower = -999.;
  pair<Float_t, Float_t> dtow;
  if(det==1){
    mEmcGeom->getEtaPhi(id,eta_tower,phi_tower);
    
    //now calculate distance from center of tower:
    Float_t dphi = phi_tower - track_phi;
    while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
    while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}
    
    dtow.first = eta_tower - track_eta;
    dtow.second = dphi;
  }
  if(det==3){
    mSmdEGeom->getEtaPhi(id,eta_tower,phi_tower);
    
    //now calculate distance from center of strip:
    Float_t dphi = phi_tower - track_phi;
    while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
    while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}
    
    dtow.first = eta_tower - track_eta;
    dtow.second = dphi;
  }
  if(det==4){
    mSmdPGeom->getEtaPhi(id,eta_tower,phi_tower);
    
    //now calculate distance from center of strip:
    Float_t dphi = phi_tower - track_phi;
    while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
    while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}
    
    dtow.first = eta_tower - track_eta;
    dtow.second = dphi;
  }  
  return dtow;
}

Double_t StEmcOfflineCalibrationMaker::highestNeighbor(Int_t id){
  Double_t nSigma=0.;
  
  for(Int_t deta=-1; deta<=1; deta++){
    for(Int_t dphi=-1; dphi<=1; dphi++){
      if(deta==0 && dphi==0) continue;
      Int_t nextId = mEmcPosition->getNextTowerId(id,deta,dphi);
      if(nextId<1 || nextId>4800) continue;
      if((mADC[BTOW-1][nextId-1]-mPedestal[BTOW-1][nextId-1]) > nSigma*mPedRMS[BTOW-1][nextId-1]){
	nSigma = (mADC[BTOW-1][nextId-1] - mPedestal[BTOW-1][nextId-1])/mPedRMS[BTOW-1][nextId-1];
      }
    }
  }  
  return nSigma;
}

void StEmcOfflineCalibrationMaker::addHighTowerTrigger(UInt_t trigId){ 
  htTriggers.push_back(trigId);
}
