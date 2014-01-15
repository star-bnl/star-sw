//StEmcOfflineCalibrationMaker.cxx

#include <math.h>
#include <vector>
//#include <algorithm>
#include <stdio.h>

#include "TFile.h"
#include "TTree.h"
#include "TH2.h"
#include "TH3.h"
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
	
  towerSlopes[0] = NULL;
  towerSlopes[1] = NULL;
  smdSlopes[0]=NULL;
  smdSlopes[1]=NULL;
  preshowerSlopes = NULL;
  mapcheck = NULL;
  mbTriggers.clear();
  htTriggers.clear();
}

StEmcOfflineCalibrationMaker::~StEmcOfflineCalibrationMaker() { }

Int_t StEmcOfflineCalibrationMaker::Init()
{
  TString mfname;
  mfname += filename;
  mfname.ReplaceAll(".calib.",".map.");
  mapFile = new TFile(mfname.Data(),"RECREATE");
  myFile = new TFile(filename,"RECREATE");
  calibTree = new TTree("calibTree","BTOW calibration tree");
  myEvent = new StEmcOfflineCalibrationEvent();
  myTrack = new StEmcOfflineCalibrationTrack();
  calibTree->Branch("event_branch","StEmcOfflineCalibrationEvent",&myEvent);
  calibTree->SetAutoSave(1000000000);
	
  muDstMaker = dynamic_cast<StMuDstMaker*>(GetMaker("MuDst")); assert(muDstMaker);
  mADCtoEMaker = dynamic_cast<StEmcADCtoEMaker*>(GetMaker("Eread")); assert(mADCtoEMaker);
  //emcTrigMaker = dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger")); assert(emcTrigMaker);
  emcTrigMaker = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu")); assert(emcTrigMaker);
  emcTrigMaker->useOnlineDB();
  LOG_INFO << "Using online DB..." << endm;

  mTables = mADCtoEMaker->getBemcData()->getTables();
  mEmcGeom = StEmcGeom::instance("bemc");
  mSmdEGeom=StEmcGeom::instance("bsmde");
  mSmdPGeom=StEmcGeom::instance("bsmdp");

  LOG_INFO << "StEmcOfflineCalibrationMaker::Init() == kStOk" << endm;
  return StMaker::Init();
}

Int_t StEmcOfflineCalibrationMaker::InitRun(int run)
{
  LOG_DEBUG<<"StEmcOfflineCalibrationMaker::InitRun"<<endm;
	//switch histograms
  if(towerSlopes[0]){
    myFile->cd();
    towerSlopes[0]->Write();
    towerSlopes[1]->Write();
    preshowerSlopes->Write();
    smdSlopes[0]->Write();
    smdSlopes[1]->Write();
    mapcheck->Write();
  }

  mHT0threshold = emcTrigMaker->bemc->barrelHighTowerTh(0);
  mHT1threshold = emcTrigMaker->bemc->barrelHighTowerTh(1);
  mHT2threshold = emcTrigMaker->bemc->barrelHighTowerTh(2);
  mHT3threshold = emcTrigMaker->bemc->barrelHighTowerTh(3);
	
  LOG_INFO << "HT thresholds: " << mHT0threshold << " " << mHT1threshold << " " << mHT2threshold << " " << mHT3threshold << endm;

	//look for histograms from this run in the current file and switch to them if found, otherwise create them
  char name[200];
  sprintf(name,"towerSlopes_R%i",run);
  towerSlopes[0] = NULL;
  myFile->GetObject(name,towerSlopes[0]);
  if(towerSlopes[0]){
    sprintf(name,"towerSlopes_HT_R%i",run);
    myFile->GetObject(name,towerSlopes[1]);
		
    sprintf(name,"preshowerSlopes_R%i",run);
    myFile->GetObject(name,preshowerSlopes);

    sprintf(name,"smdSlopesE_R%i",run);
    myFile->GetObject(name,smdSlopes[0]);

    sprintf(name,"smdSlopesP_R%i",run);
    myFile->GetObject(name,smdSlopes[1]);
  }else{
    towerSlopes[0] = new TH2F(name,"ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);
	       
    sprintf(name,"towerSlopes_HT_R%i",run);
    towerSlopes[1] = new TH2F(name,"ADC vs. towerID",4800,0.5,4800.5,250,-49.5,200.5);
		
    sprintf(name,"preshowerSlopes_R%i",run);		
    preshowerSlopes = new TH2F(name,"ADC vs. cap vs. towerID",4800,0.5,4800.5,500,-49.5,450.5);

    sprintf(name,"smdSlopesE_R%i",run);
    smdSlopes[0]=new TH2F(name,"ADC vs. stripID",18000,0.5,18000.5,1223,-199.5,1023.5);

    sprintf(name,"smdSlopesP_R%i",run);
    smdSlopes[1]=new TH2F(name,"ADC vs. stripID",18000,0.5,18000.5,1223,-199.5,1023.5);
  }
  if(!mapcheck){
    mapcheck = new TH2F("mapcheck","Track Projection vs EMC hit",4800,0.5,4800.5,4800,0.5,4800.5);
  }	
	//pedestals, rms, status


  float pedestal, rms;
  int status;
  int pedstatus;
  for (int id=1; id<=4800; id++){

    mTables->getPedestal(BTOW, id, 0, pedestal, rms);
    mTables->getStatus(BTOW, id, pedstatus, "pedestal");
    mTables->getStatus(BTOW, id, status);
    mPedestal[BTOW-1][id-1] = pedestal;
    mPedRMS[BTOW-1][id-1] = rms;
    mStatus[BTOW-1][id-1] = status * pedstatus;
		
    mTables->getPedestal(BPRS, id, 0, pedestal, rms);
    mTables->getStatus(BPRS, id, status);
    mPedestal[BPRS-1][id-1] = pedestal;
    mPedRMS[BPRS-1][id-1]	= rms;
    mStatus[BPRS-1][id-1]	= status;		
  }
	
  for(int sid=1;sid<=18000;sid++){
		
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
	

  LOG_DEBUG << "got peds and status in InitRun()" << endm;
  LOG_DEBUG << "finished init run for " << run << endm;
  return StMaker::InitRun(run);
}

Int_t StEmcOfflineCalibrationMaker::Make()
{		
  LOG_DEBUG<<"StEmcOfflineCalibrationMaker::Make()"<<endm;
	//get pointers to useful objects
  TChain* chain = muDstMaker->chain(); assert(chain);
  StMuDst* muDst = muDstMaker->muDst(); assert(muDst);
  StMuEvent* event = muDst->event(); assert(event);
  StRunInfo* runInfo = &(event->runInfo()); assert(runInfo);
//	TrgDataType2005* trgData	= (TrgDataType2005*)event->triggerData()->getTriggerStructure(); assert(trgData);
	
  LOG_DEBUG << "general pointer assertions OK" << endm;
	
  const StTriggerId& trigs = event->triggerIdCollection().nominal();
	//toss out fpd1-tpcdead-fast triggers immediately
  for(unsigned int i = 0; i < fastTriggers.size(); i++){
    if(trigs.isTrigger(fastTriggers[i]))return kStOK;
  } 
  HT0towersAboveThreshold.clear();
  HT1towersAboveThreshold.clear();
  HT2towersAboveThreshold.clear();
  HT3towersAboveThreshold.clear();

  myEvent->triggerResult.clear();
  myEvent->towersAboveThreshold.clear();
	
  myEvent->triggerIds = trigs.triggerIds();
  myEvent->l2Result = event->L2Result();	
	//basic event info
  bool fillQA = (runInfo->beamFillNumber(blue)==runInfo->beamFillNumber(yellow));
  myEvent->fill = (fillQA) ? (unsigned short)runInfo->beamFillNumber(blue):0;
  myEvent->run = event->runNumber();
  myEvent->event = event->eventNumber();
  myEvent->date	= GetDate();
  myEvent->time	= GetTime();
	
	//filename manipulations
  TString inputfile(chain->GetFile()->GetName());
  int index1 = inputfile.Index(".MuDst");
  TString string1(inputfile(index1-19,7));
  TString string2(inputfile(index1-7,7));
  myEvent->fileid1 = string1.Atoi();
  myEvent->fileid2 = string2.Atoi();	
	
	//store all vertices from this event	
  myEvent->nVertices	= muDst->numberOfPrimaryVertices();
  for(unsigned int i=0; i<myEvent->nVertices; i++){
    if(i>9){
      LOG_WARN << "found more than 10 vertices for R"<<myEvent->run<< ", event "<<myEvent->event<<endm;
      continue;
    }
		
    assert(muDst->primaryVertex(i));
    StThreeVectorF stvertex = muDst->primaryVertex(i)->position();
    myEvent->vx[i] = stvertex.x();
    myEvent->vy[i] = stvertex.y();
    myEvent->vz[i] = stvertex.z();
    myEvent->ranking[i] = muDst->primaryVertex(i)->ranking();
  }
		
	//fill ADC values from StEmcCollection obtained from MuDst
  //mEmcCollection = muDst->emcCollection();
  mEmcCollection = mADCtoEMaker->getEmcCollection();
  for(int p=0;p<2;p++)
    for(int q=0;q<18000;q++) mADCSmd[p][q]=0;
  getADCs(BTOW);
  getADCs(BPRS);
  getADCs(BSMDP);
  getADCs(BSMDE);
  for(int id=1; id<=4800; id++){
    if((mADC[BTOW-1][id-1] != 0)){
      //if(myEvent->mbTrigger)
      towerSlopes[0]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]);
      //if(myEvent->htTrigger) towerSlopes[1]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]); 
    }
    if(mADC[BPRS-1][id-1] != 0 && mCapacitor[id-1]!= CAP1 && mCapacitor[id-1]!=CAP2){		
      //		LOG_DEBUG << "filling preshower slopes histogram" << endm;
      preshowerSlopes->Fill(id, mADC[BPRS-1][id-1]-mPedestal[BPRS-1][id-1]);
    }
  }
	
  for(int id=1; id<=18000; id++){
    if((mADCSmd[BSMDE-3][id-1] != 0) && mCapacitorSmd[BSMDE-3][id-1] != CAP1 && mCapacitorSmd[BSMDE-3][id-1] != CAP2){
			//if(myEvent->mbTrigger)
      smdSlopes[0]->Fill(id,mADCSmd[BSMDE-3][id-1]);
			//if(myEvent->htTrigger) towerSlopes[1]->Fill(id,mADC[BTOW-1][id-1]-mPedestal[BTOW-1][id-1]); 
    }
    if(mADCSmd[BSMDP-3][id-1] != 0 && mCapacitorSmd[BSMDP-3][id-1] != CAP1 && mCapacitorSmd[BSMDP-3][id-1] != CAP2){
	//		LOG_DEBUG << "filling preshower slopes histogram" << endm;
      smdSlopes[1]->Fill(id, mADCSmd[BSMDP-3][id-1]);
    }
  }
	
	
  LOG_DEBUG << "got ADCs" << endm;


	
	//trigger maker
  for(unsigned int i = 0; i < htTriggers.size(); i++){
    if(trigs.isTrigger(htTriggers[i])){
      myEvent->triggerResult[htTriggers[i]]=emcTrigMaker->isTrigger(htTriggers[i]);
      if(emcTrigMaker->bemc->getTowersAboveThreshold((int)htTriggers[i]).size() > 0){
      myEvent->towersAboveThreshold[htTriggers[i]] = emcTrigMaker->bemc->getTowersAboveThreshold((int)htTriggers[i]);
      }else{
	// Run 9 pp 200
	if(htTriggers[i] == 240530)myEvent->towersAboveThreshold[htTriggers[i]] = HT3towersAboveThreshold;
	if(htTriggers[i] == 240540)myEvent->towersAboveThreshold[htTriggers[i]] = HT2towersAboveThreshold;
	if(htTriggers[i] == 240560)myEvent->towersAboveThreshold[htTriggers[i]] = HT1towersAboveThreshold;
	if(htTriggers[i] == 240570 || htTriggers[i] == 240550)myEvent->towersAboveThreshold[htTriggers[i]] = HT0towersAboveThreshold;
	// Run 11
	//if(htTriggers[i] == 320801 || htTriggers[i] == 330801)myEvent->towersAboveThreshold[htTriggers[i]] = HT3towersAboveThreshold; // add in ht3
	//if(htTriggers[i] == 320503 || htTriggers[i] == 330503)myEvent->towersAboveThreshold[htTriggers[i]] = HT2towersAboveThreshold;
	//if(htTriggers[i] == 320501 || htTriggers[i] == 330501)myEvent->towersAboveThreshold[htTriggers[i]] = HT1towersAboveThreshold;
	//if(htTriggers[i] == 320500 || htTriggers[i] == 320504 || htTriggers[i] == 320514 || htTriggers[i] == 320524 || htTriggers[i] == 330524)myEvent->towersAboveThreshold[htTriggers[i]] = HT0towersAboveThreshold;
      }
    }
  }
  for(unsigned int i = 0; i < httpTriggers.size(); i++){
    myEvent->triggerResult[httpTriggers[i]]=emcTrigMaker->isTrigger(httpTriggers[i]);
    myEvent->towersAboveThreshold[httpTriggers[i]] = emcTrigMaker->bemc->getTriggerPatchesAboveThreshold(httpTriggers[i]);
  }


  /*
  for(unsigned int i = 0; i < mbTriggers.size(); i++){
    myEvent->triggerResult[mbTriggers[i]]=emcTrigMaker->isTrigger(mbTriggers[i]);
  }
  */
  myEvent->htTrigMaker[0] = emcTrigMaker->isTrigger(137213);
  //std::map<int,int>::const_iterator p = (emcTrigMaker->barrelTowersAboveThreshold(137213)).begin();
  //myEvent->htTrigMaker[1] = p->first;
  //myEvent->htTrigMaker[2] = p->second;

  const StEventSummary& evtSummary = muDstMaker->muDst()->event()->eventSummary();
  Double_t mField = evtSummary.magneticField()/10;
  //now for the tracks

  vector<int> track_ids; 

 for(unsigned int vertex_index=0; vertex_index<myEvent->nVertices; vertex_index++){
    muDst->setVertexIndex(vertex_index);
    TObjArray* primaryTracks = muDst->primaryTracks();
    const StMuTrack* track;
    const StMuTrack* primarytrack;

    int nentries = muDst->numberOfPrimaryTracks();
    //cout<<nentries<<" tracks in vertex "<<vertex_index<<endl;
    assert(nentries==primaryTracks->GetEntries());
    pair<unsigned int, pair<float,float> > smd_eta_center;
    pair<unsigned int, pair<float,float> > smd_phi_center;

		//cout<<nentries<<endl;
    for(int i=0; i<nentries; i++){
      track = NULL;
      primarytrack = muDst->primaryTracks(i);
      myTrack->flag = primarytrack->flag();
      myTrack->bad = primarytrack->bad();

      //if(primarytrack->flag()<=0)continue;
      //if(primarytrack->bad())continue;
      //if((float)(primarytrack->nHitsFit()/primarytrack->nHitsPoss()) < 0.51)continue;
      //if(primarytrack->eta() > 2)continue;
      track = primarytrack->globalTrack();
      int primused = 0;
      if(!track)primused=1;
      if(!track)track = primarytrack;
      myTrack->charge = track->charge();
      myTrack->eta = track->eta();
      double p;
      StPhysicalHelixD outerhelix;
      pair<unsigned int, pair<float,float> > center_tower;
      outerhelix = track->outerHelix();
      p = outerhelix.momentum(mField*tesla).mag();
      //float testeta = outerhelix.momentum(mField*tesla).pseudoRapidity();
      myTrack->track = outerhelix.momentum(mField*tesla);
      //p = track->p().mag();
      center_tower = getTrackTower(track, false,1);
      smd_eta_center=getTrackTower(track,false,3);
      smd_phi_center=getTrackTower(track,false,4);
      
      
      //project track to BEMC
      
      int id = center_tower.first;
      int exitid = (getTrackTower(track, true)).first;

			
			//cout<<p<<endl;

      //cout<<i<<" "<<id<<" "<<primused<<" "<<p<<" "<<testeta<<endl;
			
      if(id > 0 && p > 1.0){
	if (id == exitid)track_ids.push_back(id);
	int etaid=smd_eta_center.first;
	int phiid=smd_phi_center.first;
	LOG_DEBUG<<"using track projection to the SMD, we get strips eta: "<<etaid<<" and phi: "<<phiid<<endm;
	if(etaid>0){
	  int smdeids[11];
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

	  for(int i = 0; i < 11; i++){
	    myTrack->smde_id[i] = smdeids[i];
	    myTrack->smde_adc[i] = mADCSmd[0][smdeids[i]-1];
	    myTrack->smde_pedestal[i] = mPedestalSmd[0][smdeids[i]-1];
	    myTrack->smde_pedestal_rms[i] = mPedRMSSmd[0][smdeids[i]-1];
	    myTrack->smde_status[i] = mStatusSmd[0][smdeids[i]-1];
	  }
	}else{
	  for(int i=0;i<11;i++){
	    myTrack->smde_id[i]=0;
	    myTrack->smde_adc[i]=0;
	    myTrack->smde_pedestal[i]=0;
	    myTrack->smde_pedestal_rms[i]=0;
	    myTrack->smde_status[i]=0;
	  }
	}
	if(phiid>0){
	  int smdpids[11];
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

	  for(int i = 0; i < 11; i++){
	    myTrack->smdp_id[i] = smdpids[i];
	    myTrack->smdp_adc[i] = mADCSmd[1][smdpids[i]-1];
	    myTrack->smdp_pedestal[i] = mPedestalSmd[1][smdpids[i]-1];
	    myTrack->smdp_pedestal_rms[i] = mPedRMSSmd[1][smdpids[i]-1];
	    myTrack->smdp_status[i] = mStatusSmd[1][smdpids[i]-1];
	  }
	}else{
	  for(int i=0;i<11;i++){
	    myTrack->smdp_id[i]=0;
	    myTrack->smdp_adc[i]=0;
	    myTrack->smdp_pedestal[i]=0;
	    myTrack->smdp_pedestal_rms[i]=0;
	    myTrack->smdp_status[i]=0;
	  }
	}
			  

				//find neighboring towers
	int softid[9];
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
	for(int tower=0; tower<9; tower++){
	  myTrack->tower_id[tower] = softid[tower];
	  myTrack->tower_adc[tower] = mADC[BTOW-1][softid[tower]-1];
	  myTrack->tower_pedestal[tower] = mPedestal[BTOW-1][softid[tower]-1];
	  myTrack->tower_pedestal_rms[tower] = mPedRMS[BTOW-1][softid[tower]-1];
	  myTrack->tower_status[tower] = mStatus[BTOW-1][softid[tower]-1];
	}
			       				
	for(int tower=0; tower<9; tower++){
	  myTrack->preshower_adc[tower]	= mADC[BPRS-1][softid[tower]-1];
	  myTrack->preshower_pedestal[tower] = mPedestal[BPRS-1][softid[tower]-1];
	  myTrack->preshower_pedestal_rms[tower] = mPedRMS[BPRS-1][softid[tower]-1];
	  myTrack->preshower_status[tower] = mStatus[BPRS-1][softid[tower]-1];
	  myTrack->preshower_cap[tower]	= mCapacitor[softid[tower]-1];
	}
				
	myTrack->p = p;
	myTrack->deta = center_tower.second.first;
	myTrack->dphi = center_tower.second.second;
	myTrack->tower_id_exit = (getTrackTower(track, true)).first;
	myTrack->highest_neighbor = highestNeighbor(myTrack->tower_id[0]);
				
	myTrack->nSigmaElectron	= track->nSigmaElectron();
	myTrack->nHits = track->nHits();
	myTrack->nFitPoints = track->nHitsFit();
	myTrack->nDedxPoints = track->nHitsDedx();
	myTrack->nHitsPossible = track->nHitsPoss();
	myTrack->dEdx = track->dEdx();
	

	/*
	// store TOF information
	
	//if(fabs(muDst->btofHeader()->vpdVz(0)-myEvent->vz[vertex_index]) < 13. && fabs(track->dcaGlobal().mag()) < 1.5 && fabs(myEvent->vz[vertex_index]) < 30. && (Double_t)track->nHitsFit()/(Double_t)track->nHitsPoss() > 0.52 && track->nHitsFit() > 14 && fabs(track->eta()) < 1.)
	//{

	// save these for track QA cuts
	myTrack->vpd_vz = (float)(muDst->btofHeader()->vpdVz(0));
	myTrack->dca_global = (float)(track->dcaGlobal().mag());

	StMuBTofPidTraits tofTraits = track->btofPidTraits();
	myTrack->tofmatchedflag = (int)tofTraits.matchFlag();
	myTrack->toftime  = tofTraits.timeOfFlight();// some tof hits pass matchflag  = 1 with -999 time of flight value
	//if(myTrack->tofmatchedflag >= 1 && myTrack->toftime > 1.0 && myTrack->toftime < 100. && tofTraits.beta() > 0.05)
	myTrack->tofbeta   = tofTraits.beta();// some tof hits pass matchflag  = 1 with -999 beta value
	//else
	//myTrack->tofbeta = -999;
	myTrack->tofpathlength = tofTraits.pathLength();
	myTrack->tofsigmaelectron = tofTraits.sigmaElectron();
	myTrack->tofprobelectron = tofTraits.probElectron();
	//cout << "Added a track with TOF info!" << endl;
	// cut on
	// if(TofMatchedFlag >= 1 && mTofTime > -1. && mTofBeta > -1. && mTofTime < 100.)
	//}
	//else
	//myTrack->tofbeta   = 999;
	*/
	myEvent->addTrack(myTrack);
	//			cout<<"I think I added a track"<<endl;
      }
    }
  }

 for(int i = 0; i < 4800; i++){
   if((mADC[BTOW-1][i]-mPedestal[BTOW-1][i]) < 5 || (mADC[BTOW-1][i]-mPedestal[BTOW-1][i]) > 100)continue;
   for(unsigned int j = 0; j < track_ids.size(); j++){
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
  for(int i=0; i<2; i++){
    for(int j=0; j<4800; j++){
      mADC[i][j] = 0;
    }
  }
  for(int i=0; i<2; i++){
    for(int j=0; j<18000; j++){
      mADCSmd[i][j] = 0;
    }
  }
}

Int_t StEmcOfflineCalibrationMaker::Finish()
{
	LOG_INFO << "cd to output file" << endm;
	myFile->cd();
	LOG_INFO << "write calibration tree" << calibTree->Write() << endm;
	LOG_INFO << "write tower histogram" << towerSlopes[0]->Write() << endm;
	towerSlopes[1]->Write();
	LOG_INFO << "write preshower histogram" << preshowerSlopes->Write() << endm;
	LOG_INFO << "write smd histogram" <<smdSlopes[0]->Write() << endm;
	smdSlopes[1]->Write();
	LOG_INFO << "close the output file" << endm;
	myFile->Close();
	mapFile->cd();
	mapcheck->Write();
	mapFile->Close();
	LOG_INFO << "StEmcOfflineCalibrationMaker::Finish() == kStOk"<<endm;
	return kStOk;
}

void StEmcOfflineCalibrationMaker::getADCs(int det) //1==BTOW, 2==BPRS, 3=BSMDE, 4=BSMDP
{
  det--;
  StDetectorId detectorid = static_cast<StDetectorId>(kBarrelEmcTowerId + det);
  StEmcDetector* detector=mEmcCollection->detector(detectorid);
  if(detector){
    for(int m=1;m<=120;m++){
      StEmcModule* module = detector->module(m);
      if(module)
	{
	  StSPtrVecEmcRawHit& rawHit=module->hits();
	  for(unsigned int k=0;k<rawHit.size();k++){
	    if(rawHit[k]){
	      int module = rawHit[k]->module();
	      int eta = rawHit[k]->eta(); 
	      int submodule = TMath::Abs(rawHit[k]->sub());
	      int ADC = rawHit[k]->adc();
	      int hitid;
	      int stat=-9;	      
	      if(det<2) stat = mEmcGeom->getId(module,eta,submodule,hitid);
	      else if(det==2) stat=mSmdEGeom->getId(module,eta,submodule,hitid);
	      else if(det==3) stat=mSmdPGeom->getId(module,eta,submodule,hitid);
	      if(det == 0 && stat == 0){
		int adc6 = emcTrigMaker->bemc->getHT6bitAdc(hitid);
		pair<int,int> httowerthreshold (hitid,adc6);
		if(adc6 > mHT0threshold)HT0towersAboveThreshold.push_back(httowerthreshold);
		if(adc6 > mHT1threshold)HT1towersAboveThreshold.push_back(httowerthreshold);
		if(adc6 > mHT2threshold)HT2towersAboveThreshold.push_back(httowerthreshold);
		if(adc6 > mHT3threshold)HT3towersAboveThreshold.push_back(httowerthreshold);
	      }
	      if(stat==0){
		if(det<2) mADC[det][hitid-1] = ADC;
		else{
		  mADCSmd[det-2][hitid-1]=ADC;
		  //cout<<"added SMD hit"<<endl;
		}
	      }
	      if(det==1){
		unsigned char CAP =  rawHit[k]->calibrationType();
		if(CAP > 127) CAP -= 128;
		mCapacitor[hitid-1] = CAP;
	      }
	      if(det>1){
		unsigned char CAP=rawHit[k]->calibrationType();
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

/*
//returns (tower, (deta,dphi))
pair<unsigned short, pair<float,float> > StEmcOfflineCalibrationMaker::getTrackTower(StMuTrack* track, bool useExitRadius){
	pair<unsigned short, pair<float,float> > tower;
	tower.first = 0;
	tower.second.first = 1000.;
	tower.second.second = 1000.;

	StThreeVectorD momentum,position;
	Double_t radius = mEmcGeom->Radius();
		
	const StEventSummary& evtSummary = muDstMaker->muDst()->event()->eventSummary();
	Double_t mField = evtSummary.magneticField()/10;
		
	//add 30 cm to radius to find out if track left same tower
	if(useExitRadius) radius += 30.0;
	
	bool goodProjection = mEmcPosition->trackOnEmc(&position,&momentum,track,mField,radius);
	if(goodProjection){
		int m,e,s,id=0;
		float eta=position.pseudoRapidity();
		float phi=position.phi();
		mEmcGeom->getBin(phi,eta,m,e,s);
		
		if(mEmcGeom->getId(m,e,s,id)==0){
			tower.first = id;
			tower.second = getTrackDetaDphi(eta, phi, id);
		}
	}
	
	return tower;
}

pair<float,float> StEmcOfflineCalibrationMaker::getTrackDetaDphi(float track_eta, float track_phi, int id){
	float eta_tower, phi_tower;
	eta_tower = phi_tower = -999.;
	mEmcGeom->getEtaPhi(id,eta_tower,phi_tower);
					
	//now calculate distance from center of tower:
	float dphi = phi_tower - track_phi;
	while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
	while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}

	pair<float, float> dtow;
	dtow.first = eta_tower - track_eta;
	dtow.second = dphi;
	
	return dtow;
}
*/

pair<unsigned short, pair<float,float> > StEmcOfflineCalibrationMaker::getTrackTower(const StMuTrack* track, bool useExitRadius,int det){ //1=BTOW, 3=BSMDE, 4=BSMDP
	pair<unsigned short, pair<float,float> > tower;
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
		int m,e,s,id=0;
		float eta=position.pseudoRapidity();
		float phi=position.phi();
		if(det==1){
		  mEmcGeom->getBin(phi,eta,m,e,s);
		  s = abs(s);
		
		  if(mEmcGeom->getId(m,e,s,id)==0){
		    tower.first = id;
		    tower.second = getTrackDetaDphi(eta, phi, id, det);
		  }
		}
		else if(det==3){
		  int check=mSmdEGeom->getBin(phi,eta,m,e,s);
		  if(!check){
		    s = abs(s);
		    if(mSmdEGeom->getId(m,e,s,id)==0){
		      tower.first = id;
		      tower.second = getTrackDetaDphi(eta, phi, id, det);
		    }
		  }
		}
		else if(det==4){
		  int check=mSmdPGeom->getBin(phi,eta,m,e,s);
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

pair<float,float> StEmcOfflineCalibrationMaker::getTrackDetaDphi(float track_eta, float track_phi, int id, int det){
	float eta_tower, phi_tower;
	eta_tower = phi_tower = -999.;
	pair<float, float> dtow;
	if(det==1){
	  mEmcGeom->getEtaPhi(id,eta_tower,phi_tower);
					
	  //now calculate distance from center of tower:
	  float dphi = phi_tower - track_phi;
	  while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
	  while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}

	  dtow.first = eta_tower - track_eta;
	  dtow.second = dphi;
	}
	if(det==3){
	  mSmdEGeom->getEtaPhi(id,eta_tower,phi_tower);
					
	  //now calculate distance from center of strip:
	  float dphi = phi_tower - track_phi;
	  while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
	  while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}

	  dtow.first = eta_tower - track_eta;
	  dtow.second = dphi;
	}
	if(det==4){
	  mSmdPGeom->getEtaPhi(id,eta_tower,phi_tower);
					
	  //now calculate distance from center of strip:
	  float dphi = phi_tower - track_phi;
	  while(dphi >  M_PI)		{dphi -= 2.0 * M_PI;}
	  while(dphi < -1.*M_PI)	{dphi += 2.0 * M_PI;}

	  dtow.first = eta_tower - track_eta;
	  dtow.second = dphi;
	}
	
	return dtow;
}

double StEmcOfflineCalibrationMaker::highestNeighbor(int id){
	double nSigma=0.;
	
	for(int deta=-1; deta<=1; deta++){
		for(int dphi=-1; dphi<=1; dphi++){
			if(deta==0 && dphi==0) continue;
			int nextId = mEmcPosition->getNextTowerId(id,deta,dphi);
			if(nextId<1 || nextId>4800) continue;
			if((mADC[BTOW-1][nextId-1]-mPedestal[BTOW-1][nextId-1]) > nSigma*mPedRMS[BTOW-1][nextId-1]){
				nSigma = (mADC[BTOW-1][nextId-1] - mPedestal[BTOW-1][nextId-1])/mPedRMS[BTOW-1][nextId-1];
			}
		}
	}
	
	return nSigma;
}

void StEmcOfflineCalibrationMaker::addMinBiasTrigger(unsigned int trigId){ 
	mbTriggers.push_back(trigId);
}

void StEmcOfflineCalibrationMaker::addHighTowerTrigger(unsigned int trigId){ 
	htTriggers.push_back(trigId);
}
void StEmcOfflineCalibrationMaker::addHTTPTrigger(unsigned int trigId){
  httpTriggers.push_back(trigId);
}
void StEmcOfflineCalibrationMaker::addFastTrigger(unsigned int trigId){
  fastTriggers.push_back(trigId);
}
