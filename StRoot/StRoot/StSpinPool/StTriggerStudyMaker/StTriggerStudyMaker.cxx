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

//StEmc
#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/database/StEmcDecoder.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcTriggerMaker/StEmcTriggerMaker.h"

//logger
#include "StMessMgr.h"


#include "StTriggerStudyMaker.h"
#include "StDetectorDbMaker/StDetectorDbBeamInfo.h"
#include "StSpinPool/StTriggerFilterMaker/StTriggerFilterMaker.h"
#include "StTriggerStudyEvent.h"
#include "StEnumerations.h"
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"
#include "TTree.h"
#include "TFile.h"

ClassImp(StTriggerStudyMaker)

StTriggerStudyMaker::StTriggerStudyMaker(const char* filename, const char* name):StMaker(name),nTriggers(12)
{
  mName = filename;
  runNumber = 0;
  muDstMaker = NULL;
  mTriggerSimuMaker = NULL;
  mTree = NULL;
  mFile = NULL;
  mTSEvent = NULL;
}
//_____________________________________________________________________________
Int_t StTriggerStudyMaker::Init()
{
  int trigarray[12] = {117001,137221,137222,137822,117300,137571,137575,137585,137586,137611,137501,137622};
  for(int i = 0; i < nTriggers; i++){
    triggers[i] = trigarray[i];
    trigmap[triggers[i]] = i;
  }

  mFile = new TFile(mName,"RECREATE"); 
  mTSEvent = new StTriggerStudyEvent();
  mTree = new TTree("trigStudyTree","Trigger Study Tree");
  mTree->Branch("event_branch","StTriggerStudyEvent",&mTSEvent);
  mTree->SetAutoSave(1000000000);

  muDstMaker = dynamic_cast<StMuDstMaker*>(GetMaker("MuDst"));
  assert(muDstMaker);
  mADCtoEMaker	= dynamic_cast<StEmcADCtoEMaker*>(GetMaker("Eread")); assert(mADCtoEMaker);
  mTriggerSimuMaker = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));

  mDecoder = new StEmcDecoder();
  mEmcGeom = StEmcGeom::instance("bemc");
  mBemcTables = mADCtoEMaker->getBemcData()->getTables();

  zvertall = new TH1F("zvertall","Z Vertex Distribution",280,-139.5,140.5);
  bbctall = new TH1F("bbctall","BBC Time Bins",18,-1.5,16.5);
  jp1et = new TH1F("jp1et","JP1 Et Distribution",70,17.9,31.9);
  jp2et = new TH1F("jp2et","JP2 Et Distribution",70,17.9,31.9);
  jp1et0 = new TH1F("jp1et0","JP1 Et Distribution",70,3.9,17.9);
  jp0et0 = new TH1F("jp0et0","JP0 Et Distribution",70,3.9,17.9);
  for(int i = 0; i < nTriggers; i++){
    trigmap[triggers[i]] = i;
    char namez[100];
    sprintf(namez,"zvert%i",triggers[i]);
    zverttrig[i] = new TH1F(namez,"Z Vertex Distribution",280,-139.5,140.5);
    char nameb[100];
    sprintf(nameb,"bbct%i",triggers[i]);
    bbcttrig[i] = new TH1F(nameb,"BBC Time Bins",18,-1.5,16.5);
  }

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StTriggerStudyMaker::InitRun(int run)
{
  if(runNumber != 0){
    LOG_ERROR<<"Attempting to change run: check file list"<<endm;
    return kStErr;
  }
  LOG_INFO<<"Now starting Run#"<<run<<endm;
  runNumber = run;
  return kStOk;

}
//_____________________________________________________________________________
Int_t StTriggerStudyMaker::Finish(){
  mFile->cd();
  mTree->Write();
  zvertall->Write();
  bbctall->Write();
  jp1et->Write();
  jp2et->Write();
  jp1et0->Write();
  jp0et0->Write();
  for(int i = 0; i < nTriggers; i++){
    zverttrig[i]->Write();
    bbcttrig[i]->Write();
  }
  mFile->Close();
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StTriggerStudyMaker::Make(){
  fillTree();
  mTSEvent->Clear();
  return kStOK;
}
//_____________________________________________________________________________
void StTriggerStudyMaker::fillTree()
{
  StDetectorDbTriggerID* dbtrig = StDetectorDbTriggerID::instance();
  StMuDst* mudst = muDstMaker->muDst();assert(mudst);
  StMuEvent* muevent = mudst->event();assert(muevent);

  int bbctimebin = muevent->bbcTriggerDetector().onlineTimeDifference();
  mTSEvent->setBbcTimeBin(bbctimebin);
  bbctall->Fill(bbctimebin/32);

  float vertmag = TMath::Abs(muevent->primaryVertexPosition().mag());
  float vertz = muevent->primaryVertexPosition().z();
  if(vertmag > 1e-7)mTSEvent->setVertexPosition(muevent->primaryVertexPosition());
  if(vertmag > 1e-7)zvertall->Fill(vertz);

  int isjp1 = 0;
  int isjp0 = 0;
  int isjp2 = 0;
  float jpEt[12] = {0,0,0,0,0,0,0,0,0};
  float maxjpEt = -1;
  mEmcCollection = mudst->emcCollection();
  StDetectorId detectorid = static_cast<StDetectorId>(kBarrelEmcTowerId);
  StEmcDetector* detector=mEmcCollection->detector(detectorid);
  if(detector){
    for(int m=1;m<=120;m++){
      StEmcModule* module = detector->module(m);
      if(module){
	StSPtrVecEmcRawHit& rawHit=module->hits();
	for(unsigned int k=0;k<rawHit.size();k++){
	  if(rawHit[k]){
	    int module = rawHit[k]->module();
	    int eta = rawHit[k]->eta(); 
	    int submodule = abs(rawHit[k]->sub());
	    int ADC = rawHit[k]->adc();
	    int hitid;
	    int stat = mEmcGeom->getId(module,eta,submodule,hitid);
	    int status;
	    mBemcTables->getStatus(BTOW,hitid,status);
	    float pedestal,rms;
	    mBemcTables->getPedestal(BTOW,hitid,0,pedestal,rms);
	    if(stat==0&&status==1&&((float)ADC-pedestal) > 2*rms){
	      //mTSEvent->addTowerADC(hitid,(float)ADC-pedestal);
	      float theta;
	      mEmcGeom->getTheta(hitid,theta);
	      float calib = mBemcTables->calib(BTOW,hitid);
	      float energy = calib * (float)(ADC-pedestal);
	      float et = energy*sin(theta);
	      if(et > 0.2){
		int jpID;
		mDecoder->GetJetPatchFromTowerId(hitid,jpID);
		jpEt[jpID]+=et;
	      }
	    }
	  }
	}
      }
      else { LOG_WARN<<"couldn't find StEmcModule "<<m<<" for detector "<<BTOW<<endm;}
    }
  }

  const StTriggerId& trigs = muevent->triggerIdCollection().nominal();
  vector<unsigned int>triggers = trigs.triggerIds();
  vector<unsigned int>prescales;
  vector<unsigned int>simutriggers;

  mTSEvent->setTriggers(triggers);

  for(unsigned int i = 0; i < triggers.size(); i++){
    unsigned int prs = (unsigned int)dbtrig->getTotalPrescaleByTrgId(triggers[i]);
    prescales.push_back(prs);

    if(triggers[i] == 137222)isjp1 = 1;
    if(triggers[i] == 137501)isjp0 = 1;
    if(triggers[i] == 137585)isjp2 = 1;
    map<int,int>::iterator iter = trigmap.find(triggers[i]);
    if(iter!=trigmap.end()){
      int tr = iter->second;
      if(vertmag > 1e-7)zverttrig[tr]->Fill(vertz);
      bbcttrig[tr]->Fill(bbctimebin/32);
    }
    if(mTriggerSimuMaker && mTriggerSimuMaker->isTrigger(triggers[i])){
      simutriggers.push_back(triggers[i]);
      StTriggerSimuResult trigsimuresult = mTriggerSimuMaker->detailedResult(triggers[i]);
      vector<short> towers = trigsimuresult.highTowerIds();
      for(unsigned int j = 0; j < towers.size(); j++){
	int dsm = trigsimuresult.highTowerAdc(towers[j]);
	//if(dsm > 0)mTSEvent->addTowerDSM(towers[j],dsm);
      }
      vector<short> jpatches = trigsimuresult.jetPatchIds();
      for(unsigned int j = 0; j < jpatches.size(); j++){
	int dsm = trigsimuresult.jetPatchAdc(jpatches[j]);
	if(dsm > 0)mTSEvent->addJPatchDSM(jpatches[j],dsm);
      }
      vector<short> tpatches = trigsimuresult.triggerPatchIds();
      for(unsigned int j = 0; j < tpatches.size(); j++){
	int dsm = trigsimuresult.triggerPatchAdc(tpatches[j]);
	if(dsm > 0)mTSEvent->addTPatchDSM(tpatches[j],dsm);
      }
    }
  }
  vector<float> jpetvec;
  for(int k = 0; k < 12; k++){
    if(jpEt[k] > maxjpEt)maxjpEt = jpEt[k];
    jpetvec.push_back(jpEt[k]);
  }
  if(isjp1) jp1et->Fill(maxjpEt);
  if(isjp1 && isjp2)jp2et->Fill(maxjpEt);
  if(isjp0) jp0et0->Fill(maxjpEt);
  if(isjp0 && isjp1)jp1et0->Fill(maxjpEt);

  mTSEvent->setJPEt(jpetvec);
  mTSEvent->setPrescales(prescales);
  mTSEvent->setSimuTriggers(simutriggers);

  mTree->Fill();
}
