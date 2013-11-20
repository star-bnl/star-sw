#include "StMessMgr.h"
#include "StLuminosityMaker.h"
#include "StLuminosityHolder.h"
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"
#include "StDetectorDbMaker/StDetectorDbBeamInfo.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StTriggerId.h"
#include "StSpinPool/StTriggerFilterMaker/StTriggerFilterMaker.h"

ClassImp(StLuminosityMaker)

StLuminosityMaker::StLuminosityMaker(const char* name):StMaker(name)
{
  mTriggers.clear();
  mNTotal.clear();
  mNCuts.clear();
  mPrescales.clear();
  mLumTotal.clear();
  mLumCuts.clear();
  mXsec = -1.0;
  runMode = "";
  runNumber = 0;
  mVertexCut = 0;
  mOverrideMode = 0;
  muDstMaker = NULL;
  mTriggerSimuMaker = NULL;
  mLumHolder = NULL;
  mFilterMode = 0;
}
//_____________________________________________________________________________
Int_t StLuminosityMaker::Init()
{
  muDstMaker = dynamic_cast<StMuDstMaker*>(GetMaker("MuDst"));
  assert(muDstMaker);
  mTriggerSimuMaker = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));
  if(!strcmp(runMode,"")){
    LOG_ERROR<<"runMode is not set"<<endm;
    return kStErr;
  }

  LOG_INFO<<"Using minbias cross-section: "<<getCrossSectionNB()<<" (nb)"<<endm;

  mLumHolder = new TClonesArray("StLuminosityHolder",20);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StLuminosityMaker::InitRun(int run)
{
  if(runNumber != 0){
    saveOutput();
    printOutput();
  }
  LOG_INFO<<"Now starting Run#"<<run<<endm;
  runNumber = run;
  for(unsigned int i = 0; i < mTriggers.size(); i++){
    mNTotal[i] = 0;
    mNCuts[i] = 0;
    mNVertex[i] = 0;
    mPrescales[i] = 0.0;
  }

  return kStOk;

}
//_____________________________________________________________________________
Int_t StLuminosityMaker::Finish(){
  saveOutput();
  printOutput();
  return StMaker::Finish();
}
//_____________________________________________________________________________
Int_t StLuminosityMaker::Make(){
  StDetectorDbTriggerID* dbtrig = StDetectorDbTriggerID::instance();
  StMuDst* mudst = muDstMaker->muDst();assert(mudst);
  StMuEvent* muevent = mudst->event();assert(muevent);

  int passtrig = 0;
  int passvert = 0;

  const StTriggerId& trigs = muevent->triggerIdCollection().nominal();

  for(unsigned int i = 0; i < mTriggers.size(); i++){
    if(trigs.isTrigger(mTriggers[i])){
      float prs = dbtrig->getTotalPrescaleByTrgId(mTriggers[i]);
      mPrescales[i] = prs;
      mNTotal[i]++;
      passtrig++;
      float vertz = TMath::Abs(muevent->primaryVertexPosition().z());
      if(vertz > 1e-5)mNVertex[i]++;
      if(mTriggerSimuMaker && !mTriggerSimuMaker->isTrigger(mTriggers[i]))continue;
      mNSoftTrig[i]++;
      if(mVertexCut > vertz && vertz > 1e-5){
	mNCuts[i]++;
	passvert++;
      }
    }
  }
  if(mFilterMode && !passvert && !passtrig) return kStSkip;

  return kStOK;
}
//_____________________________________________________________________________
void StLuminosityMaker::addTrigger(unsigned int trigId)
{
  mTriggers.push_back(trigId);
  mNTotal.push_back(0);
  mNCuts.push_back(0);
  mNVertex.push_back(0);
  mNSoftTrig.push_back(0);
  mPrescales.push_back(0.0);
  mLumTotal.push_back(0.0);
  mLumCuts.push_back(0.0);
  mLumVertex.push_back(0.0);
  mLumSoftTrig.push_back(0.0);
}
//_____________________________________________________________________________
void StLuminosityMaker::setMode(const char* newMode)
{
  float oldxsec = mXsec;

  if(!strcmp(newMode,"dAu2008")){
    runMode = newMode;
    mXsec = 13.8e3;
  }else if(!strcmp(newMode,"pp2008")){
    runMode = newMode;
    mXsec = 26.1e6;
  }else if(!strcmp(newMode,"AuAu2007")){
    runMode = newMode;
    mXsec = 10e9;
  }else if(!strcmp(newMode,"pp2006")){
    runMode = newMode;
    mXsec = 26.1e6;
  }else if(!strcmp(newMode,"pp2006_62GeV")){
    runMode = newMode;
    mXsec = 20e6;
  }else if(!strcmp(newMode,"pp2005")){
    runMode = newMode;
    mXsec = 26.1e6;
  }else if(!strcmp(newMode,"CuCu2005")){
    runMode = newMode;
    mXsec = 3.3e9;
  }else if(!strcmp(newMode,"pp2004")){
    runMode = newMode;
    mXsec = 26.1e6;
  }else if(!strcmp(newMode,"AuAu2004")){
    runMode = newMode;
    mXsec = 6.5e9;
  }else if(!strcmp(newMode,"dAu2003")){
    runMode = newMode;
    mXsec = 2.2e9 * .2;
  }else if(!strcmp(newMode,"pp2003")){
    runMode = newMode;
    mXsec = 26.1e6;
  }else if(strcmp(newMode,"")){
    LOG_ERROR<<"Invalid mode "<<newMode<<" passed to StLuminosityMaker"<<endm;
  }else{
    LOG_ERROR<<"StLuminosityMaker requires a running mode"<<endm;
  }
  if(mOverrideMode){
    mXsec = oldxsec;
  }

}
//_____________________________________________________________________________
void StLuminosityMaker::setCrossSectionNB(float newXsec,int overRideMode)
{
  mXsec = newXsec;
  mOverrideMode = overRideMode;
  runMode = "manual";
}
//_____________________________________________________________________________
void StLuminosityMaker::printOutput()
{
  LOG_INFO<<"Luminosity analyzed for Run #"<<runNumber<<":"<<endm;
  for(unsigned int i = 0; i < mTriggers.size(); i++){
    float totalsampled = 0;
    float cutsampled = 0;
    if(mNTotal[i] > 0){
      totalsampled = mPrescales[0]*mNTotal[0]/(mPrescales[i]*mXsec);
      cutsampled = totalsampled * mNCuts[i] / mNTotal[i];
    }

    LOG_INFO<<"Trigger "<<mTriggers[i]<<": "<<totalsampled<<" nb^-1 total and "<<cutsampled<<" nb^-1 after cuts, "<<mNTotal[i]<<" "<<mNCuts[i]<<endm;
  }
}
//_____________________________________________________________________________
void StLuminosityMaker::getTriggersFromFilterMaker(const char* filtername)
{
  StTriggerFilterMaker* filter = dynamic_cast<StTriggerFilterMaker*>(GetMaker(filtername));
  assert(filter);
  vector<unsigned int>filtertriggers = filter->getTriggers();
  for(unsigned int i = 0; i < filtertriggers.size(); i++){
    addTrigger(filtertriggers[i]);
  }
}
//_____________________________________________________________________________
void StLuminosityMaker::saveOutput()
{
  for(unsigned int i = 0; i < mTriggers.size(); i++){
    float totalsampled = 0;
    float vertsampled = 0;
    float softtrigsampled = 0;
    float cutsampled = 0;
    if(mNTotal[i] > 0){
      totalsampled = mPrescales[0]*mNTotal[0]/(mPrescales[i]*mXsec);
      cutsampled = totalsampled * mNCuts[i] / mNTotal[i];
      softtrigsampled = totalsampled * mNSoftTrig[i] / mNTotal[i];
      vertsampled = totalsampled * mNVertex[i] / mNTotal[i];
    }
    mLumTotal[i] = totalsampled;
    mLumCuts[i] = cutsampled;
    mLumSoftTrig[i] = softtrigsampled;
    mLumVertex[i] = vertsampled;
  }

  StLuminosityHolder* lum = new StLuminosityHolder(runNumber);

  lum->setTriggers(mTriggers);
  lum->setLumTotal(mLumTotal);
  lum->setLumCuts(mLumCuts);
  lum->setLumVertex(mLumVertex);
  lum->setLumSoftTrig(mLumSoftTrig);
  lum->setNTotal(mNTotal);
  lum->setNCuts(mNCuts);
  lum->setNVertex(mNVertex);
  lum->setNSoftTrig(mNSoftTrig);
  lum->setPrescales(mPrescales);
  lum->setCrossSectionNB(mXsec);
  lum->setVertexCutcm(mVertexCut);

  new ( (*mLumHolder)[mLumHolder->GetLast()+1] ) StLuminosityHolder(*lum);

}
