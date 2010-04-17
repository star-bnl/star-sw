//////////////////////////////////////////////////////////////////////////
//
// StL2_2008EmulatorMaker  Jan Balewski  (Fall, 2007)
//
// Goal: 
//
//
//////////////////////////////////////////////////////////////////////////
// StEvent
#include "StEventTypes.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

//L2 stuff
#include "L2jetAlgo/L2jetAlgo2009.h"
#include "StL2_2009EmulatorMaker.h"

ClassImp(StL2_2009EmulatorMaker)

StL2_2009EmulatorMaker::StL2_2009EmulatorMaker(const char *name) : StMaker(name){
}

//========================================
StL2_2009EmulatorMaker::~StL2_2009EmulatorMaker(){
}

#if 1 // disable for now, fix it one pp 2009 run starts,Jan 
//========================================
//========================================
Int_t  
StL2_2009EmulatorMaker::InitRun(int runNo){
  //WARN: do NOT use  runNo for any setup - it would break for M-C
  //WARN: do NOT use runNo  to controll setup of L2-algos- it would break for M-C
  
  initRun1();
  LOG_INFO << Form("::setupL2Algos2009(), dbDate=%d  ", mYearMonthDay)<<endm;

  
  //setup every algo one by one, params may be time dependent
  mL2algo.push_back(new L2jetAlgo2009("jet09-algo",mL2EmcDb,mL2EmcDb->logPath,20)); // tmp:offset=20;
  //   add here L2adc2energy and test saving output


  initRun2(runNo);

  LOG_INFO  << "::InitRun() done, run=" <<runNo<<" isMC="<<mMCflag<<endm;
    return kStOK; 
}  
#endif
//_____________________________________________________________________________
Int_t 
StL2_2009EmulatorMaker::Init() {
  init();
  LOG_INFO <<Form("Init()")<<endm;

 return StMaker::Init();
}

//========================================
void 
StL2_2009EmulatorMaker::Clear(const Option_t*){
  clear();
  //slows down code: LOG_DEBUG<<"StL2_2009EmulatorMaker::Clear()"<<endm;
}


//========================================
Int_t 
StL2_2009EmulatorMaker::Make(){

  make();   
  return kStOK;
}

//========================================
Int_t 
StL2_2009EmulatorMaker::Finish(){
  finish();
  return kStOK;
}


#if 0
//_____________________________________________________________________________
void
StL2_2009EmulatorMaker::initHistoA(TString core) {
  LOG_INFO <<Form("initHistoA(),")<<endm;

  assert(mHList);
  memset(hA,0,sizeof(hA));

  hA[0]=0;
  hA[1]=new TH1F(core+"trgID", "event counter for Jan",10,1,11);

  //.... add histos to the list
  int i;
  for(i=0;i<mxAH;i++) {
    if( hA[i]==0) continue;
    mHList->Add( hA[i]);
  }
}
#endif
