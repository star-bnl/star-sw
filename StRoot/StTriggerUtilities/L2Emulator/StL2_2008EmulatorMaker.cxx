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
#include "StEvent/StEvent.h"

//MuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"


//trg stuff
#include "StTriggerData2005.h"
//#include "StDaqLib/TRG/L2jetResults2006.h"
//#include "StDaqLib/TRG/trgStructures2005.h"
#include "StDaqLib/TRG/trgStructures.h"
 
//L2 stuff
#include "L2algoUtil/L2EmcDb.h"
#include "L2pedAlgo/L2pedAlgo.h"

#include "StL2_2008EmulatorMaker.h"

ClassImp(StL2_2008EmulatorMaker)

StL2_2008EmulatorMaker::StL2_2008EmulatorMaker(const char *name):StMaker(name) {
  mL2pedAlgo=0;
}

//========================================
StL2_2008EmulatorMaker::~StL2_2008EmulatorMaker(){
}


//========================================
//========================================
Int_t  
StL2_2008EmulatorMaker::InitRun(int runNo){
  //WARN: do NOT use  runNo for any setup - it would break for M-C
  //WARN: do NOT use runNo  to controll setup of L2-algos- it would break for M-C
  
  initRun1();
  LOG_INFO << Form("::setupL2Algos2008(), dbDate=%d  ", mYearMonthDay)<<endm;

  
  mL2algoN=1; // total # of L2 algos (ped, jet, ...)
  mL2algo =new L2VirtualAlgo *[mL2algoN]; // not cleared memeory leak
  memset(mL2algo,0,mL2algoN*sizeof(void*));
  //setup every algo one by one, params may be time dependent
  
  // ----------- L2 ped algo ----------------  slot 0
  addL2pedAlgo( runNo);
  mL2algo[0]=mL2pedAlgo;

  // ----------- L2 jet algo ---------------- slot 1
  // add here

  initRun2();

  LOG_INFO  << "::InitRun() done, run=" <<runNo<<" isMC="<<mMCflag<<endm;
    return kStOK; 
}  

//_____________________________________________________________________________
void
StL2_2008EmulatorMaker::addL2pedAlgo(int runNo){
  //WARN: do NOT use runNo to controll setup of L2-algo- it would break for M-C
  enum {mxPar=10}; // for any algo, separate ints & floats
  int intsPar[mxPar]; // params passed from run control gui
  float floatsPar[mxPar]; 

  
  int L2ResOff = 10; // tmp, needs new interface
  
  TString fullPath=Form("%sL2/%d/algos/algoPed.setup2", mSetupPath.Data(), mYear);
    
  assert(L2VirtualAlgo::readParams(fullPath, mxPar, intsPar, floatsPar) == 5);
  mL2pedAlgo=new L2pedAlgo("ped-algo",mL2EmcDb,mL2EmcDb->logPath,L2ResOff);
  assert(mL2pedAlgo->initRun( runNo,intsPar,floatsPar)==0);
}



//_____________________________________________________________________________
Int_t 
StL2_2008EmulatorMaker::Init() {
  init();
  LOG_INFO <<Form("Init()")<<endm;

 return StMaker::Init();
}

//========================================
void 
StL2_2008EmulatorMaker::Clear(const Option_t*){
  clear();
  //slows down code: LOG_DEBUG<<"StL2_2008EmulatorMaker::Clear()"<<endm;
}


//========================================
Int_t 
StL2_2008EmulatorMaker::Make(){

  make();   
  return kStOK;
}

//========================================
Int_t 
StL2_2008EmulatorMaker::Finish(){
  finish();
  return kStOK;
}


#if 0
//_____________________________________________________________________________
void
StL2_2008EmulatorMaker::initHistoA(TString core) {
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
