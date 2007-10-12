//////////////////////////////////////////////////////////////////////////
//
// StTriggerSimuPlayMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: exercise L0,L2 trigger simulator, lot of examples, change as needed
//
//
//////////////////////////////////////////////////////////////////////////


//get L2
#include "L2Emulator/StL2TriggerSimu.h"

//get HEAD Maker
#include "StTriggerSimuMaker.h"

#include "StTriggerSimuPlayMaker.h"

ClassImp(StTriggerSimuPlayMaker)

StTriggerSimuPlayMaker::StTriggerSimuPlayMaker(const char *name):StMaker(name) {
  mConfig=0;
  mHList=0;

}

//========================================
StTriggerSimuPlayMaker::~StTriggerSimuPlayMaker(){
}


//_____________________________________________________________________________
void
StTriggerSimuPlayMaker::initHistoA(TString core) {
  LOG_INFO <<Form("initHistoA(), mConfig=%d",mConfig)<<endm;
  assert(mHList);

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

//_____________________________________________________________________________
Int_t 
StTriggerSimuPlayMaker::Init() {
  LOG_INFO <<Form("Init(), mConfig=%d",mConfig)<<endm;
  initHistoA("spj_");

 return StMaker::Init();
}

//========================================
void 
StTriggerSimuPlayMaker::Clear(const Option_t*){
  LOG_DEBUG<<"StTriggerSimuPlayMaker::Clear()"<<endm;
}


//========================================
Int_t 
StTriggerSimuPlayMaker::Make(){

  if(mConfig==100) janTest100();

  return kStOK;
}

//========================================
Int_t 
StTriggerSimuPlayMaker::Finish(){

  return kStOK;
}


//========================================
//========================================
//========================================
void
StTriggerSimuPlayMaker::janTest100(){

  printf("in play:JanTest 100\n");
  hA[1]->Fill(2);
}


// $Id: StTriggerSimuPlayMaker.cxx,v 1.1 2007/10/12 20:10:23 balewski Exp $
//
