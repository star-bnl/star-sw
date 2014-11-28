//////////////////////////////////////////////////////////////////////////
//
// StTriggerSimuPlayMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: exercise L0,L2 trigger simulator, lot of examples, change as needed
//
//
//////////////////////////////////////////////////////////////////////////


//get L2
#include "StTriggerSimuMaker.h"

//get HEAD Maker
#include "StTriggerSimuMaker.h"
#include "StBbcTriggerSimu.h"
#include "StBemcTriggerSimu.h"
#include "StL2TriggerSimu.h"
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

  int trigID=137611;
  printf("%s:JanTest100()\n", GetName());


  hA[1]->Fill(2);
  StTriggerSimuMaker *trgSimMk= (StTriggerSimuMaker*) StMaker::GetChain()->GetMaker("StarTrigSimu");
  assert(trgSimMk);
  int dec=trgSimMk->isTrigger(trigID);
  printf("trigID=%d final decision=%d ,",trigID,dec);
  if(trgSimMk->bbc) printf("L0-BBC decision==%d ,",trgSimMk->bbc->triggerDecision(trigID));
  if(trgSimMk->bemc) printf("L0-BEMC decision=%d ,",trgSimMk->bemc->triggerDecision(trigID));
  if(trgSimMk->lTwo) printf("L2 decision=%d ",trgSimMk->lTwo->triggerDecision(trigID));
  printf("\n");
}


// $Id: StTriggerSimuPlayMaker.cxx,v 1.3 2007/11/18 21:58:50 balewski Exp $
//
