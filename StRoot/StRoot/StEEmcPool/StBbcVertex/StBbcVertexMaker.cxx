// *-- Author : Jan Balewski
// 
// $Id: StBbcVertexMaker.cxx,v 1.3 2011/04/11 19:35:40 fisyak Exp $

#include <TFile.h>
 
#include "StBbcVertexMaker.h"

#include "TChain.h"
#include "TClonesArray.h"
#include "StL0Trigger.h"
#include "StEventInfo.h"
#include "StEventSummary.h"
#include "StarClassLibrary/StThreeVectorF.hh"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StEvent/StTriggerId.h"

#include <StMessMgr.h>
#include "BbcHex.h"

ClassImp(StBbcVertexMaker)

//________________________________________________
//________________________________________________
StBbcVertexMaker::StBbcVertexMaker(const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

}


//___________________ _____________________________
//________________________________________________
StBbcVertexMaker::~StBbcVertexMaker(){
 // Save all objects in this file
  // hfile->Write();
}

//________________________________________________
//________________________________________________
Int_t StBbcVertexMaker::InitRun(int runNo){
  initRun(runNo);
  return kStOK;
}
 
//________________________________________________
//________________________________________________
Int_t StBbcVertexMaker::Init(){
  BbcVertex::init();
  //  gMessMgr->Info() << GetName() << "has MCflag="<< MCflag<<endm;
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t StBbcVertexMaker::Finish(){
  finish();
  return kStOK;
}
 
 
 

//________________________________________________
//________________________________________________
Int_t StBbcVertexMaker::Make(){
  clear();  

  if( !unpackMuTrig()) return kStOK;

  doVertex();

  return kStOK;
}




//________________________________________________
//________________________________________________
bool StBbcVertexMaker::unpackMuTrig(){
  
  //  printf("%s::unpackMuTrig() is called ..........\n",StMaker::GetName());
 
  // Access to muDst .......................
  StMuEvent* muEve = mMuDstMaker->muDst()->event();
  //  int nPrim = mMuDstMaker->muDst()->primaryTracks()->GetEntries();  // get number of primary tracks
 
  StEventInfo &info=muEve->eventInfo();
  unixTime=info.time();
  if(unixTime0==0) unixTime0=unixTime;

  StMuTriggerIdCollection& trgIdColl=muEve->triggerIdCollection();

  StEventSummary &smry=muEve->eventSummary();
  StThreeVectorF ver=smry.primaryVertexPosition();

  zTpc=ver.z();
  if( fabs(ver.x())<0.000001 &&fabs(ver.y())<0.000001 &&fabs(ver.z())<0.000001 ) {
    zTpc=999;
    return false;
  }

  const StTriggerId& oflTrgId=trgIdColl.nominal();
  vector<unsigned int> trgId=oflTrgId.triggerIds();

  // printf("\n\n ==================== processing eventID %d nPrim=%d nTrig=%d==============\n", info.id(),nPrim, trgId.size());

  bool isGood=false;
  UInt_t i;
  for(i = 0; i < trgId.size() ; i++){
    // printf("i=%d trgId=%d\n",i,trgId[i]);
    //.......... minB trig in pp200 in 2004
    if(trgId[i]==10) isGood=true; // minb-slow
    if(trgId[i]==45010) isGood=true; // minb-slow
    //   if(trgId[i]==45020) isGood=true; // minb-fast
    // if(trgId[i]==45203) isGood=true; // eth-1-slow
  }
  if(!isGood)  return false;

  StBbcTriggerDetector & bbc=muEve->bbcTriggerDetector ();
  for(i=0;i<mxHex;i++) {
    hex[0][i]->setHit(bbc.tdc(i),bbc.adc(i));
    hex[1][i]->setHit(bbc.tdc(i+24),bbc.adc(i+24));
    //    hex[0][i]->print();  hex[1][i]->print();
  }
  onlTdiff=bbc.onlineTimeDifference ();
  return isGood;
}




// $Log: StBbcVertexMaker.cxx,v $
// Revision 1.3  2011/04/11 19:35:40  fisyak
// Replace uint by UInt_t, use TMath
//
// Revision 1.2  2004/12/04 05:07:38  balewski
// export to NN
//
// Revision 1.1  2004/08/31 03:44:13  balewski
// first
//
// Revision 1.7  2004/08/26 04:39:40  balewski
