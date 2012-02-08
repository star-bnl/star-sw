////
#include "StFgtMCEvalMaker.h"
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuMcTrack.h>
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"

Int_t StFgtMCEvalMaker::Make(){
   Int_t ierr = kStOk;
  StMcEvent* mMcEvent = 0;  
  mMcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");
  //assert(mMcEvent);
  cout <<" in eval maker" << endl;
  if(!mMcEvent) return false;
  cout <<"got mcevent!!!!" <<endl;
  //initialize momentum vectors
    StMcVertex *V=mMcEvent->primaryVertex(); 
  //  mVertex=TVector3(V->position().x(),V->position().y(),V->position().z());

  for(unsigned int i=0; i<mMcEvent->tracks().size();i++){//loop tracks
    StMcTrack* mcTrack = mMcEvent->tracks()[i];
    int pdgId=mcTrack->pdgId();
    float pt=mcTrack->pt();
    i++;
    if(abs(pdgId)==11 || abs(pdgId)==12 || abs(pdgId)==24 || abs(pdgId)==21 || abs(pdgId) < 10 || abs(pdgId)==92 || pt<10.0) continue;
    cout<<"high pt MC track: pdgId="<<pdgId<<" pt="<<pt<<endl;
  }
  return ierr;
};

Int_t StFgtMCEvalMaker::Finish(){
   Int_t ierr = kStOk;
   return ierr;
};

Int_t StFgtMCEvalMaker::Init(){
   Int_t ierr = kStOk;
   return ierr;
};
