#include "StEStructGevsim.h"
#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

#include "TParticle.h"

StEStructGevsim::StEStructGevsim(): mgevsim(0), meventCount(0), meventsToDo(0), mAmDone(false) {};

StEStructGevsim::StEStructGevsim(int nevents, TGeVSim* gevsim, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){
  meventsToDo=nevents;
  mgevsim=gevsim;
  mECuts=ecuts;
  mTCuts=tcuts;

  mtrackArray = mgevsim->GetListOfParticles();  // pointer to gevsim's internal TClonesArray

};

bool StEStructGevsim::hasGenerator() { return (mgevsim) ? true : false ; };

//-------------------------------------------------------------------------
StEStructEvent* StEStructGevsim::next() {

  if(!mgevsim || meventCount==meventsToDo){
    mAmDone=true;
    return (StEStructEvent*)NULL;
  }
  return generateEvent();
}

//--------------------------------------------------------------------------
StEStructEvent* StEStructGevsim::generateEvent(){

  StEStructEvent* retVal=NULL;

  if(mgevsim) mgevsim->GenerateEvent(); // fills mtrackArray with TParticles

  retVal = new StEStructEvent();

  fillTracks(retVal);
  retVal->SetCentrality((double) mrefMult);
  //bool useEvent= (mECuts->goodNumberOfTracks(mrefMult));
  bool useEvent= (mECuts->goodCentrality(mrefMult));
  if(!useEvent){
    delete retVal;
    retVal=NULL;
  } else {
    retVal->FillChargeCollections();
  }
  mECuts->fillHistogram(mECuts->centralityName(),(float)mrefMult,useEvent);

  return retVal;
}   

//--------------------------------------------------------------------------
void StEStructGevsim::fillTracks(StEStructEvent* estructEvent){

  mrefMult=0;
  int numParticles=mtrackArray->GetEntries(); 

  StEStructTrack* eTrack= new StEStructTrack();

  for(int i=0;i<numParticles;i++){ 
    TParticle* track = (TParticle*)mtrackArray->At(i);

    // From here on, this is similar to the mudst reader with muTrack->TParticle

    eTrack->SetInComplete();

    // check cuts first, then fill
    bool useTrack=true;
    float pt = track->Pt();
    if(pt<0.15)continue;

    float eta = track->Eta();
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    float phi = track->Phi();
    if(phi>M_PI) phi -= 2*M_PI;
    useTrack = (mTCuts->goodPhi(phi) && useTrack);
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    float _r=pt/0.139;
    float yt=log(sqrt(1+_r*_r)+_r);
    useTrack = (mTCuts->goodYt(yt) && useTrack);
    float mt = sqrt(pt*pt + 0.139*0.139);
    float xt = 1 - exp( -1*(mt-0.139)/0.4 );
    useTrack = (mTCuts->goodXt(xt) && useTrack);

    int pdgCode = track->GetPdgCode();
    int charge = 0;
    if(pdgCode>0) charge = 1;
    else charge = -1;
    useTrack = ( mTCuts->goodCharge(charge) && useTrack);

    if(useTrack) {
      mrefMult++;
      mTCuts->fillHistograms(useTrack);
      
      // now fill the eTrack
      eTrack->SetPx(track->Px());
      eTrack->SetPy(track->Py());
      eTrack->SetPz(track->Pz());
      eTrack->SetBx(0);
      eTrack->SetBy(0);
      eTrack->SetBz(0);
      eTrack->SetBxGlobal(0);
      eTrack->SetByGlobal(0);
      eTrack->SetBzGlobal(0);
      eTrack->SetEta(eta);
      eTrack->SetPhi(phi);
      eTrack->SetDetectorID(1); //TPC
      eTrack->SetCharge(charge);

      estructEvent->AddTrack(eTrack);
    } //useTrack

    
  }; // particle loop
    

  delete eTrack;
  return;
  
}    

