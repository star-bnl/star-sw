/**********************************************************************
 *
 * $Id: StEStructPythia.cxx,v 1.3 2004/06/09 22:37:51 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for (T)Pythia event generator
 *
 **********************************************************************/
#include "StEStructPythia.h"

#include "StEStructPool/AnalysisMaker/StEStructEventCuts.h"
#include "StEStructPool/AnalysisMaker/StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

StEStructPythia::StEStructPythia(): mpythia(0), meventCount(0), meventsToDo(0), mAmDone(false), mECuts(0), mTCuts(0){};

StEStructPythia::StEStructPythia(int nevents, TPythia6* pythia, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts): meventCount(0), mAmDone(false){

  
  meventsToDo=nevents;
  mpythia=pythia;
  mECuts=ecuts;
  mTCuts=tcuts;

};

bool StEStructPythia::hasGenerator() { return (mpythia) ? true : false ; };
bool StEStructPythia::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructPythia::hasTrackCuts() { return (mTCuts) ? true : false ; }


//-------------------------------------------------------------------------
StEStructEvent* StEStructPythia::next() {

  if(!mpythia || meventCount==meventsToDo){
    mAmDone=true;
    return (StEStructEvent*)NULL;
  }
  return generateEvent();
}

//--------------------------------------------------------------------------
StEStructEvent* StEStructPythia::generateEvent(){

  StEStructEvent* retVal=NULL;

  if(mpythia) mpythia->GenerateEvent();

  retVal = new StEStructEvent();

  fillTracks(retVal);
  bool useEvent=mECuts->goodNumberOfTracks(mrefMult);
  if(!useEvent){
    delete retVal;
    retVal=NULL;
  } else {
    retVal->FillChargeCollections();
  }
  mECuts->fillHistogram(mECuts->numTracksName(),(float)mrefMult,useEvent);

  return retVal;
}   

//--------------------------------------------------------------------------
void StEStructPythia::fillTracks(StEStructEvent* estructEvent){

  mrefMult=0;
  Pyjets_t* pstr= mpythia->GetPyjets();
  int numParticles=mpythia->GetN();

  StEStructTrack* eTrack= new StEStructTrack();
  int pid;

  for(int i=2;i<numParticles;i++){ // 0 & 1 for incoming protons 

    eTrack->SetInComplete();
    pid = pstr->K[1][i];
    if(!measureable(pid))continue;  // checks if pi,k,p or e
    float p[3];
    float v[3];
    for(int k=0;k<3;k++){
      p[k]=pstr->P[k][i];
      v[k]=pstr->V[k][i];
    }

    float pt=sqrt(p[0]*p[0]+p[1]*p[1]);
    if(pt<0.15)continue;


    float eta=-999.;

    float ptotal=sqrt(p[0]*p[0]+p[1]*p[1]+p[2]*p[2]);
    if(ptotal<0.1) continue;

    float theta=acos(p[2]/ptotal);
    eta=-1.0*log(tan(theta/2.0));

    float* gdca = globalDCA(p,v);    
    bool useTrack=true;
    useTrack = (mTCuts->goodGlobalDCA(gdca[3]) && useTrack);

    /*
    float energy=pstr->P[3][i];
    float num=energy+p[2];
    float den=energy-p[2];
    if(den!=0.) { 
       float arg=num/den;
       if(arg>0.) eta=0.5*log(arg);
    }
    */
    useTrack = (mTCuts->goodEta(eta) && useTrack);
    float phi=atan2((double)p[1], (double)p[0]);
    useTrack=(mTCuts->goodPhi(phi) && useTrack);

    if(useTrack)mrefMult++;

    useTrack=(mTCuts->goodPt(pt) && useTrack);
 
    float _r=pt/0.139;
    float yt=log(sqrt(1+_r*_r)+_r);
    useTrack = (mTCuts->goodYt(yt) && useTrack);

    mTCuts->fillHistograms(useTrack);

    eTrack->SetBx(gdca[0]);
    eTrack->SetBy(gdca[1]);
    eTrack->SetBz(gdca[2]);
    eTrack->SetBxGlobal(gdca[0]);
    eTrack->SetByGlobal(gdca[1]);
    eTrack->SetBzGlobal(gdca[2]);

    delete [] gdca;
    if(!useTrack) continue;
  
    eTrack->SetPx(p[0]);
    eTrack->SetPy(p[1]);
    eTrack->SetPz(p[2]);
    eTrack->SetEta(eta);
    eTrack->SetPhi(phi);

    if(pid<0){
      eTrack->SetCharge(-1);
    } else {
      eTrack->SetCharge(1);
    }    
    estructEvent->AddTrack(eTrack);
  }

  delete eTrack;
  return;

}    


//--------------------------------------------------------------------------
void StEStructPythia::setEventCuts(StEStructEventCuts* cuts){

  if(mECuts) delete mECuts;
  mECuts=cuts;

};

//---------------------------------------------------------------
void StEStructPythia::setTrackCuts(StEStructTrackCuts* cuts){
  if(mTCuts) delete mTCuts;
  mTCuts=cuts;
}




/**********************************************************************
 *
 * $Log: StEStructPythia.cxx,v $
 * Revision 1.3  2004/06/09 22:37:51  prindle
 * Moved some variable declarations inside comment to get rid of
 * unused variable warnings.
 * ved
 *
 * Revision 1.2  2004/03/24 21:26:52  porter
 * fixed error calculating eta as rapidity
 *
 * Revision 1.1  2003/11/21 06:24:56  porter
 * Pythia event generater as an StEStructEventReader
 *
 *
 *********************************************************************/
