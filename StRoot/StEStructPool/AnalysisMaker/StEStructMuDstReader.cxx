/**********************************************************************
 *
 * $Id: StEStructMuDstReader.cxx,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  event reader class for common MuDsts
 *               Uses the StMuDstMaker for real reading
 *
 ***********************************************************************/
#include "StEStructMuDstReader.h"

#include "StEStructEventCuts.h"
#include "StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StDetectorId.h"


StEStructMuDstReader::StEStructMuDstReader(): mMaker(0), mECuts(0), mTCuts(0), mInChain(false) , mAmDone(false){};


StEStructMuDstReader::StEStructMuDstReader(StMuDstMaker* maker, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts, bool inChain) : mAmDone(false) { 

    mInChain=inChain;
    setMuDstMaker(maker,inChain); 
    setEventCuts(ecuts);
    setTrackCuts(tcuts);

};

//-------------------------------------------------------------------------
StEStructMuDstReader::~StEStructMuDstReader(){};

void StEStructMuDstReader::setMuDstMaker(StMuDstMaker* maker, bool inChain){ 
  mInChain=inChain;
  mMaker=maker; 
};

void StEStructMuDstReader::setEventCuts(StEStructEventCuts* ecuts) { mECuts=ecuts; };
void StEStructMuDstReader::setTrackCuts(StEStructTrackCuts* tcuts) { mTCuts=tcuts; };

bool StEStructMuDstReader::hasMaker() { return (mMaker) ? true : false ; }
bool StEStructMuDstReader::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructMuDstReader::hasTrackCuts() { return (mTCuts) ? true : false ; }


//-------------------------------------------------------------------------
StEStructEvent* StEStructMuDstReader::next() {

  StEStructEvent* retVal=NULL;
  if(!mMaker) return retVal;
  if(!mInChain){
     int iret=mMaker->Make();
     if(iret){
        mAmDone=true;
        return retVal;
     }
  }
  if(!mMaker->muDst()) return retVal;
  return fillEvent();
}
     
//-------------------------------------------------------------------------
StEStructEvent* StEStructMuDstReader::fillEvent(){

 StEStructEvent* retVal=NULL;
 StMuDst* muDst=mMaker->muDst();
 StMuEvent* muEvent=muDst->event();
 if(!muEvent)    return retVal;
 
 /*  
  *
  */

 unsigned int tword=muEvent->l0Trigger().triggerWord();
 int refMult=muEvent->refMult();

 float x=muEvent->eventSummary().primaryVertexPosition().x();
 float y=muEvent->eventSummary().primaryVertexPosition().y();
 float z=muEvent->eventSummary().primaryVertexPosition().z();

 bool useEvent=true;

 if(!mECuts->goodTrigger(tword) ||
    !mECuts->goodPrimaryVertexZ(z) ||
    !mECuts->goodCentrality(refMult))useEvent=false;
 
 retVal = new StEStructEvent();

 retVal->SetEventID(muEvent->eventNumber());
 retVal->SetRunID(muEvent->runNumber());
 retVal->SetOrigMult(muEvent->eventSummary().numberOfTracks());
 retVal->SetVertex(x,y,z);
 retVal->SetCentMult((int)muEvent->refMult());
 retVal->SetCentrality((int)muEvent->refMult());
 retVal->SetZDCe((float)muEvent->zdcAdcAttentuatedSumEast());
 retVal->SetZDCw((float)muEvent->zdcAdcAttentuatedSumWest());
 retVal->SetBField((float)muEvent->magneticField());

 if(!fillTracks(retVal))useEvent=false; 
 if(!mECuts->goodNumberOfTracks(mrefMult))useEvent=false;

 mECuts->fillHistogram(mECuts->triggerWordName(),(float)tword,useEvent);
 mECuts->fillHistogram(mECuts->primaryVertexZName(),z,useEvent);
 mECuts->fillHistogram(mECuts->centralityName(),(float)refMult,useEvent);
 mECuts->fillHistogram(mECuts->numTracksName(),(float)mrefMult,useEvent);
 
 if( !useEvent){
   delete retVal;
   retVal=NULL;
 }

 if(retVal)retVal->FillChargeCollections();
 
 return retVal;
} 
 

//-------------------------------------------------------------
bool StEStructMuDstReader::fillTracks(StEStructEvent* estructEvent) {

  //
  // create a single EbyE track, check cuts,
  // fill variables, add to StEStructEvent tracks which
  // does a copy.
  //

  StMuDst* muDst=mMaker->muDst();
  int numPrimaries=muDst->primaryTracks()->GetEntries();
  if(0==numPrimaries)return false;

  StEStructTrack* eTrack = new StEStructTrack();

  mrefMult=0;
  for(int i=0; i<numPrimaries; i++){

    eTrack->SetInComplete();
    StMuTrack* track=muDst->primaryTracks(i);
    
    bool useTrack=true;
    if(track->pt()<0.15)continue;

    useTrack = ( mTCuts->goodFlag(track->flag()) && useTrack);
    useTrack = ( mTCuts->goodCharge(track->charge()) && useTrack);
    useTrack = ( mTCuts->goodNFitPoints(track->nHitsFit()) && useTrack);
    useTrack = (mTCuts->goodNFitNMax((float)((float)track->nHitsFit()/(float)track->nHitsPoss())) && useTrack);
    useTrack = (mTCuts->goodGlobalDCA(track->dcaGlobal().magnitude()) && useTrack);
    useTrack = (mTCuts->goodEta(track->eta()) && useTrack);
    useTrack = (mTCuts->goodChi2(track->chi2()) && useTrack);
    useTrack = (mTCuts->goodPhi(track->phi()) && useTrack);

    /*    int pid=0;
    int pidMask=15;

    if( mTCuts->goodElectron(track->nSigmaElectron()) )pid=1<<0;
    if( mTCuts->goodPion(track->nSigmaPion()) ) pid=1<<1;
    if( mTCuts->goodKaon(track->nSigmaKaon()) ) pid=1<<2;
    if( mTCuts->goodProton(track->nSigmaProton()) ) pid=1<<3;

    useTrack = ( useTrack && (pid & pidMask) ); // any good PID we keep
    */
    //quick addition of electron removal...

    //  float p=(track->p()).mag();
    // if(p>0.18&&p<0.45&&abs(track->nSigmaElectron())<1.5)useTrack=false;

    if(useTrack)mrefMult++;

    useTrack = (mTCuts->goodPt(track->pt()) && useTrack);
    float _r=track->pt()/0.139;
    float yt=log(sqrt(1+_r*_r)+_r);
    useTrack = (mTCuts->goodYt(yt) && useTrack);

    mTCuts->fillHistograms(useTrack);

    if(useTrack) { 
      fillEStructTrack(eTrack,track);
      estructEvent->AddTrack(eTrack);
     }  

  }

  delete eTrack;
  return true;
}

//-------------------------------------------------------------------------
void StEStructMuDstReader::fillEStructTrack(StEStructTrack* eTrack,StMuTrack* mTrack){

  StThreeVectorF p=mTrack->p();  
  eTrack->SetPx(p.x());
  eTrack->SetPy(p.y());
  eTrack->SetPz(p.z());

  StThreeVectorF b=mTrack->dca();
  eTrack->SetBx(b.x());
  eTrack->SetBy(b.y());
  eTrack->SetBz(b.z());

  StThreeVectorF gb=mTrack->dcaGlobal();
  eTrack->SetBxGlobal(gb.x());
  eTrack->SetByGlobal(gb.y());
  eTrack->SetBzGlobal(gb.z());

  eTrack->SetEta(mTrack->eta());
  eTrack->SetPhi(mTrack->phi());

  eTrack->SetDedx(mTrack->dEdx());
  eTrack->SetChi2(mTrack->chi2());

  //
  // -> note in my analysis I chose nSigma instead of prob.
  //
  eTrack->SetPIDe(mTrack->nSigmaElectron()); 
  eTrack->SetPIDpi(mTrack->nSigmaPion());
  eTrack->SetPIDp(mTrack->nSigmaProton());
  eTrack->SetPIDk(mTrack->nSigmaKaon());
  eTrack->SetPIDd(10000.);

  eTrack->SetNFitPoints(mTrack->nHitsFit());
  eTrack->SetNFoundPoints(mTrack->nHits());
  eTrack->SetNMaxPoints(mTrack->nHitsPoss());

  StTrackTopologyMap map = mTrack->topologyMap();

  eTrack->SetTopologyMapTPCNHits(map.numberOfHits(kTpcId));
  eTrack->SetTopologyMapData(0,map.data(0));
  eTrack->SetTopologyMapData(1,map.data(1));

  eTrack->SetDetectorID(1); //TPC
  eTrack->SetFlag(mTrack->flag());
  eTrack->SetCharge(mTrack->charge());

}; 
  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.cxx,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
