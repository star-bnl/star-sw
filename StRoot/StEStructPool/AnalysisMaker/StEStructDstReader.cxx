/**********************************************************************
 *
 * $Id: StEStructDstReader.cxx,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  reads Estruct dst via StEStructEventMaker class
 *
 ***********************************************************************/

#include "StEStructDstReader.h"

#include "StEStructEventCuts.h"
#include "StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructEventMaker.h"
#include "StDetectorId.h"

ClassImp(StEStructDstReader)

StEStructDstReader::StEStructDstReader(): mMaker(0), mECuts(0), mTCuts(0), mInChain(false) , mAmDone(false){};


StEStructDstReader::StEStructDstReader(StEStructEventMaker* maker, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts, bool inChain) : mAmDone(false) { 

    mInChain=inChain;
    setEventMaker(maker,inChain); 
    setEventCuts(ecuts);
    setTrackCuts(tcuts);

};

StEStructDstReader::~StEStructDstReader(){};

void StEStructDstReader::setEventMaker(StEStructEventMaker* maker, bool inChain){ 
  mInChain=inChain;
  mMaker=maker; 
};

void StEStructDstReader::setEventCuts(StEStructEventCuts* ecuts) { mECuts=ecuts; };
void StEStructDstReader::setTrackCuts(StEStructTrackCuts* tcuts) { mTCuts=tcuts; };

bool StEStructDstReader::hasMaker() { return (mMaker) ? true : false ; }
bool StEStructDstReader::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructDstReader::hasTrackCuts() { return (mTCuts) ? true : false ; }


//-------------------------------------------------------------------------
StEStructEvent* StEStructDstReader::next() {

  StEStructEvent* retVal=NULL;
  if(!mMaker) return retVal;
  if(!mInChain){
     int iret=mMaker->Make();
     if(iret){
        mAmDone=true;
        return retVal;
     }
  }

  retVal=mMaker->event();
  if(!checkEvent(retVal)){
    delete retVal;
    retVal=NULL;
  }

  return retVal;
}

//-----------------------------------------------------------
bool StEStructDstReader::checkEvent(StEStructEvent* e){

  // don't have the trigger word yet
   
  bool useEvent=true;

  if(!mECuts->goodPrimaryVertexZ(e->Vz()) ||
     !mECuts->goodCentrality(e->CentMult())) useEvent=false;

  mrefMult=getNumberOfTracks(e);
  if(!mECuts->goodNumberOfTracks(mrefMult))useEvent=false;

  mECuts->fillHistogram(mECuts->primaryVertexZName(),e->Vz(),useEvent);
  mECuts->fillHistogram(mECuts->centralityName(),e->CentMult(),useEvent);
  mECuts->fillHistogram(mECuts->numTracksName(),(float)mrefMult,useEvent);

  return useEvent;
}

//-------------------------------------------------------------------
int StEStructDstReader::getNumberOfTracks(StEStructEvent* e){

  int numPrimaries=e->Ntrack();
  if(0==numPrimaries) return 0;

  
  int numfinal=0;
  for(int i=0;i<numPrimaries;i++){

    StEStructTrack* track=(StEStructTrack*)e->Tracks()->UncheckedAt(i);

    bool useTrack=true;
    if(track->Pt()<0.15)continue;
     
    useTrack = (mTCuts->goodFlag(track->Flag()) && useTrack);
    useTrack = ( mTCuts->goodCharge(track->Charge()) && useTrack);
    useTrack = ( mTCuts->goodNFitPoints(track->NFitPoints()) && useTrack);
    useTrack = (mTCuts->goodNFitNMax((float)((float)track->NFitPoints()/(float)track->NMaxPoints())) && useTrack);
    
    useTrack = (mTCuts->goodGlobalDCA(track->DcaGlobal()) && useTrack);
    useTrack = (mTCuts->goodEta(track->Eta()) && useTrack);
    useTrack = (mTCuts->goodChi2(track->Chi2()) && useTrack);
    useTrack = (mTCuts->goodPhi(track->Phi()) && useTrack);

    if(useTrack)numfinal++;
 
    useTrack = (mTCuts->goodPt(track->Pt()) && useTrack);
    useTrack = (mTCuts->goodYt(track->Yt()) && useTrack);
    mTCuts->fillHistograms(useTrack);

  }

  return numfinal;
}


/***********************************************************************
 *
 * $Log: StEStructDstReader.cxx,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
