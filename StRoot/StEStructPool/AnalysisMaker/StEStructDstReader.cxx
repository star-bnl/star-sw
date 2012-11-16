/**********************************************************************
 *
 * $Id: StEStructDstReader.cxx,v 1.4 2012/11/16 21:19:06 prindle Exp $
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

StEStructDstReader::StEStructDstReader(): mMaker(0), mInChain(false) , mAmDone(false){};


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


bool StEStructDstReader::hasMaker() { return (mMaker) ? true : false ; }


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
     !mECuts->goodCentrality(e->Centrality())) useEvent=false;

  mECuts->fillHistogram(mECuts->primaryVertexZName(),e->Vz(),useEvent);
  mECuts->fillHistogram(mECuts->centralityName(),e->Centrality(),useEvent);

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
 * Revision 1.4  2012/11/16 21:19:06  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.3  2006/04/06 00:53:57  prindle
 *   Tried to rationalize the way centrality is defined.
 *   Now the reader gives a float to StEStructEvent and this float is
 * what is being used to define centrality. When we need a centrality
 * bin index we pass this number into the centrality singleton object.
 *
 * Revision 1.2  2006/02/22 22:03:13  prindle
 * Removed all references to multRef
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
