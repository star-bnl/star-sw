/***************************************************************************
 *
 * $Id: StMuDst.cxx,v 1.1 2002/03/08 17:04:17 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StMuDst.h"

#include "StContainers.h"
#include "StEvent/StEventTypes.h"

#include "StarClassLibrary/StTimer.hh"
#include "StMuDstMaker.h"
#include "StMuTrack.h"
#include "StMuEvent.h"
#include "StMuDebug.h"
#include "TClonesArray.h"
#include "TTree.h"


TClonesArray* StMuDst::arrays[__NARRAYS__] = {0,0,0,0,0,0,0,0,0};
TClonesArray* StMuDst::strangeArrays[__NSTRANGEARRAYS__] = {0,0,0,0,0,0,0,0,0,0,0};


StMuDst::StMuDst() {
  DEBUGMESSAGE("");
  /* no-op */
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::unset() {
  for ( int i=0; i<__NARRAYS__; i++) {
    arrays[i] = 0;
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    strangeArrays[i] = 0;
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::set(StMuDstMaker* maker) {
  DEBUGMESSAGE2("");
  if (!maker) { DEBUGVALUE(maker); return;}
  for ( int i=0; i<__NARRAYS__; i++) {
    arrays[i] = maker->mArrays[i];
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    strangeArrays[i] = maker->mStrangeArrays[i];
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst:: fixTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrayx
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  /// fill an array with the indices to the global tracks as function of trackId
  int *globalIndex = new int[StMuArrays::arraySizes[muGlobal]];
  for (int i=0; i<StMuArrays::arraySizes[muGlobal]; i++) globalIndex[i]=-1;   // there must be an better way
  int nGlobals = arrays[muGlobal]->GetEntries();
  for (int i=0; i<nGlobals; i++) globalIndex[ globalTracks(i)->id() ] = i;
  /// set the indices for the primary tracks
  DEBUGVALUE2(arrays[muPrimary]->GetEntries());
  int nPrimaries = arrays[muPrimary]->GetEntries();
  for (int i=0; i<nPrimaries; i++) {
     primaryTracks(i)->setIndex2Global( globalIndex[ primaryTracks(i)->id() ] );
  }
  delete globalIndex;
  DEBUGVALUE2(timer.elapsedTime());
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StEvent* StMuDst::createStEvent() {
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  StEvent* ev = new StEvent();
  StMuEvent* mu = event(); 

  ev->setInfo( new StEventInfo(mu->eventInfo()) );
  ev->setRunInfo( new StRunInfo(mu->runInfo()) );
  ev->setSummary( new StEventSummary(mu->eventSummary()) );
  //   ev->setSoftwareMonitor(SoftwareMonitor*);
  //   ev->setTpcHitCollection(StTpcHitCollection*);
  //   ev->setFtpcHitCollection(StFtpcHitCollection*);
  //   ev->setSvtHitCollection(StSvtHitCollection*);
  //   ev->setSsdHitCollection(StSsdHitCollection*);
  //   ev->setEmcCollection(StEmcCollection*);
  //   ev->setRichCollection(StRichCollection*);
  //   ev->setTofCollection(StTofCollection*);
  ev->setFpdCollection( new StFpdCollection(mu->fpdCollection()) );
  //   ev->setTriggerDetectorCollection(StTriggerDetectorCollection*);
  ev->setL0Trigger ( new StL0Trigger(mu->l0Trigger()) );
  //   ev->setL1Trigger ( new StL0Trigger(mu->l0Trigger()) );
  ev->setL3Trigger ( new StL3Trigger() );

  StPrimaryVertex* vp  = new StPrimaryVertex();  
  ev->addPrimaryVertex(vp);
  vp->setPosition( mu->eventSummary().primaryVertexPosition() );

  /// create an array of pointers to track nodes as a function of trackId 
  StTrackNode** nodes = new StTrackNode*[StMuArrays::arraySizes[muGlobal]];
  for (int i=0; i<StMuArrays::arraySizes[muGlobal]; i++) nodes[i]=0;   // there must be an better way
  // add global tracks to tracknodes
  int nGlobals = arrays[muGlobal]->GetEntries();
  for (int i=0; i<nGlobals; i++) {
    int id = globalTracks(i)->id();
    if (nodes[id]==0) nodes[id] = new StTrackNode();
    nodes[id]->addTrack( createStGlobalTrack(globalTracks(i)) );
  }
  /// add primary tracks to tracknodes and primary vertex
  int nPrimaries = arrays[muPrimary]->GetEntries();
  for (int i=0; i<nPrimaries; i++) {
    int id = primaryTracks(i)->id();
    if (nodes[id]==0) nodes[id] = new StTrackNode();
    StPrimaryTrack* t = createStPrimaryTrack(primaryTracks(i));
    nodes[id]->addTrack( t );
    vp->addDaughter( t );
  }
  /// add all tracknodes to the event
  for (int i=0; i<StMuArrays::arraySizes[muGlobal]; i++) {
    if (nodes[i]) ev->trackNodes().push_back(nodes[i]);
  } 
  /// do the same excercise for the l3 tracks
  /// we do this later
  /// we do this later
  /// we do this later
  
  // add global tracks to tracknodes
  int nStates = arrays[muState]->GetEntries();
  for (int i=0; i<nStates; i++) {
    ev->addDetectorState(detectorStates(i));
  }
  
  DEBUGVALUE2(timer.elapsedTime());
  return ev;
}


StPrimaryTrack* StMuDst::createStPrimaryTrack(StMuTrack*) {
  StPrimaryTrack* t = new StPrimaryTrack();
  return t;
}

StGlobalTrack* StMuDst::createStGlobalTrack(StMuTrack*) {
  StGlobalTrack* t = new StGlobalTrack();
  
  return t;
}

ClassImp(StMuDst)

/***************************************************************************
 *
 * $Log: StMuDst.cxx,v $
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
