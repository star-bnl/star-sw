/***************************************************************************
 *
 * $Id: StMuDst.cxx,v 1.6 2002/05/20 18:57:18 laue Exp $
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

#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"

TClonesArray* StMuDst::arrays[__NARRAYS__] = {0,0,0,0,0,0,0,0,0};
TClonesArray* StMuDst::strangeArrays[__NSTRANGEARRAYS__] = {0,0,0,0,0,0,0,0,0,0,0,0};


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

  StStrangeEvMuDst* ev = strangeEvent();
  cout << ev <<endl;
  int nV0s = v0s()->GetEntries(); for (int i=0;i<nV0s; i++) v0s(i)->SetEvent(ev);
  int nXis = xis()->GetEntries(); for (int i=0;i<nXis; i++) xis(i)->SetEvent(ev);
  //  int nKinks = kinks()->GetEntries(); for (int i=0;i<nKinks; i++) kinks(i)->SetEvent(ev);

}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::set(TClonesArray** theArrays, TClonesArray** theStrangeArrays) {
  DEBUGMESSAGE2("");
  for ( int i=0; i<__NARRAYS__; i++) {
    arrays[i] = theArrays[i];
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    strangeArrays[i] = theStrangeArrays[i];
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst:: fixTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrayx

  if ( !(arrays[muGlobal]&&arrays[muPrimary]) ) return;
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  /// fill an array with the indices to the global tracks as function of trackId
  static int *globalIndex = new int[StMuArrays::arraySizes[muGlobal]];
  for (int i=0; i<StMuArrays::arraySizes[muGlobal]; i++) globalIndex[i]=-1;   // there must be an better way
  int nGlobals = arrays[muGlobal]->GetEntries();
  for (int i=0; i<nGlobals; i++) {
    globalIndex[ globalTracks(i)->id() ] = i;
    globalTracks(i)->setIndex2Global(i);
  }
  /// set the indices for the primary tracks
  DEBUGVALUE2(arrays[muPrimary]->GetEntries());
  int nPrimaries = arrays[muPrimary]->GetEntries();
  for (int i=0; i<nPrimaries; i++) {
     primaryTracks(i)->setIndex2Global( globalIndex[ primaryTracks(i)->id() ] );
  }
  DEBUGVALUE2(timer.elapsedTime());
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
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
    nodes[id]->addTrack( createStTrack(globalTracks(i)) );
  }
  /// add primary tracks to tracknodes and primary vertex
  int nPrimaries = arrays[muPrimary]->GetEntries();
  for (int i=0; i<nPrimaries; i++) {
    int id = primaryTracks(i)->id();
    if (nodes[id]==0) nodes[id] = new StTrackNode();
    StTrack* t = createStTrack(primaryTracks(i));
    nodes[id]->addTrack( t );
    vp->addDaughter( t );
  }
  /// add all tracknodes to the event
  for (int i=0; i<StMuArrays::arraySizes[muGlobal]; i++) {
    if (nodes[i]) ev->trackNodes().push_back(nodes[i]);
  } 
  /// do the same excercise for the l3 tracks
  /// we do this later
  /// we do this laterb
  /// we do this later
  
  // add global tracks to tracknodes
  int nStates = arrays[muState]->GetEntries();
  for (int i=0; i<nStates; i++) {
    ev->addDetectorState(detectorStates(i));
  }
  
  DEBUGVALUE2(timer.elapsedTime());
  return ev;
}

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/PhysicalConstants.h"
StTrackGeometry* StMuDst::trackGeometry(int q, StPhysicalHelixD* h) {
  static StPhysicalHelixD nullHelix;
  StHelixModel* model=0; 
  if  (nullHelix!=*h) 
    model = new StHelixModel(q, h->phase()+h->h()*pi/2, h->curvature(), h->dipAngle(), h->origin(), 
			     h->momentumAt(0,event()->runInfo().magneticField()*kilogauss), h->h());
  return model;
}

StTrack* StMuDst::createStTrack(StMuTrack* track) {
  StTrack* t;
  if (track->type() == primary) t = new StPrimaryTrack();
  if (track->type() == global) t = new StGlobalTrack();
  t->setFlag( track->flag() );

  StPhysicalHelixD helix;
  helix = track->helix(); t->setGeometry( trackGeometry( track->charge(), &helix ) );
  helix = track->outerHelix(); t->setOuterGeometry( trackGeometry( track->charge(), &helix ) );

  return t;
}


ClassImp(StMuDst)

/***************************************************************************
 *
 * $Log: StMuDst.cxx,v $
 * Revision 1.6  2002/05/20 18:57:18  laue
 * update for Christof
 *
 * Revision 1.5  2002/03/27 00:50:11  laue
 * bux fix from earlier check in
 *
 * Revision 1.4  2002/03/26 19:33:14  laue
 * minor updates
 *
 * Revision 1.3  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.2  2002/03/14 04:12:55  laue
 * bug fix: StMuL3EventSummary.cxx
 * update: StMuDst.h StMuDst.cxx
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
