
/***************************************************************************
 *
 * $Id: StarMuEvent.cxx,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include "StEvent/StEvent.h" 
#include "StEvent/StEventTypes.h" 
#include "StEvent/StEventSummary.h" 
#include "StEvent/StEventInfo.h" 
#include "StEvent/StDetectorState.h" 

#include "StEventUtilities/StuRefMult.hh"

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/StTimer.hh"

#include "StarMuException.hh"
#include "StarMuEvent.h"
#include "StarMuTrack.h"
#include "StarMuL3EventSummary.h"
#include "StarMuCut.h"
#include "StarMuDebug.h"

#include "TClonesArray.h"
#include "TObject.h"
#include "TClass.h"

ClassImp(StarMuEvent)

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StarMuEvent::StarMuEvent() {
  DEBUGMESSAGE("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StarMuEvent::StarMuEvent(const StEvent* event) {
  try {
    fill(event);
  }
  catch (StarMuException e) {
    throw e;
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StarMuEvent::~StarMuEvent(){
  DEBUGMESSAGE("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StarMuEvent::clear(){
  DEBUGMESSAGE1("");
}
/*
void StarMuEvent::fixTrackIndices() {
  ///
  ///  track list and global tracklist are filled parallel 
  /// both indices must increase
   
  StTimer timer;
  timer.start();
  int lastMatch=0;
  int matchState=-1;
  int nGlobalTracks = TCAfTracksGlobal->GetEntries();
  int nTracks = TCAfTracks->GetEntries();
  for (int igt=0; igt<nGlobalTracks; igt++) {
    StarMuTrack* gt = (StarMuTrack*)TCAfTracksGlobal->UncheckedAt(igt);
    gt->setIndex(igt); 
    matchState=0;
    for (int it=lastMatch; it<nTracks; it++) {
      StarMuTrack* t = (StarMuTrack*)TCAfTracks->UncheckedAt(it);
      if (gt->mTrackId == t->mTrackId) { 
	t->setIndex(igt); 
	lastMatch=it;
	matchState=1;
      }
      if (matchState && it>lastMatch) continue; /// break loop if not further matches are found
    }
  }
  timer.stop();
  if (StarMuDebug::mDebug) cout << "StarMuEvent::fixTrackIndices() " << timer.elapsedTime() << " sec " << endl;
}
*/
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StarMuEvent::fill(const StEvent* event){
  DEBUGMESSAGE("");
  if ( !event ) throw StarMuExceptionNullPointer("no StEvent",PF);
  if ( !event->info() ) throw StarMuExceptionNullPointer("no event info",PF);
  if ( !event->runInfo() ) throw StarMuExceptionNullPointer("no run info",PF);
  if ( !event->summary() ) throw StarMuExceptionNullPointer("no event summary",PF);
  if ( !event->triggerDetectorCollection() ) throw StarMuExceptionNullPointer("no trigger detector collection",PF);
  //  if (event->numberOfPrimaryVertices() != 1 ) throw StarMuExceptionBadValue("!= 1 primary vertex");


  /// classes that we just copy from StEvent
  mRunInfo = *event->runInfo();
  mEventInfo = *event->info();
  mEventSummary = *event->summary();
  mCtbTriggerDetector = event->triggerDetectorCollection()->ctb();
  mZdcTriggerDetector = event->triggerDetectorCollection()->zdc();
  mBbcTriggerDetector = event->triggerDetectorCollection()->bbc();
  if (event->fpdCollection())
    mFpdCollection = *event->fpdCollection();
  if (event->l0Trigger())
    mL0Trigger = *event->l0Trigger();
  mL3EventSummary.fill(event);

  mRefMultPos = uncorrectedNumberOfPositivePrimaries(*event);
  mRefMultNeg = uncorrectedNumberOfNegativePrimaries(*event); 

//!   mReactionPlane[0] = event->mReactionPlane[0];              
//!   mReactionPlane[1] = event->mReactionPlane[1];              
//!   mReactionPlanePtWgt[0] = event->mReactionPlanePtWgt[0];              
//!   mReactionPlanePtWgt[1] = event->mReactionPlanePtWgt[1];              
} 
/***************************************************************************
 *
 * $Log: StarMuEvent.cxx,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
