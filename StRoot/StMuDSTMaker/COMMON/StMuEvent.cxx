
/***************************************************************************
 *
 * $Id: StMuEvent.cxx,v 1.1 2002/03/08 17:04:17 laue Exp $
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

#include "StMuException.hh"
#include "StMuEvent.h"
#include "StMuTrack.h"
#include "StMuL3EventSummary.h"
#include "StMuCut.h"
#include "StMuDebug.h"

#include "TClonesArray.h"
#include "TObject.h"
#include "TClass.h"

ClassImp(StMuEvent)

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::StMuEvent() {
  DEBUGMESSAGE("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::StMuEvent(const StEvent* event) {
  try {
    fill(event);
  }
  catch (StMuException e) {
    throw e;
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::~StMuEvent(){
  DEBUGMESSAGE("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuEvent::clear(){
  DEBUGMESSAGE1("");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuEvent::fill(const StEvent* event){
  DEBUGMESSAGE("");
  if ( !event ) throw StMuExceptionNullPointer("no StEvent",PF);
  if ( !event->info() ) throw StMuExceptionNullPointer("no event info",PF);
  if ( !event->runInfo() ) throw StMuExceptionNullPointer("no run info",PF);
  if ( !event->summary() ) throw StMuExceptionNullPointer("no event summary",PF);
  if ( !event->triggerDetectorCollection() ) throw StMuExceptionNullPointer("no trigger detector collection",PF);
  //  if (event->numberOfPrimaryVertices() != 1 ) throw StMuExceptionBadValue("!= 1 primary vertex");


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
 * $Log: StMuEvent.cxx,v $
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
