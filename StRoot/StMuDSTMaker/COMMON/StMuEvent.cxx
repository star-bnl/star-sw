
/***************************************************************************
 *
 * $Id: StMuEvent.cxx,v 1.7 2004/02/17 04:56:36 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include <string.h> 
#include "StEvent/StEvent.h" 
#include "StEvent/StEventTypes.h" 
#include "StEvent/StEventSummary.h" 
#include "StEvent/StEventInfo.h" 
#include "StEvent/StDetectorState.h" 

#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuFtpcRefMult.hh"

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
  int n = (char*)mReactionPlanePtWgt - (char*)&mRefMultPos+sizeof(mReactionPlanePtWgt);
  memset(&mRefMultPos,0,n);
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
  if ( !event ) throw StMuExceptionNullPointer("no StEvent",__PRETTYF__);
  if ( !event->info() ) throw StMuExceptionNullPointer("no event info",__PRETTYF__);
  if ( !event->runInfo() ) throw StMuExceptionNullPointer("no run info",__PRETTYF__);
  if ( !event->summary() ) throw StMuExceptionNullPointer("no event summary",__PRETTYF__);
//  if (event->numberOfPrimaryVertices() != 1 ) throw StMuExceptionBadValue("!= 1 primary vertex");


  /// classes that we just copy from StEvent
  mRunInfo = *event->runInfo();
  mEventInfo = *event->info();
  mEventSummary = *event->summary();

  if ( !event->triggerDetectorCollection() ) {
    DEBUGVALUE2(event->type());
    DEBUGVALUE2(event->info()->time());
    DEBUGMESSAGE2("no trigger detector collection, creating dummy");
    mEventInfo.setTime(0);
  }
  else {
    mCtbTriggerDetector = event->triggerDetectorCollection()->ctb();
    mZdcTriggerDetector = event->triggerDetectorCollection()->zdc();
    mBbcTriggerDetector = event->triggerDetectorCollection()->bbc();
  }

  if (event->fpdCollection())
    mFpdCollection = *event->fpdCollection();
  if (event->l0Trigger())
    mL0Trigger = *event->l0Trigger();
  mL3EventSummary.fill(event);

  mTriggerIdCollection.fill( event->triggerIdCollection() ); // pointer check done inside


  mRefMultPos = uncorrectedNumberOfPositivePrimaries(*event);
  mRefMultNeg = uncorrectedNumberOfNegativePrimaries(*event); 
  mRefMultFtpcEast = uncorrectedNumberOfFtpcEastPrimaries(*event);
  mRefMultFtpcWest = uncorrectedNumberOfFtpcWestPrimaries(*event); 

//!   mReactionPlane[0] = event->mReactionPlane[0];              
//!   mReactionPlane[1] = event->mReactionPlane[1];              
//!   mReactionPlanePtWgt[0] = event->mReactionPlanePtWgt[0];              
//!   mReactionPlanePtWgt[1] = event->mReactionPlanePtWgt[1];              
} 
/***************************************************************************
 *
 * $Log: StMuEvent.cxx,v $
 * Revision 1.7  2004/02/17 04:56:36  jeromel
 * Extended help, added crs support, restored __GNUC__ for PRETTY_FUNCTION(checked once
 * more and yes, it is ONLY defined in GCC and so is __FUCTION__),  use of a consistent
 * internal __PRETTYF__, return NULL if no case selected (+message) and protected against
 * NULL mChain.
 *
 * Revision 1.6  2003/10/20 19:50:13  perev
 * workaround added for TClonesArray::Delete + some cleanup of MuEmc
 *
 * Revision 1.5  2003/07/22 19:14:41  laue
 * multiplicities for FTPC added
 *
 * Revision 1.4  2003/02/20 15:29:42  laue
 * StMuTriggerIdCollection added
 *
 * Revision 1.3  2003/02/19 13:51:58  laue
 * added the StTriggerIdCollection
 *
 * Revision 1.2  2002/08/27 19:05:57  laue
 * Minor updates to make the muDst from simulation work
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
