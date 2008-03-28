
/***************************************************************************
 *
 * $Id: StMuEvent.cxx,v 1.15 2007/09/05 23:21:21 mvl Exp $
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
#include "StMuDst.h"
#include "StMuPrimaryVertex.h"

#include "TClonesArray.h"
#include "TObject.h"
#include "TClass.h"

ClassImp(StMuEvent)

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::StMuEvent() : mPrimaryVertexError(-999.,-999.,-999) { 
  DEBUGMESSAGE("");
  int n = (char*)mReactionPlanePtWgt - (char*)&mRefMultPos+sizeof(mReactionPlanePtWgt);
  memset(&mRefMultPos,0,n);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuEvent::StMuEvent(const StEvent* event) : mPrimaryVertexError(-999.,-999.,-999.) { 
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
  const StPrimaryVertex *p_vtx=event->primaryVertex();
  if (p_vtx) {
    mPrimaryVertexError=p_vtx->positionError();
  }

  if ( !event->triggerDetectorCollection() ) {
    DEBUGVALUE2(event->type());
    DEBUGVALUE2(event->info()->time());
    DEBUGMESSAGE2("no trigger detector collection, creating dummy");
    mEventInfo.setTime(0);
  }
  else {
    mVpdTriggerDetector = event->triggerDetectorCollection()->vpd();
    mMtdTriggerDetector = event->triggerDetectorCollection()->mtd();
    mCtbTriggerDetector = event->triggerDetectorCollection()->ctb();
    mZdcTriggerDetector = event->triggerDetectorCollection()->zdc();
    mBbcTriggerDetector = event->triggerDetectorCollection()->bbc();
    mEmcTriggerDetector = event->triggerDetectorCollection()->emc();
    mFpdTriggerDetector = event->triggerDetectorCollection()->fpd();
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

  if (event->triggerData()) // MC has no triggerData
    mL2Result.Set(event->triggerData()->l2ResultLength(),(const Int_t*) event->triggerData()->l2Result());
} 

unsigned short StMuEvent::refMultPos(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultPos;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultPos();
  return 0;
}

unsigned short StMuEvent::refMultNeg(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultNeg;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultNeg();
  return 0;
}

unsigned short StMuEvent::refMult(int vtx_id) {return refMultPos(vtx_id)+refMultNeg(vtx_id);}

unsigned short StMuEvent::refMultFtpcEast(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultFtpcEast;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultFtpcEast();
  return 0;
}

unsigned short StMuEvent::refMultFtpcWest(int vtx_id) {
  // Check old format (no StMuPrimaryVertex stored)
  if (StMuDst::numberOfPrimaryVertices()==0 && (vtx_id == 0 || vtx_id == -1))
     return mRefMultFtpcWest;
  if (vtx_id == -1)
     vtx_id = StMuDst::currentVertexIndex();
  if (StMuDst::primaryVertex(vtx_id))
     return StMuDst::primaryVertex(vtx_id)->refMultFtpcWest();
  return 0;
}

unsigned short StMuEvent::refMultFtpc(int vtx_id) {return refMultFtpcEast(vtx_id)+refMultFtpcWest(vtx_id);}
/***************************************************************************
 *
 * $Log: StMuEvent.cxx,v $
 * Revision 1.15  2007/09/05 23:21:21  mvl
 * Added StMtdTriggerDetector
 *
 * Revision 1.14  2007/04/20 06:25:21  mvl
 * Removed Q-vectors (will implement utility class).
 * Added Vpd info.
 *
 * Revision 1.12  2006/09/20 17:23:39  mvl
 * Added protected for events with StTriggerData (e.g. simulation)
 *
 * Revision 1.11  2006/09/20 01:50:35  mvl
 * Added data member and code for L2Result array (TArrayI).
 *
 * Revision 1.10  2005/08/19 19:46:05  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(int i) and
 *    StMuDst::numberOfprimaryTracks().
 * 2) refMult is now stored for all vertices, in StMuPrimaryVertex. The
 *    obvious way to access these numbers is from the StMuprimaryVertex structures,
 *    but for ebakcward compatibility a function is provided in StMuEvent as well
 *    (this is the only function taht works for existing MuDst)
 *
 * As an aside, I've also changes the internals of StMuDst::createStEvent and
 * StMuDst::fixTrackIndices() to be able to deal with a larger range of index numbers for tracks as generated by Ittf.
 *
 * BIG FAT WARNING: StMudst2StEventMaker and StMuDstFilterMaker
 * do not fully support the multiple vertex functionality yet.
 *
 * Revision 1.9  2004/12/02 00:19:52  mvl
 * Added error on primary vertex
 *
 * Revision 1.8  2004/08/04 17:57:13  mvl
 * Added EMC trigger information and fpd trigger (tower) information
 *
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
