/***************************************************************************
 *
 * $Id: StarMuEvent.h,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
#ifndef StarMuEvent_h
#define StarMuEvent_h

#include "TObject.h"
#include "StarMuL3EventSummary.h"

#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StCtbTriggerDetector.h"
#include "StEvent/StZdcTriggerDetector.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StFpdCollection.h"
#include "StEvent/StL0Trigger.h"
#include "StEvent/StDetectorState.h"

#include "StStrangeMuDstMaker/StStrangeMuDst.hh"

class StEvent;
/* class StTrack; */
/* class StV0MuDst; */
/* class StXiMuDst; */
/* class StV0Vertex; */
/* class StXiVertex; */
/* class StKinkVertex; */
/* class StSPtrVecTrackNode; */
/* class StTrackNode; */
/* class StRichSpectra; */
/* class StDetectorState; */

class StarMuCut;

class StarMuEvent : public TObject {
 public:
  StarMuEvent();
  StarMuEvent(const StEvent*); 
  virtual ~StarMuEvent();

  int eventId();
  int eventNumber();
  int runId();
  int runNumber();
  StRunInfo& runInfo();
  StEventInfo& eventInfo();
  StEventSummary& eventSummary();
  StCtbTriggerDetector& ctbTriggerDetector();
  StZdcTriggerDetector& zdcTriggerDetector();
  StBbcTriggerDetector& bbcTriggerDetector();
  StFpdCollection& fpdCollection(); 
  StL0Trigger& l0Trigger(); 
  StarMuL3EventSummary& l3EventSummary();
  UShort_t refMultPos();
  UShort_t refMultNeg();
  UShort_t refMult();
  Float_t reactionPlane(unsigned short);
  Float_t reactionPlanePtWgt(unsigned short);
  
 private:
  void clear();
  void fill(const StEvent*);

  /// classes that we just takes from StEvent
  StRunInfo mRunInfo;
  StEventInfo mEventInfo;
  StEventSummary mEventSummary;
  StCtbTriggerDetector mCtbTriggerDetector;
  StZdcTriggerDetector mZdcTriggerDetector;
  StBbcTriggerDetector mBbcTriggerDetector;
  StFpdCollection mFpdCollection; 
  StL0Trigger mL0Trigger; 
  StarMuL3EventSummary mL3EventSummary;

  UShort_t mRefMultPos;
  UShort_t mRefMultNeg;
  Float_t mReactionPlane[2];              
  Float_t mReactionPlanePtWgt[2];              

  friend class StarMuDst;
  friend class StarMuDstMaker;
  friend class StarMuL3EventSummary;
  ClassDef(StarMuEvent,2)
};

inline int StarMuEvent::eventId() { return mEventInfo.id();}
inline int StarMuEvent::eventNumber() { return mEventInfo.id();}
inline int StarMuEvent::runId() { return mEventInfo.runId();}
inline int StarMuEvent::runNumber() { return mEventInfo.runId();}
inline StRunInfo& StarMuEvent::runInfo() {return mRunInfo;}
inline StEventInfo& StarMuEvent::eventInfo() {return mEventInfo;}
inline StEventSummary& StarMuEvent::eventSummary() {return mEventSummary;}
inline StCtbTriggerDetector& StarMuEvent::ctbTriggerDetector() {return mCtbTriggerDetector;}
inline StZdcTriggerDetector& StarMuEvent::zdcTriggerDetector() {return mZdcTriggerDetector;}
inline StBbcTriggerDetector& StarMuEvent::bbcTriggerDetector() {return mBbcTriggerDetector;}
inline StFpdCollection& StarMuEvent::fpdCollection() {return mFpdCollection;} 
inline StL0Trigger& StarMuEvent::l0Trigger() {return mL0Trigger;} 
inline StarMuL3EventSummary& StarMuEvent::l3EventSummary() {return mL3EventSummary;}
inline UShort_t StarMuEvent::refMultPos() {return mRefMultPos;}
inline UShort_t StarMuEvent::refMultNeg() {return mRefMultNeg;}
inline UShort_t StarMuEvent::refMult() {return refMultPos()+refMultNeg();}
inline Float_t StarMuEvent::reactionPlane(unsigned short s) {return (s==0) ? mReactionPlane[0] : mReactionPlane[1];}
inline Float_t StarMuEvent::reactionPlanePtWgt(unsigned short s) {return (s==0) ? mReactionPlanePtWgt[0] : mReactionPlanePtWgt[1];;}


#endif
/***************************************************************************
 *
 * $Log: StarMuEvent.h,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
