/***************************************************************************
 *
 * $Id: StMuEvent.h,v 1.2 2002/03/20 16:04:11 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
#ifndef StMuEvent_h
#define StMuEvent_h

#include "TObject.h"
#include "StMuL3EventSummary.h"

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
//#include "StarClassLibrary/StThreeVectorD.hh"

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

class StMuCut;

class StMuEvent : public TObject {
 public:
  StMuEvent();
  StMuEvent(const StEvent*); 
  virtual ~StMuEvent();

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
  StMuL3EventSummary& l3EventSummary();

  unsigned short refMultPos();
  unsigned short refMultNeg();
  unsigned short refMult();
  double reactionPlane(unsigned short);
  double reactionPlanePtWgt(unsigned short);
  double magneticField();
  double zdcAdcAttentuatedSumWest();
  double zdcAdcAttentuatedSumEast();
  double ctbMultiplicity();
  StThreeVectorF primaryVertexPosition();
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
  StMuL3EventSummary mL3EventSummary;

  UShort_t mRefMultPos;
  UShort_t mRefMultNeg;
  Float_t mReactionPlane[2];              
  Float_t mReactionPlanePtWgt[2];              

  friend class StMuDst;
  friend class StMuDstMaker;
  friend class StMuL3EventSummary;
  ClassDef(StMuEvent,2)
};

inline int StMuEvent::eventId() { return mEventInfo.id();}
inline int StMuEvent::eventNumber() { return mEventInfo.id();}
inline int StMuEvent::runId() { return mEventInfo.runId();}
inline int StMuEvent::runNumber() { return mEventInfo.runId();}
inline StRunInfo& StMuEvent::runInfo() {return mRunInfo;}
inline StEventInfo& StMuEvent::eventInfo() {return mEventInfo;}
inline StEventSummary& StMuEvent::eventSummary() {return mEventSummary;}
inline StCtbTriggerDetector& StMuEvent::ctbTriggerDetector() {return mCtbTriggerDetector;}
inline StZdcTriggerDetector& StMuEvent::zdcTriggerDetector() {return mZdcTriggerDetector;}
inline StBbcTriggerDetector& StMuEvent::bbcTriggerDetector() {return mBbcTriggerDetector;}
inline StFpdCollection& StMuEvent::fpdCollection() {return mFpdCollection;} 
inline StL0Trigger& StMuEvent::l0Trigger() {return mL0Trigger;} 
inline StMuL3EventSummary& StMuEvent::l3EventSummary() {return mL3EventSummary;}
inline unsigned short StMuEvent::refMultPos() {return mRefMultPos;}
inline unsigned short StMuEvent::refMultNeg() {return mRefMultNeg;}
inline unsigned short StMuEvent::refMult() {return refMultPos()+refMultNeg();}
inline double StMuEvent::reactionPlane(unsigned short s) {return (s==0) ? mReactionPlane[0] : mReactionPlane[1];}
inline double StMuEvent::reactionPlanePtWgt(unsigned short s) {return (s==0) ? mReactionPlanePtWgt[0] : mReactionPlanePtWgt[1];}
inline double StMuEvent::magneticField() { return mEventSummary.magneticField();}
inline double StMuEvent::zdcAdcAttentuatedSumWest() { return mZdcTriggerDetector.adc(10);}
inline double StMuEvent::zdcAdcAttentuatedSumEast() { return mZdcTriggerDetector.adc(13);}
inline double StMuEvent::ctbMultiplicity() { 
  double ctb=0;
  for (unsigned int slat = 0; slat < mCtbTriggerDetector.numberOfSlats(); slat++) {
    for (unsigned int tray = 0; tray < mCtbTriggerDetector.numberOfTrays(); tray++) {
      ctb += mCtbTriggerDetector.mips(tray,slat,0);
    }
  }
  return ctb;
}
inline StThreeVectorF StMuEvent::primaryVertexPosition() { return mEventSummary.primaryVertexPosition();}

#endif
/***************************************************************************
 *
 * $Log: StMuEvent.h,v $
 * Revision 1.2  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
