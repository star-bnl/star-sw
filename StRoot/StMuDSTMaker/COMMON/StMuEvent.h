/***************************************************************************
 *
 * $Id: StMuEvent.h,v 1.9 2004/05/02 04:10:14 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
#ifndef StMuEvent_h
#define StMuEvent_h

#include "TObject.h"
#include "StMuL3EventSummary.h"
#include "StMuEmcCollection.h"
#include "StMuTriggerIdCollection.h"

#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StCtbTriggerDetector.h"
#include "StEvent/StZdcTriggerDetector.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StFpdCollection.h"
#include "StEvent/StL0Trigger.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StDetectorState.h"

#include "StStrangeMuDstMaker/StStrangeMuDst.hh"
//#include "StarClassLibrary/StThreeVectorD.hh"

class StEvent;
class StMuCut;

/**
   @class StMuEvent
   The StMuEvent class holds the event-wise information of the STAR's common muDst.  
   Most of its data members are classes from the StEvent package.
   Please refer to the StEvent manual for information on these classes.
 */
class StMuEvent : public TObject {
 public:
  StMuEvent();
  StMuEvent(const StEvent*); 
  virtual ~StMuEvent();

  int eventId();
  int eventNumber();
  int runId();
  int runNumber();
  // classes taken strait from StEvent
  StRunInfo& runInfo();
  StEventInfo& eventInfo();
  StEventSummary& eventSummary();
  StCtbTriggerDetector& ctbTriggerDetector();
  StZdcTriggerDetector& zdcTriggerDetector();
  StBbcTriggerDetector& bbcTriggerDetector();
  StFpdCollection& fpdCollection(); 
  StL0Trigger& l0Trigger(); 
  // Special classes for the muDst
  StMuL3EventSummary& l3EventSummary();
  StMuTriggerIdCollection& triggerIdCollection();

  /// Reference multiplicity of positive particles as defined in StEventUtilities/StuRefMult.hh
  unsigned short refMultPos();
  /// Reference multiplicity of negative particles as defined in StEventUtilities/StuRefMult.hh
  unsigned short refMultNeg();
  /// Reference multiplicity of charged particles as defined in StEventUtilities/StuRefMult.hh
  unsigned short refMult();
  /// Reference multiplicity of particles in the east FTPC as defined in StEventUtilities/StuFtpcRefMult.hh
  unsigned short refMultFtpcEast();
  /// Reference multiplicity of particles in the west FTPC as defined in StEventUtilities/StuFtpcRefMult.hh
  unsigned short refMultFtpcWest();
  /// Reference multiplicity of particles in the east+west FTPC as defined in StEventUtilities/StuFtpcRefMult.hh
  unsigned short refMultFtpc();
  /// Currently not filled properly.
  double reactionPlane(unsigned short);
  void   setReactionPlane(unsigned short, double v);
  /// Currently not filled properly.
  double reactionPlanePtWgt(unsigned short);
  void   setReactionPlanePtWgt(unsigned short, double v);
  double magneticField();
  double zdcAdcAttentuatedSumWest();
  double zdcAdcAttentuatedSumEast();
  double ctbMultiplicity();
  ///    The StMuDst is supposed to be structured in 'physical events'.  Therefore there is only 1 primary vertex per mu event.
  StThreeVectorF primaryVertexPosition();
 protected:
  void clear();
  void fill(const StEvent*);

  // classes that we just takes from StEvent
  StRunInfo mRunInfo;
  StEventInfo mEventInfo;
  StEventSummary mEventSummary;
  StCtbTriggerDetector mCtbTriggerDetector;
  StZdcTriggerDetector mZdcTriggerDetector;
  StBbcTriggerDetector mBbcTriggerDetector;
  StFpdCollection mFpdCollection; 
  StL0Trigger mL0Trigger; 
  // special classes from MuDst
  StMuL3EventSummary mL3EventSummary;
  StMuTriggerIdCollection mTriggerIdCollection;

  UShort_t mRefMultPos;
  UShort_t mRefMultNeg;
  UShort_t mRefMultFtpcEast;
  UShort_t mRefMultFtpcWest;
  Float_t mReactionPlane[2];              
  Float_t mReactionPlanePtWgt[2];              

  friend class StMuDst;
  friend class StMuDstMaker;
  friend class StMuL3EventSummary;
  ClassDef(StMuEvent,4)
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
// special classes for muDst
inline StMuL3EventSummary& StMuEvent::l3EventSummary() {return mL3EventSummary;}
inline StMuTriggerIdCollection& StMuEvent::triggerIdCollection(){return mTriggerIdCollection;}
inline unsigned short StMuEvent::refMultPos() {return mRefMultPos;}
inline unsigned short StMuEvent::refMultNeg() {return mRefMultNeg;}
inline unsigned short StMuEvent::refMult() {return refMultPos()+refMultNeg();}
inline unsigned short StMuEvent::refMultFtpcEast() {return mRefMultFtpcEast;}
inline unsigned short StMuEvent::refMultFtpcWest() {return mRefMultFtpcWest;}
inline unsigned short StMuEvent::refMultFtpc() {return mRefMultFtpcEast+mRefMultFtpcWest;}
inline double StMuEvent::reactionPlane(unsigned short s) {return (s==0) ? mReactionPlane[0] : mReactionPlane[1];}
inline void StMuEvent::setReactionPlane(unsigned short s, double v) {(s==0) ? mReactionPlane[0]=v : mReactionPlane[1]=v;}
inline double StMuEvent::reactionPlanePtWgt(unsigned short s) {return (s==0) ? mReactionPlanePtWgt[0] : mReactionPlanePtWgt[1];}
inline void StMuEvent::setReactionPlanePtWgt(unsigned short s, double v) {(s==0) ? mReactionPlanePtWgt[0]=v : mReactionPlanePtWgt[1]=v;}
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
 * Revision 1.9  2004/05/02 04:10:14  perev
 * private => protected
 *
 * Revision 1.8  2003/07/22 19:14:40  laue
 * multiplicities for FTPC added
 *
 * Revision 1.7  2003/02/20 15:29:42  laue
 * StMuTriggerIdCollection added
 *
 * Revision 1.6  2003/02/19 13:52:11  laue
 * added the StTriggerIdCollection
 *
 * Revision 1.5  2003/01/09 18:59:46  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.4  2002/08/23 17:30:18  laue
 * additional member functions added (Helen Caines' request)
 *
 * Revision 1.3  2002/08/20 19:55:49  laue
 * Doxygen comments added
 *
 * Revision 1.2  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
