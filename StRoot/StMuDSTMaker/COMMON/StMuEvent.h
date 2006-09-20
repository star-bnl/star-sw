/***************************************************************************
 *
 * $Id: StMuEvent.h,v 1.14 2006/09/20 01:50:35 mvl Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
#ifndef StMuEvent_h
#define StMuEvent_h

#include "TObject.h"
#include "TArrayI.h"
#include "StMuL3EventSummary.h"
#include "StMuEmcCollection.h"
#include "StMuTriggerIdCollection.h"

#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StCtbTriggerDetector.h"
#include "StEvent/StZdcTriggerDetector.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StEmcTriggerDetector.h"
#include "StEvent/StFpdTriggerDetector.h"
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
  StEmcTriggerDetector& emcTriggerDetector();
  StFpdTriggerDetector& fpdTriggerDetector();
  StFpdCollection& fpdCollection(); 
  StL0Trigger& l0Trigger(); 
  // Special classes for the muDst
  StMuL3EventSummary& l3EventSummary();
  StMuTriggerIdCollection& triggerIdCollection();

  /// Reference multiplicity of positive particles as defined in StEventUtilities/StuRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  unsigned short refMultPos(int vtx_id = -1);
  /// Reference multiplicity of negative particles as defined in StEventUtilities/StuRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  unsigned short refMultNeg(int vtx_id = -1);
  /// Reference multiplicity of charged particles as defined in StEventUtilities/StuRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  unsigned short refMult(int vtx_id = -1);
  /// Reference multiplicity of particles in the east FTPC as defined in StEventUtilities/StuFtpcRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  unsigned short refMultFtpcEast(int vtx_id = -1);
  /// Reference multiplicity of particles in the west FTPC as defined in StEventUtilities/StuFtpcRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  unsigned short refMultFtpcWest(int vtx_id = -1);
  /// Reference multiplicity of particles in the east+west FTPC as defined in StEventUtilities/StuFtpcRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  unsigned short refMultFtpc(int vtx_id = -1);
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
  StThreeVectorF primaryVertexErrors();
  TArrayI& L2Result(); // Raw L2Result[] array

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
  StEmcTriggerDetector mEmcTriggerDetector;
  StFpdTriggerDetector mFpdTriggerDetector;
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

  StThreeVectorF mPrimaryVertexError;

  TArrayI mL2Result; // Raw L2 info
  friend class StMuDst;
  friend class StMuDstMaker;
  friend class StMuMomentumShiftMaker;
  friend class StMuL3EventSummary;
  ClassDef(StMuEvent,7)
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
inline StEmcTriggerDetector& StMuEvent::emcTriggerDetector() {return mEmcTriggerDetector;}
inline StFpdTriggerDetector& StMuEvent::fpdTriggerDetector() {return mFpdTriggerDetector;}
inline StFpdCollection& StMuEvent::fpdCollection() {return mFpdCollection;} 
inline StL0Trigger& StMuEvent::l0Trigger() {return mL0Trigger;} 
// special classes for muDst
inline StMuL3EventSummary& StMuEvent::l3EventSummary() {return mL3EventSummary;}
inline StMuTriggerIdCollection& StMuEvent::triggerIdCollection(){return mTriggerIdCollection;}
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
inline StThreeVectorF StMuEvent::primaryVertexErrors() { return mPrimaryVertexError;}
inline TArrayI &StMuEvent::L2Result() { return mL2Result; }

#endif
/***************************************************************************
 *
 * $Log: StMuEvent.h,v $
 * Revision 1.14  2006/09/20 01:50:35  mvl
 * Added data member and code for L2Result array (TArrayI).
 *
 * Revision 1.13  2005/08/19 19:46:05  mvl
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
 * Revision 1.12  2005/03/17 21:55:00  mvl
 * Added StMuMomentumShiftMaker for applying a magnetic field scaling to the reconstructed MuDst. This class accesses StMuTrack, StMuEvent and StMuHelix and some Strangeness MuDst data members as 'friend'
 *
 * Revision 1.11  2004/12/02 00:19:52  mvl
 * Added error on primary vertex
 *
 * Revision 1.10  2004/08/04 17:57:13  mvl
 * Added EMC trigger information and fpd trigger (tower) information
 *
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
