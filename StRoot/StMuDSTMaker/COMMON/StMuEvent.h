/***************************************************************************
 *
 * $Id: StMuEvent.h,v 1.37 2019/02/21 14:00:02 jdb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/
#ifndef StMuEvent_h
#define StMuEvent_h

#include "TObject.h"
#include "TArrayI.h"
#include "StMuL3EventSummary.h"
#include "StMuEmcCollection.h"
#include "StMuFmsCollection.h"
#include "StMuTriggerIdCollection.h"

#include "StEvent/StEventInfo.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventSummary.h"
#include "StEvent/StVpdTriggerDetector.h"
#include "StEvent/StMtdTriggerDetector.h"
#include "StEvent/StCtbTriggerDetector.h"
#include "StEvent/StZdcTriggerDetector.h"
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StEmcTriggerDetector.h"
#include "StEvent/StFpdTriggerDetector.h"
#include "StEvent/StFmsTriggerDetector.h"
#include "StEvent/StFpdCollection.h"
#include "StEvent/StL0Trigger.h"
#include "StEvent/StTriggerIdCollection.h"
#include "StEvent/StDetectorState.h"

#ifndef __NO_STRANGE_MUDST__
#include "StStrangeMuDstMaker/StStrangeMuDst.hh"
#endif
//#include "StarClassLibrary/StThreeVectorD.hh"

class StEvent;
class StMuCut;
class StTofCollection; // calibrated vpd for TOF
class StTriggerData;

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
  // classes taken straight from StEvent
  StRunInfo& runInfo();
  StEventInfo& eventInfo();
  StEventSummary& eventSummary();
  StVpdTriggerDetector& vpdTriggerDetector();
  StMtdTriggerDetector& mtdTriggerDetector();
  StCtbTriggerDetector& ctbTriggerDetector();
  StZdcTriggerDetector& zdcTriggerDetector();
  StBbcTriggerDetector& bbcTriggerDetector();
  StEmcTriggerDetector& emcTriggerDetector();
  StFpdTriggerDetector& fpdTriggerDetector();
  StFmsTriggerDetector& fmsTriggerDetector();
  StFpdCollection& fpdCollection(); 
  StL0Trigger& l0Trigger(); 
  // Special classes for the muDst
  StMuL3EventSummary& l3EventSummary();
  StMuTriggerIdCollection& triggerIdCollection();
  const StTriggerData* triggerData() const;

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
  unsigned short grefmult(int vtx_id=-1);
  unsigned short btofTrayMultiplicity();
  unsigned short etofHitMultiplicity();
  unsigned short etofDigiMultiplicity();
  float nearestVertexZ(int vtx_id=-1);

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
  StThreeVectorF primaryVertexPosition(int vtx_id = -1) const;
  StThreeVectorF primaryVertexErrors(int vtx_id = -1) const;
  TArrayI& L2Result(); // Raw L2Result[] array

  // Calibrated VPD info from StTofCollection in StEvent
  unsigned int numberOfVpdEastHits();  
  unsigned int numberOfVpdWestHits();
  float vpdTstart();
  float vpdTdiff(); 
  float vpdVz();

  unsigned int numberOfPxlInnerHits();
  unsigned int numberOfPxlOuterHits();
  unsigned int numberOfIstHits();
  unsigned int numberOfSsdHits();

 protected:
  void clear();
  void fill(const StEvent*);

  // classes that we just takes from StEvent
  StRunInfo mRunInfo;
  StEventInfo mEventInfo;
  StEventSummary mEventSummary;
  StVpdTriggerDetector mVpdTriggerDetector;
  StMtdTriggerDetector mMtdTriggerDetector;
  StCtbTriggerDetector mCtbTriggerDetector;
  StZdcTriggerDetector mZdcTriggerDetector;
  StBbcTriggerDetector mBbcTriggerDetector;
  StEmcTriggerDetector mEmcTriggerDetector;
  StFpdTriggerDetector mFpdTriggerDetector;
  StFmsTriggerDetector mFmsTriggerDetector;
  StFpdCollection mFpdCollection; 
  StL0Trigger mL0Trigger; 
  // special classes from MuDst
  StMuL3EventSummary mL3EventSummary;
  StMuTriggerIdCollection mTriggerIdCollection;
  StTriggerData* mTriggerData;

  UShort_t mRefMultPos;
  UShort_t mRefMultNeg;
  UShort_t mRefMultFtpcEast;
  UShort_t mRefMultFtpcWest;
  Float_t mReactionPlane[2];              
  Float_t mReactionPlanePtWgt[2];
  
  UShort_t mNHitsHFT[4];

  StThreeVectorF mPrimaryVertexError;

  TArrayI mL2Result; // Raw L2 info

  UInt_t  mVpdEast;    // VPD East Hit pattern 
  UInt_t  mVpdWest;    // VPD West Hit pattern
  Float_t mVpdTstart;  // VPD start time (calibrated)
  Float_t mVpdTdiff;   // VPD time difference (calibrated)
  Float_t mVpdVz;      // VPD vertex z

  friend class StMuDst;
  friend class StMuDstMaker;
  friend class StMuMomentumShiftMaker;
  friend class StMuL3EventSummary;
  ClassDef(StMuEvent,15)
};

inline int StMuEvent::eventId() { return mEventInfo.id();}
inline int StMuEvent::eventNumber() { return mEventInfo.id();}
inline int StMuEvent::runId() { return mEventInfo.runId();}
inline int StMuEvent::runNumber() { return mEventInfo.runId();}
inline StRunInfo& StMuEvent::runInfo() {return mRunInfo;}
inline StEventInfo& StMuEvent::eventInfo() {return mEventInfo;}
inline StEventSummary& StMuEvent::eventSummary() {return mEventSummary;}
inline StVpdTriggerDetector& StMuEvent::vpdTriggerDetector() {return mVpdTriggerDetector;}
inline StMtdTriggerDetector& StMuEvent::mtdTriggerDetector() {return mMtdTriggerDetector;}
inline StCtbTriggerDetector& StMuEvent::ctbTriggerDetector() {return mCtbTriggerDetector;}
inline StZdcTriggerDetector& StMuEvent::zdcTriggerDetector() {return mZdcTriggerDetector;}
inline StBbcTriggerDetector& StMuEvent::bbcTriggerDetector() {return mBbcTriggerDetector;}
inline StEmcTriggerDetector& StMuEvent::emcTriggerDetector() {return mEmcTriggerDetector;}
inline StFpdTriggerDetector& StMuEvent::fpdTriggerDetector() {return mFpdTriggerDetector;}
inline StFmsTriggerDetector& StMuEvent::fmsTriggerDetector() {return mFmsTriggerDetector;}
inline StFpdCollection& StMuEvent::fpdCollection() {return mFpdCollection;} 
inline StL0Trigger& StMuEvent::l0Trigger() {return mL0Trigger;} 
// special classes for muDst
inline StMuL3EventSummary& StMuEvent::l3EventSummary() {return mL3EventSummary;}
inline StMuTriggerIdCollection& StMuEvent::triggerIdCollection(){return mTriggerIdCollection;}
//inline const StTriggerData* StMuEvent::triggerData() const { if(mTriggerData!=0) {mTriggerData->setDebug(0); return mTriggerData; } else return 0; }
inline const StTriggerData* StMuEvent::triggerData() const { return mTriggerData; }
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
inline TArrayI &StMuEvent::L2Result() { return mL2Result; }
inline unsigned int StMuEvent::numberOfVpdEastHits() {  
  unsigned int num = 0;
  for(int i=0;i<32;i++) {
    num += mVpdEast>>i & 1;
  }
  return num;
}
inline unsigned int StMuEvent::numberOfVpdWestHits() {
  unsigned int num = 0;
  for(int i=0;i<32;i++) {
    num += mVpdWest>>i & 1;
  }
  return num;
}
inline float StMuEvent::vpdTstart() { return mVpdTstart; }
inline float StMuEvent::vpdTdiff() { return mVpdTdiff; }
inline float StMuEvent::vpdVz() { return mVpdVz; }
inline unsigned int StMuEvent::numberOfPxlInnerHits() { return mNHitsHFT[0]; }
inline unsigned int StMuEvent::numberOfPxlOuterHits() { return mNHitsHFT[1]; }
inline unsigned int StMuEvent::numberOfIstHits() { return mNHitsHFT[2]; }
inline unsigned int StMuEvent::numberOfSsdHits() { return mNHitsHFT[3]; }

#endif
/***************************************************************************
 *
 * $Log: StMuEvent.h,v $
 * Revision 1.37  2019/02/21 14:00:02  jdb
 * Bumped the ClassDef versions in MuDst where eTOF was added. I also added the etofTypes to the LinkDef file
 *
 * Revision 1.36  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.35  2017/04/17 20:40:56  smirnovd
 * StMuEvent: Declare getters const. They don't modify anything
 *
 * Revision 1.34  2015/07/21 22:27:35  jeromel
 * Direct use of -D and one misisng NO_STRANGE_MUDST added
 *
 * Revision 1.33  2015/07/21 20:47:46  jeromel
 * Brute force removal of include file (this include is compiled in many package + rootcint)
 *
 * Revision 1.32  2015/05/05 18:32:12  jeromel
 * mNHitsHFT added but class version not incremented - fixed
 *
 * Revision 1.31  2015/03/06 20:02:01  jdb
 * Added 4 unsigned shorts to StMuEvent at request of Xin Dong. Change StMuEvent.{h, cxx}
 *
 * Revision 1.30  2010/05/28 19:47:51  tone421
 * Removed a cout needed for test purposes in StMuDstMaker. Made sure StTriggerData objects copied into the MuDst have a debug value of 0..
 *
 * Revision 1.29  2010/05/26 17:34:59  tone421
 * Added const protection to StTriggerData* StMuEvent::triggerData()
 *
 * Revision 1.27  2010/02/03 17:16:22  tone421
 * Added function StMuEvent::nearestVertexZ(int vtx_id) which returns the z distance of the nearest vertex in relation to vertex vtx_id
 *
 * Revision 1.26  2010/02/03 04:54:45  tone421
 * Added StMuEvent::btofTrayMultiplicity() to return only TOF hits from trays. Should be looked at instead of ctbSum for run 9 and beyond.
 *
 * Revision 1.25  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 * Revision 1.24  2009/08/25 15:49:49  fine
 * fix the compilation issues under SL5_64_bits  gcc 4.3.2
 *
 * Revision 1.23  2009/01/09 19:43:47  tone421
 * OAdded gremult in StMuEvent (globals tracks with DCA < 3cm, >= 10 TPC fit hits and |eta| < 0.5)
 *
 * Revision 1.22  2008/11/18 15:34:33  tone421
 * 2 changes. The first ensures StMuEvent::primaryVertexPosition() returns the position of current vertex (set by StMuDst::setVertexIndex(Int_t vtx_id)): previously it returned the position of best ranked vertex. The second insures events with no vertex have a PVx=PYy=PYz=-999 rather than 0.
 *
 * Revision 1.21  2008/06/26 15:41:29  tone421
 *
 * Add getter and setter for vpd z vertex position
 *
 * Revision 1.20  2008/02/20 09:00:48  mvl
 * Included FMS data (StFMSTriggerDetector) (code by Akio)
 *
 * Revision 1.19  2007/09/21 02:27:12  mvl
 * Added calibrated VPD info from StTofCollection (run-8 prep)
 *
 * Revision 1.18  2007/09/05 23:21:21  mvl
 * Added StMtdTriggerDetector
 *
 * Revision 1.17  2007/08/02 20:46:46  mvl
 * Switch off Q-vector branhces in StMuDstMaker and increase version number in StMuEvent.
 * This is to avoid wranings when reading P07ib data which has Q-vector information stored with more recent libraries.
 *
 * Revision 1.16  2007/04/20 06:25:21  mvl
 * Removed Q-vectors (will implement utility class).
 * Added Vpd info.
 *
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
