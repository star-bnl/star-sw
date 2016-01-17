/***************************************************************************
 *
 * $Id: StMuEvent.h,v 1.34 2015/07/21 22:27:35 jeromel Exp $
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
#include "StMuPrimaryVertex.h"
#include "StMuTrack.h"
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
#include "StDetectorDbMaker/St_trigDetSumsC.h"

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

  Int_t eventId()                               { return mEventInfo.id();}   
  Int_t eventNumber() 				{ return mEventInfo.id();}   
  Int_t runId()       				{ return mEventInfo.runId();}
  Int_t runNumber()   				{ return mEventInfo.runId();}
  // classes taken straight from StEvent
  StRunInfo& runInfo()                          { return mRunInfo;}
  StEventInfo& eventInfo()                      { return mEventInfo;} 
  StEventSummary& eventSummary()                { return mEventSummary;}
  StVpdTriggerDetector& vpdTriggerDetector()    { return mVpdTriggerDetector;}
  StMtdTriggerDetector& mtdTriggerDetector()	{ return mMtdTriggerDetector;}
  StCtbTriggerDetector& ctbTriggerDetector()	{ return mCtbTriggerDetector;}
  StZdcTriggerDetector& zdcTriggerDetector()	{ return mZdcTriggerDetector;}
  StBbcTriggerDetector& bbcTriggerDetector()	{ return mBbcTriggerDetector;}
  StEmcTriggerDetector& emcTriggerDetector()	{ return mEmcTriggerDetector;}
  StFpdTriggerDetector& fpdTriggerDetector()	{ return mFpdTriggerDetector;}
  StFmsTriggerDetector& fmsTriggerDetector()	{ return mFmsTriggerDetector;}
  StFpdCollection& fpdCollection()  		{ return mFpdCollection;}     
  StL0Trigger& l0Trigger() 			{ return mL0Trigger;}         
  // Special classes for the muDst
  StMuL3EventSummary& l3EventSummary()          { return mL3EventSummary;}
  StMuTriggerIdCollection& triggerIdCollection(){ return mTriggerIdCollection;}
  const StTriggerData* triggerData() const      { return mTriggerData; }

  /// Reference multiplicity of positive particles as defined in StEventUtilities/StuRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  UShort_t refMultPos(Int_t vtx_id = -1);
  /// Reference multiplicity of negative particles as defined in StEventUtilities/StuRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  UShort_t refMultNeg(Int_t vtx_id = -1);
  /// Reference multiplicity of charged particles as defined in StEventUtilities/StuRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  UShort_t refMult(Int_t vtx_id = -1);
  /// Reference multiplicity of particles in the east FTPC as defined in StEventUtilities/StuFtpcRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  UShort_t refMultFtpcEast(Int_t vtx_id = -1);
  /// Reference multiplicity of particles in the west FTPC as defined in StEventUtilities/StuFtpcRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  UShort_t refMultFtpcWest(Int_t vtx_id = -1);
  /// Reference multiplicity of particles in the east+west FTPC as defined in StEventUtilities/StuFtpcRefMult.hh for vertex vtx_id (-1 is default index from StMuDst)
  UShort_t refMultFtpc(Int_t vtx_id = -1);
  UShort_t grefmult(Int_t vtx_id=-1);
  UShort_t btofTrayMultiplicity();
  Float_t nearestVertexZ(Int_t vtx_id=-1);

	/// Currently not filled properly.
  Double_t reactionPlane(UShort_t s)            { return (s==0) ? mReactionPlane[0] : mReactionPlane[1];}
  void   setReactionPlane(UShort_t s, Double_t v){(s==0) ? mReactionPlane[0]=v : mReactionPlane[1]=v;}
  /// Currently not filled properly.
  Double_t reactionPlanePtWgt(UShort_t s)       { return (s==0) ? mReactionPlanePtWgt[0] : mReactionPlanePtWgt[1];}
  void   setReactionPlanePtWgt(UShort_t s, Double_t v) {(s==0) ? mReactionPlanePtWgt[0]=v : mReactionPlanePtWgt[1]=v;}
  Double_t magneticField()                      { return mEventSummary.magneticField();}
  Double_t zdcAdcAttentuatedSumWest()           { return mZdcTriggerDetector.adc(10);}
  Double_t zdcAdcAttentuatedSumEast()           { return mZdcTriggerDetector.adc(13);}
  Double_t ctbMultiplicity() { 
    Double_t ctb=0;
    for (UInt_t slat = 0; slat < mCtbTriggerDetector.numberOfSlats(); slat++) {
      for (UInt_t tray = 0; tray < mCtbTriggerDetector.numberOfTrays(); tray++) {
	ctb += mCtbTriggerDetector.mips(tray,slat,0);
      }
    }
    return ctb;
  }

  ///    The StMuDst is supposed to be structured in 'physical events'.  Therefore there is only 1 primary vertex per mu event.
  StThreeVectorF primaryVertexPosition(Int_t vtx_id = -1);
  StThreeVectorF primaryVertexErrors(Int_t vtx_id = -1);
  TArrayI& L2Result()                           { return mL2Result; }

  // Calibrated VPD info from StTofCollection in StEvent
  UInt_t numberOfVpdEastHits()  {  
    UInt_t num = 0;
    for(Int_t i=0;i<32;i++) {
      num += mVpdEast>>i & 1;
    }
    return num;
  }
  UInt_t numberOfVpdWestHits() {
    UInt_t num = 0;
    for(Int_t i=0;i<32;i++) {
      num += mVpdWest>>i & 1;
    }
    return num;
  }
  Float_t vpdTstart()                           { return mVpdTstart;}
  Float_t vpdTdiff()                            { return mVpdTdiff;}
  Float_t vpdVz()                               { return mVpdVz; }
  const trigDetSums_st& trigDetSums() const     { return *&mTrigDetSums;}

  UInt_t numberOfPxlInnerHits()                 { return mNHitsHFT[0]; }
  UInt_t numberOfPxlOuterHits()			{ return mNHitsHFT[1]; }
  UInt_t numberOfIstHits()			{ return mNHitsHFT[2]; }
  UInt_t numberOfSsdHits()			{ return mNHitsHFT[3]; }



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
  trigDetSums_st mTrigDetSums; // RICH scalers
  friend class StMuDst;
  friend class StMuDstMaker;
  friend class StMuMomentumShiftMaker;
  friend class StMuL3EventSummary;
  ClassDef(StMuEvent,16)
};

#endif
/***************************************************************************
 *
 * $Log: StMuEvent.h,v $
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
 * Added 4 UShort_ts to StMuEvent at request of Xin Dong. Change StMuEvent.{h, cxx}
 *
 * Revision 1.30  2010/05/28 19:47:51  tone421
 * Removed a cout needed for test purposes in StMuDstMaker. Made sure StTriggerData objects copied into the MuDst have a debug value of 0..
 *
 * Revision 1.29  2010/05/26 17:34:59  tone421
 * Added const protection to StTriggerData* StMuEvent::triggerData()
 *
 * Revision 1.27  2010/02/03 17:16:22  tone421
 * Added function StMuEvent::nearestVertexZ(Int_t vtx_id) which returns the z distance of the nearest vertex in relation to vertex vtx_id
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
 *    This also affects StMuDst::primaryTracks(Int_t i) and
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
