/***************************************************************************
 *
 * $Id: StHbtEvent.hh,v 1.20 2003/01/17 16:46:22 mercedes Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   HbtEvent is the "transient microDST"  Objects of this class are
 *   generated from the input data by a Reader, and then presented to
 *   the Cuts of the various active Analyses.
 *
 ***************************************************************************
 *
 * $Log: StHbtEvent.hh,v $
 * Revision 1.20  2003/01/17 16:46:22  mercedes
 * StMuEvent::refMult() added
 *
 * Revision 1.19  2002/11/19 23:27:25  renault
 * New event constructor to find V0 daughters informations(helix for average
 * separation calculation)
 *
 * Revision 1.18  2002/03/21 18:49:31  laue
 * updated for new MuDst reader
 *
 * Revision 1.17  2001/12/06 16:47:13  laue
 * l3 trigger algorithm added
 *
 * Revision 1.14  2001/06/21 19:15:45  laue
 * Modified fiels:
 *   CTH.hh : new constructor added
 *   StHbtEvent, StHbtKink, StHbtTrack : constructors from the persistent
 *                                   (TTree) classes added
 *   StHbtLikeSignAnalysis : minor changes, for debugging
 *   StHbtTypes: split into different files
 * Added files: for the new TTree muDst's
 *   StExceptions.cxx StExceptions.hh StHbtEnumeration.hh
 *   StHbtHelix.hh StHbtHisto.hh StHbtString.hh StHbtTFile.hh
 *   StHbtTTreeEvent.cxx StHbtTTreeEvent.h StHbtTTreeKink.cxx
 *   StHbtTTreeKink.h StHbtTTreeTrack.cxx StHbtTTreeTrack.h
 *   StHbtTTreeV0.cxx StHbtTTreeV0.h StHbtVector.hh
 *
 * Revision 1.13  2001/06/04 19:09:52  rcwells
 * Adding B-field, run number, and improved reaction plane functionality
 *
 * Revision 1.12  2001/05/25 23:23:59  lisa
 * Added in StHbtKink stuff
 *
 * Revision 1.11  2001/05/15 15:30:16  rcwells
 * Added magnetic field to StHbtEvent
 *
 * Revision 1.10  2000/08/31 22:31:31  laue
 * StHbtAnalysis: output changed (a little bit less)
 * StHbtEvent: new version, members for reference mult added
 * StHbtIOBinary: new IO for new StHbtEvent version
 * StHbtTypes: TTree typedef to StHbtTTree added
 * StHbtVertexAnalysis: overflow and underflow added
 *
 * Revision 1.9  2000/05/25 21:54:16  laue
 * RotateZ implemented. Rotates momentum and helix around the z axis
 *
 * Revision 1.7  2000/02/18 21:32:23  laue
 * franksTrackCut changed. If mCharge is set to '0' there will be no cut
 * on charge. This is important for front-loaded cuts.
 *
 * copy constructor implemented for StHbtEvent, StHbtTrack and StHbtV0.
 *
 * franks1HistoD.cxx franks1HistoD.h franks2HistoD.cxx franks2HistoD.h
 * removed. We can now (CC5 on Solaris) use the versions (no D)
 *
 * Revision 1.6  1999/09/16 18:47:59  lisa
 * replace placeholder HbtV0Track stuff with Helens StHbtV0 classes
 *
 * Revision 1.5  1999/09/03 22:39:15  lisa
 * Readers now MUST have Report() methods and MAY have WriteHbtEvent() methods
 *
 * Revision 1.4  1999/07/19 14:24:06  hardtke
 * modifications to implement uDST
 *
 * Revision 1.3  1999/07/06 22:33:22  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtEvent_hh
#define StHbtEvent_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StHbtMaker/Infrastructure/StHbtXiCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtKinkCollection.hh"

class StHbtTrackCut;
class StHbtV0Cut;
class StHbtXiCut;
class StHbtKinkCut;
#ifdef __ROOT__
class StHbtTTreeEvent;
class StMuDst;
#endif

class StHbtEvent{
public:
  StHbtEvent();
#ifdef __ROOT__
  StHbtEvent(const StHbtTTreeEvent* ); 
  StHbtEvent(const StMuDst*, int trackType );
  StHbtEvent(const StMuDst*, int trackType, bool readV0Daughters);
#endif
  StHbtEvent(const StHbtEvent&, StHbtTrackCut* =0, StHbtV0Cut* =0,  StHbtXiCut* =0, StHbtKinkCut* =0); // copy constructor with track and v0 cuts
  ~StHbtEvent();
  void RotateZ(const double);

  unsigned short EventNumber() const;
  int RunNumber() const;
  unsigned short CtbMult() const;
  unsigned short ZdcAdcEast() const;
  unsigned short ZdcAdcWest() const;
  int NumberOfTpcHits() const;
  unsigned short NumberOfTracks() const;
  unsigned short NumberOfGoodTracks() const;
  unsigned int UncorrectedNumberOfPositivePrimaries() const;
  unsigned int UncorrectedNumberOfNegativePrimaries() const;
  unsigned int UncorrectedNumberOfPrimaries() const;
  float ReactionPlane(const int& wgt=0) const;
  float ReactionPlaneError(const int& wgt=0) const;
  float ReactionPlaneSubEventDifference(const int& wgt=0) const;
  StHbtThreeVector PrimVertPos() const;
  StHbtV0Collection* V0Collection() const;
  StHbtXiCollection* XiCollection() const;
  StHbtKinkCollection* KinkCollection() const;
  StHbtTrackCollection* TrackCollection() const;
  double MagneticField() const;
  unsigned int TriggerWord() const;
  unsigned int TriggerActionWord() const;
  unsigned int L3TriggerAlgorithm(const unsigned int& l=0) const;

  void SetEventNumber(const unsigned short&);
  void SetRunNumber(const int&);
  void SetCtbMult(const unsigned short&);
  void SetZdcAdcEast(const unsigned short&);
  void SetZdcAdcWest(const unsigned short&);
  void SetNumberOfTpcHits(const int&);
  void SetNumberOfTracks(const unsigned short&);
  void SetNumberOfGoodTracks(const unsigned short&);
  void SetUncorrectedNumberOfPositivePrimaries(const unsigned int&);
  void SetUncorrectedNumberOfNegativePrimaries(const unsigned int&); 
  void SetUncorrectedNumberOfPrimaries(const unsigned int&);
  void SetReactionPlane(const float&,const int& wgt=0);
  void SetReactionPlaneError(const float&, const int& wgt=0);
  void SetReactionPlaneSubEventDifference(const float&, const int& wgt=0);
  void SetPrimVertPos(const StHbtThreeVector&);
  void SetMagneticField(const double&);
  void SetTriggerWord(const unsigned int&);
  void SetTriggerActionWord(const unsigned int&);
  void SetL3TriggerAlgorithm(const unsigned int&, const unsigned int&);

  // For I/O of this object -- functions defined in StHbtIO.cc
  friend ostream& operator<<(ostream& out, StHbtEvent& ev);
  friend istream& operator>>(istream& in,  StHbtEvent& ev);

private:
  unsigned short mEventNumber;           //
  unsigned short mRunNumber;
  unsigned short mCtbMultiplicity;       // Central Trigger Barrel
  unsigned short mZdcAdc[2];       // Zero-degree calorimeter 
                                         //values east/west
  int mTpcNhits;                         // number of TPC hits
  unsigned short mNumberOfTracks;     // total number of TPC tracks
  unsigned short mNumberOfGoodTracks; // number of "good" tracks
  unsigned int mUncorrectedNumberOfPositivePrimaries;
  unsigned int mUncorrectedNumberOfNegativePrimaries;
  unsigned int mUncorrectedNumberOfPrimaries;
  float mReactionPlane[2]; //reaction plane/error  //   
  float mReactionPlanePtWgt[2]; //reaction plane/error with pT weight //     
  double mMagneticField; // magnetic field in Z direction
  unsigned int mTriggerWord;
  unsigned int mTriggerActionWord;
  unsigned int mL3TriggerAlgorithm[4];

  StHbtThreeVector mPrimVertPos;
  StHbtTrackCollection* mTrackCollection;
  StHbtV0Collection* mV0Collection;
  StHbtXiCollection* mXiCollection;
  StHbtKinkCollection* mKinkCollection;

  friend class StHbtIOBinary;
  friend class StHbtTTreeEvent;
  friend class StHbtTTreeTrack;
  friend class StHbtTTreeV0;
  friend class StHbtTTreeXi;
  friend class StHbtTTreeKink;
};



#endif 
