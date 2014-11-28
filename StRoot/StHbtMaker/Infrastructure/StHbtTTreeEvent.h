/***************************************************************************
 *
 * $Id: StHbtTTreeEvent.h,v 1.5 2001/12/07 00:36:16 laue Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************
 *
 ***************************************************************************
 *
 * $Log: StHbtTTreeEvent.h,v $
 * Revision 1.5  2001/12/07 00:36:16  laue
 * Ouuuups, forgot to check this ones in.
 * L3 trigger algorithm added
 *
 * Revision 1.1  2001/06/21 19:15:47  laue
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
 *
 **************************************************************************/

#ifndef StHbtTTreeEvent_hh
#define StHbtTTreeEvent_hh

/* #include "StHbtMaker/Infrastructure/StHbtTypes.hh" */
/* #include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh" */
/* #include "StHbtMaker/Infrastructure/StHbtV0Collection.hh" */

#include "TObject.h"
#include "TClonesArray.h"

class StHbtEvent;
class StHbtTrack;
class StHbtV0;
class StHbtXi;
class StHbtKink;
class StHbtEventCut;
class StHbtTrackCut;
class StHbtV0Cut;
class StHbtXiCut;
class StHbtKinkCut;

class StHbtTTreeEvent : public TObject {
public:
  StHbtTTreeEvent();
  StHbtTTreeEvent(const StHbtEvent*, StHbtTrackCut*, StHbtV0Cut*, StHbtXiCut*, StHbtKinkCut*);
  virtual ~StHbtTTreeEvent();
  void clear();
  void fill(const StHbtEvent*, StHbtTrackCut*, StHbtV0Cut*, StHbtXiCut*, StHbtKinkCut*); //!
  void SetMagneticField(double);
private:
  void initClonesArrays();
  void fillEventInfo(const StHbtEvent* event);  //! 
  void addTrack(const StHbtEvent*, const StHbtTrack*); //!
  void addV0(const StHbtEvent*, const StHbtV0*); //! 
  void addXi(const StHbtEvent*, const StHbtXi*); //! 
  void addKink(const StHbtEvent*, const StHbtKink*); //!
  TClonesArray* tracks() const {return fTracks;}
  TClonesArray* v0s() const {return fV0s;}
  TClonesArray* xis() const {return fXis;}
  TClonesArray* kinks() const {return fKinks;}

  UShort_t mEventNumber;           //
  UShort_t mRunNumber;             //
  
  UInt_t mTriggerWord;
  UInt_t mTriggerActionWord;

  UShort_t mTpcNhits;              // number of TPC hits
  UShort_t mNumberOfTracks;        // total number of TPC tracks
  UShort_t mNumberOfGoodTracks;    // number of "good" tracks
  UShort_t mUncorrectedNumberOfPositivePrimaries;
  UShort_t mUncorrectedNumberOfNegativePrimaries;
  Float_t mCtbMultiplicity;       // Central Trigger Barrel
  Float_t mZdcAdc[2];             // Zero-degree calorimeter 
                                       //values east/west
  Float_t mReactionPlane[2];              
  Float_t mVertexX;
  Float_t mVertexY;
  Float_t mVertexZ;
  Float_t mMagneticField; // magnetic field in Z direction


  UShort_t       mNtracks;
  TClonesArray*        fTracks;
  static TClonesArray* fgTracks;

  UShort_t       mNv0s;
  TClonesArray*        fV0s;
  static TClonesArray* fgV0s;

  UShort_t       mNxis;
  TClonesArray*        fXis;
  static TClonesArray* fgXis;

  UShort_t       mNkinks;
  TClonesArray*        fKinks;   
  static TClonesArray* fgKinks;

  static Int_t mDebug; //! do not write this to disk
  Short_t mTrackType;  //! do not write this to disk

  Float_t mReactionPlanePtWgt[2];              
  //UInt_t mL3TriggerAlgorithm[4];
  UInt_t mL3TriggerAlgorithm;

  friend class StHbtEvent;
  friend class StHbtTrack;
  friend class StHbtV0;
  friend class StHbtXi;
  friend class StHbtKink;

  ClassDef(StHbtTTreeEvent,5)
};



#endif
