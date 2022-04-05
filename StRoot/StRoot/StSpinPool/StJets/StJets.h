// -*- mode: c++;-*-
// $Id: StJets.h,v 1.4 2010/06/30 17:51:58 pibero Exp $
#ifndef StJets_h
#define StJets_h

#include <vector>
using std::vector;

#include "TObject.h"
#include "TClonesArray.h"

#include "TrackToJetIndex.h"
#include "TowerToJetIndex.h"

class StJet;

/*!
  \class StJets
  \author T.Henry (Texas A&M)
  StJets persistently encapsulates the event-wise results of a given jet algorithm.  That is,
  it stores a container of StJet objects.  Additionally, it also stores some information
  to persistently store  the parent-daughter relationsip between jets and tracks.
 */
class StJets : public TObject
{
public:

  StJets();
  virtual ~StJets();
    
  void Clear(bool clearTracks = false);
  void Clear(Option_t* opt);
        
  ///Set the BEMC corrupt flag.  true --> event is corrupt, no jet finding was performed
  void setBemcCorrupt(bool v) { mCorrupt = v; }
  bool bemcCorrupt() const { return mCorrupt; }

  ///The number of jets found in this event
  int nJets() const { return mJets->GetEntriesFast(); }

  ///Access to the jets in this event.
  TClonesArray* jets() { return mJets; }

  void addJet(StJet& jet);

  ///The track/tower to jet indices TClonesArray: this contains _all_ the 4momenta contained in jets for jet finding!  This is for expert use only
  TClonesArray* tracks() { return mTrackToJetIndices; }
  TClonesArray* towers() { return mTowerToJetIndices; }

  void addTrackToIndex(TrackToJetIndex& t2j);
  void addTowerToIndex(TowerToJetIndex& t2j);
    
  ///Here's how you get the 4-momenta of a track/tower in a given jet.  This contains tracks and energy-corrected-towers.  Use this for Frag. Function
  TObjArray tracks(int jetIndex) const;
  TObjArray towers(int jetIndex) const;
  vector<TLorentzVector*> particles(int jetIndex) const; // for backward compatibility

  ///access to event numbers, used to synchronize with StMuDstMaker for simultaneous reading
  int eventId    () const { return mEventId    ; }
  int eventNumber() const { return mEventNumber; }
  int runId      () const { return mRunId      ; }
  int runNumber  () const { return mRunNumber  ; }

  void seteventId    (int v) { mEventId     = v; }
  void seteventNumber(int v) { mEventNumber = v; }
  void setrunId      (int v) { mRunId       = v; }
  void setrunNumber  (int v) { mRunNumber   = v; }

  ///A double check, used to synchronize with StMuDstMaker for simultaneous reading
  //    bool isSameEvent(const StMuDst*);

  ///Number of towers with e>0.4 GeV (after status check)
  int nDylanPoints() const { return mDylanPoints; }
  void setDylanPoints(int v) { mDylanPoints = v; }

  ///Summed energy of towers with e>0.4 (after status check)
  double sumEmcE() const { return mSumEmcE; }
  void setSumEmcE(double v) { mSumEmcE = v; }

  ///User Interface as per Thomas H's request.  Access jet kinematics based on index:
  double e     (int jetIndex) const;
  double et    (int jetIndex) const;
  double p     (int jetIndex) const;
  double pt    (int jetIndex) const;
  double phi   (int jetIndex) const;
  double eta   (int jetIndex) const;
  int    nCell (int jetIndex) const;
  int    charge(int jetIndex) const;

private:
  int    mDylanPoints;
  double mSumEmcE;
  int    mEventId;
  int    mEventNumber;
  int    mRunId;
  int    mRunNumber;
  bool   mCorrupt;

  bool inBounds(int);
  StJet* jet(int);
    
  TClonesArray* mJets;
  TClonesArray* mTrackToJetIndices;
  TClonesArray* mTowerToJetIndices;
    
  ClassDef(StJets,2);
};

//inlines
inline void StJets::Clear(Option_t* opt)
{
  TObject::Clear(opt);
  mEventId = mEventNumber = mRunId = mRunNumber = 0;
  mCorrupt = false;
}

#endif // StJets_h
