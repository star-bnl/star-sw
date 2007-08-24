// -*- mode: C++ -*-

#ifndef __StGammaEvent_h__
#define __StGammaEvent_h__

#include <set>

#include "TObject.h"
#include "TVector3.h"

#include "StGammaCandidate.h"
#include "StGammaCandidateMaker.h"
//#include "StGammaIsolation.h"
//#include "StGammaDistribution.h"
//#include "StGammaFit.h"

#include "StGammaTrack.h"
#include "StGammaTower.h"
#include "StGammaStrip.h"

#include "TClonesArray.h"
#include "TObjString.h"

class StMuTrack;
class StGammaPythiaEvent;

#define TPC_VERTEX 0x0001

class StGammaEvent : public TObject {
  
 public:
  StGammaEvent();
  ~StGammaEvent(){ /* nada */ }

  void Clear(Option_t *opts="");
  UShort_t mFlags;  /// Event flags (see above)
  UShort_t flags() const { return mFlags; }

  StGammaTrack *newTrack( StMuTrack *mutr=0 );/// Add a new track
  StGammaTower *newTower();/// Add a new tower
  StGammaTower *newPreshower1();/// Add a new preshower1 (bprs) element
  StGammaTower *newPreshower2();/// Add a new preshower2 element
  StGammaTower *newPostshower();/// Add a new postshower element
  StGammaStrip *newStrip();/// Add a new SMD strip
  StGammaCandidate *newCandidate();/// Add a new gamma candidate

  Int_t numberOfTracks() const { return mTracks->GetEntriesFast(); } /// Return number of tracks
  Int_t numberOfTowers() const { return mTowers->GetEntriesFast(); } /// Return number of towers
  Int_t numberOfPreshower1() const { return mPreshower1->GetEntriesFast(); } /// Return number of pre1
  Int_t numberOfPreshower2() const { return mPreshower2->GetEntriesFast(); } /// Return number of pre2
  Int_t numberOfPostshower() const { return mPostshower->GetEntriesFast(); } /// Return number of post
  Int_t numberOfStrips() const { return mStrips->GetEntriesFast(); } /// Return number of strips
  Int_t numberOfCandidates() const { return mCandidates->GetEntriesFast(); } /// Return number of candidates

  Float_t sumPt( Float_t eta_min = -2.5, Float_t eta_max = +2.5 ) const;/// Returns track+tower pT in eta range
  Float_t sumTrackPt(Float_t eta_min = -2.5, Float_t eta_max = +2.5 ) const;/// Returns track pT in eta range
  Float_t sumTowerPt(Float_t eta_min = -2.5, Float_t eta_max = +2.5 ) const;/// Returns tower pT in eta range

  StGammaTrack *track( Int_t i ) const { return (StGammaTrack*)mTracks->At(i); }/// Return ith track
  StGammaTower *tower( Int_t i ) const { return (StGammaTower*)mTowers->At(i); }/// Return ith tower
  StGammaTower *preshower1( Int_t i ) const { return (StGammaTower*)mPreshower1->At(i); }/// Return ith pre1
  StGammaTower *preshower2( Int_t i ) const { return (StGammaTower*)mPreshower2->At(i); }/// Return ith pre2
  StGammaTower *postshower( Int_t i ) const { return (StGammaTower*)mPostshower->At(i); }/// Return ith post
  StGammaStrip *strip( Int_t i ) const { return (StGammaStrip*)mStrips->At(i); }/// Return ith strip
  StGammaCandidate *candidate( Int_t i ) const { return (StGammaCandidate*)mCandidates->At(i); }/// Return ith candidate
  
  //                                                     Run, event and vertex
 protected:
  Int_t mRunNumber;   /// Run number
  Int_t mEventNumber; /// Event number  
  set<int> mTriggerIds; /// Trigger ID's
  TObjString mMudstFileName; /// File which originated StGammaEvent
  TVector3 mVertex; /// Event vertex (TPC)
  Float_t mMagneticField; /// Magnetic field (kG)
  StGammaPythiaEvent* mPythia;


 public:
  void SetRunNumber( Int_t run ){ mRunNumber=run; }
  void SetEventNumber( Int_t event ){ mEventNumber=event; }
  void SetTriggerIds(const vector<unsigned int>& triggerIds) { copy(triggerIds.begin(), triggerIds.end(), inserter(mTriggerIds, mTriggerIds.begin())); }
  void SetMudstFileName(const TObjString &i) { mMudstFileName = i; }
  void SetVertex(const TVector3& vertex ){ mVertex=vertex; }
  void SetMagneticField( Float_t magneticField) { mMagneticField = magneticField; }
  void SetPythia(StGammaPythiaEvent* pythia) { mPythia = pythia; }

  Int_t runNumber() const { return mRunNumber; }/// Returns run number
  Int_t eventNumber() const { return mEventNumber; } /// Returns event number
  set<int>& triggerIds() { return mTriggerIds; }
  bool isTrigger(int id) const { return mTriggerIds.find(id) != mTriggerIds.end(); }
  TObjString muDstFileName() const { return mMudstFileName; } /// Returns muDst file from which event originated
  TVector3& vertex(){ return mVertex; }/// Returns the vertex
  Float_t magneticField() const { return mMagneticField; } /// Magnetic field (kG)
  StGammaPythiaEvent* pythia() { return mPythia; } /// Pythia event
  
 private:
  //                                                 Towers, tracks and strips
  Int_t InitArrays();

  TClonesArray *mTracks;    //-> array of all tracks
  TClonesArray *mTowers;    //-> array of all towers
  TClonesArray *mPreshower1;//-> array of all preshower1
  TClonesArray *mPreshower2;//-> array of all preshower2
  TClonesArray *mPostshower;//-> array of all postshower
  TClonesArray *mStrips;    //-> array of all strips
  TClonesArray *mCandidates;//-> array of all candidates

  friend Int_t StGammaCandidateMaker::Compress();
 
  ClassDef(StGammaEvent,1);

};

#endif
