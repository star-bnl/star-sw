// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#ifndef ST_JET_CANDIDATE_H
#define ST_JET_CANDIDATE_H

#include "TLorentzVector.h"
#include "TRefArray.h"

class StJetTrack;
class StJetTower;

class StJetCandidate : public TObject {
public:
  StJetCandidate()
    : mPt(0)
    , mEta(0)
    , mPhi(0)
    , mE(0)
    , mDetEta(0)
  {
  }

  StJetCandidate(const TVector3& vertex, float pt, float eta, float phi, float E);

  TLorentzVector fourMomentum() const;
  TVector3 momentum() const;

  float pt () const { return mPt ; }
  float eta() const { return mEta; }
  float phi() const { return mPhi; }
  float E  () const { return mE  ; }
  float px () const { return momentum().Px(); }
  float py () const { return momentum().Py(); }
  float pz () const { return momentum().Pz(); }
  float detEta() const { return mDetEta; }
  float sumTrackPt() const;
  float sumTowerPt() const;
  float neutralFraction() const { return sumTowerPt() / mPt; }
  float chargedFraction() const { return sumTrackPt() / mPt; }
  StJetTrack* leadingChargedParticle() const;
  float deltaPhi(const StJetCandidate* jet) const { return momentum().DeltaPhi(jet->momentum()); }
  float deltaR(const StJetCandidate* jet) const { return momentum().DeltaR(jet->momentum()); }

  int numberOfTracks() const { return mTracks.GetEntriesFast(); }
  int numberOfTowers() const { return mTowers.GetEntriesFast(); }

  StJetTrack* track(int i) const { return (StJetTrack*)mTracks.At(i); }
  StJetTower* tower(int i) const { return (StJetTower*)mTowers.At(i); }

  StJetTrack* getTrackById(int id) const;
  StJetTower* getTowerById(int id) const;

  const TRefArray& tracks() const { return mTracks; }
  const TRefArray& towers() const { return mTowers; }

  // Utility functions to get jet patch eta and phi from jet patch id and vice-versa
  static bool getJetPatchEtaPhi(int id, float& eta, float& phi);
  static bool getJetPatchId(float eta, float phi, int& id);

  void setPtEtaPhiE(float pt, float eta, float phi, float E);
  void setPxPyPzE(float px, float py, float pz, float E);
  StJetTrack* addTrack(StJetTrack* track) { mTracks.Add((TObject*)track); return (StJetTrack*)mTracks.Last(); }
  StJetTower* addTower(StJetTower* tower) { mTowers.Add((TObject*)tower); return (StJetTower*)mTowers.Last(); }

private:
  float mPt;
  float mEta;
  float mPhi;
  float mE;
  float mDetEta;

  TRefArray mTracks;
  TRefArray mTowers;

  ClassDef(StJetCandidate, 1);
};

inline TLorentzVector StJetCandidate::fourMomentum() const
{
  TLorentzVector fourMom;
  fourMom.SetPtEtaPhiE(mPt, mEta, mPhi, mE);
  return fourMom;
}

inline TVector3 StJetCandidate::momentum() const
{
  TVector3 mom;
  mom.SetPtEtaPhi(mPt, mEta, mPhi);
  return mom;
}

inline void StJetCandidate::setPtEtaPhiE(float pt, float eta, float phi, float E)
{
  mPt  = pt;
  mEta = eta;
  mPhi = phi;
  mE   = E;
}

inline void StJetCandidate::setPxPyPzE(float px, float py, float pz, float E)
{
  TVector3 mom(px, py, pz);
  mPt  = mom.Pt();
  mEta = mom.Eta();
  mPhi = mom.Phi();
  mE   = E;
}

#endif // ST_JET_CANDIDATE_H
