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

  StJetCandidate(const TVector3& vertex, double pt, double eta, double phi, double E);

  TLorentzVector fourMomentum() const;
  TVector3 momentum() const;

  double pt () const { return mPt ; }
  double eta() const { return mEta; }
  double phi() const { return mPhi; }
  double E  () const { return mE  ; }
  double px () const { return momentum().Px(); }
  double py () const { return momentum().Py(); }
  double pz () const { return momentum().Pz(); }
  double detEta() const { return mDetEta; }
  double sumTrackPt() const;
  double sumTowerPt() const;
  double neutralFraction() const { return sumTowerPt() / mPt; }
  double chargedFraction() const { return sumTrackPt() / mPt; }
  StJetTrack* leadingChargedParticle() const;

  int numberOfTracks() const { return mTracks.GetEntriesFast(); }
  int numberOfTowers() const { return mTowers.GetEntriesFast(); }

  StJetTrack* track(int i) const { return (StJetTrack*)mTracks.At(i); }
  StJetTower* tower(int i) const { return (StJetTower*)mTowers.At(i); }

  const TRefArray& tracks() const { return mTracks; }
  const TRefArray& towers() const { return mTowers; }

  void setPtEtaPhiE(double pt, double eta, double phi, double E);
  void setPxPyPzE(double px, double py, double pz, double E);
  void addTrack(StJetTrack* track) { mTracks.Add((TObject*)track); }
  void addTower(StJetTower* tower) { mTowers.Add((TObject*)tower); }

private:
  double mPt;
  double mEta;
  double mPhi;
  double mE;
  double mDetEta;

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

inline void StJetCandidate::setPtEtaPhiE(double pt, double eta, double phi, double E)
{
  mPt  = pt;
  mEta = eta;
  mPhi = phi;
  mE   = E;
}

inline void StJetCandidate::setPxPyPzE(double px, double py, double pz, double E)
{
  TVector3 mom(px, py, pz);
  mPt  = mom.Pt();
  mEta = mom.Eta();
  mPhi = mom.Phi();
  mE   = E;
}

#endif // ST_JET_CANDIDATE_H
