// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#ifndef ST_JET_CANDIDATE_H
#define ST_JET_CANDIDATE_H

#include <map>
#include <string>
using namespace std;

#include "TLorentzVector.h"
#include "TRef.h"
#include "TRefArray.h"

class StJetVertex;
class StJetElement;
class StJetTrack;
class StJetTower;
class StJetParticle;

class StJetCandidate : public TObject {
public:
  StJetCandidate()
    : mPt(0)
    , mEta(0)
    , mPhi(0)
    , mE(0)
    , mDetEta(0)
    , mRt(0)
    , mArea(0)
    , mAreaError(0)
  {
  }

  StJetCandidate(const TVector3& vertex, const TLorentzVector& fourMomentum, float area = 0, float area_error = 0);

  TLorentzVector fourMomentum() const;
  TVector3 momentum() const;

  float pt () const { return mPt ; }
  float eta() const { return mEta; }
  float phi() const { return mPhi; }
  float E  () const { return mE  ; }
  float area() const{ return mArea; }
  float areaError() const{ return mAreaError; }
  float px () const { return momentum().Px(); }
  float py () const { return momentum().Py(); }
  float pz () const { return momentum().Pz(); }
  float detEta() const { return mDetEta; }
  float detEta(const TVector3& vertex) const;
  bool  getBarrelDetectorEta(const TVector3& vertex, float& detEta) const;
  bool  getEndcapDetectorEta(const TVector3& vertex, float& detEta) const;
  float sumTrackPt() const;
  float sumTrackPt(float radius) const;
  float sumTowerPt() const;
  float sumTowerPt(float radius) const;
  float sumPt() const { return sumTrackPt() + sumTowerPt(); }
  float sumPt(float radius) const { return sumTrackPt(radius)+sumTowerPt(radius); }
  float profile(float radius) const { return sumPt(radius)/sumPt(); }
  float psi(float radius) const { return profile(radius); }
  float rt() const { return mRt; }
  float neutralFraction() const { return rt(); }
  float chargedFraction() const { return 1 - neutralFraction(); }
  StJetTrack* leadingChargedParticle() const;
  float deltaPhi(const StJetCandidate* jet) const { return momentum().DeltaPhi(jet->momentum()); }
  float deltaR(const StJetCandidate* jet) const { return momentum().DeltaR(jet->momentum()); }
  float deltaR(const StJetElement* element) const;

  int numberOfTracks() const { return mTracks.GetEntriesFast(); }
  int numberOfTowers() const { return mTowers.GetEntriesFast(); }
  int numberOfParticles() const { return mParticles.GetEntriesFast(); }

  StJetVertex* vertex() const { return (StJetVertex*)mVertex.GetObject(); }
  StJetTrack* track(int i) const { return (StJetTrack*)mTracks.At(i); }
  StJetTower* tower(int i) const { return (StJetTower*)mTowers.At(i); }
  StJetParticle* particle(int i) const { return (StJetParticle*)mParticles.At(i); }

  StJetTrack* getTrackById(int id) const;
  StJetTower* getTowerById(int id) const;

  const TRefArray& tracks() const { return mTracks; }
  const TRefArray& towers() const { return mTowers; }
  const TRefArray& particles() const { return mParticles; }

  // Utility functions to get jet patch eta and phi from jet patch id and vice-versa
  static float getJetPatchPhi(int jetPatch);
  static bool getBarrelJetPatchEtaPhi(int id, float& eta, float& phi);
  static bool getEndcapJetPatchEtaPhi(int id, float& eta, float& phi);
  static bool getOverlapJetPatchEtaPhi(int id, float& eta, float& phi);

  static bool getBarrelJetPatchId(float eta, float phi, int& id);
  static bool getEndcapJetPatchId(float eta, float phi, int& id);
  static bool getOverlapJetPatchId(float eta, float phi, int& id);

  void setPtEtaPhiE(float pt, float eta, float phi, float E);
  void setPxPyPzE(float px, float py, float pz, float E);
  void setVertex(const StJetVertex* vertex) { mVertex = (TObject*)vertex; }

  void setUeDensity(const char* name, float density){ string str(name); mUeDensity[str] = density; }
  map<string, float> ueDensity() const { return mUeDensity; }
  StJetTrack* addTrack(StJetTrack* track) { mTracks.Add((TObject*)track); return (StJetTrack*)mTracks.Last(); }
  StJetTower* addTower(StJetTower* tower) { mTowers.Add((TObject*)tower); return (StJetTower*)mTowers.Last(); }
  StJetParticle* addParticle(StJetParticle* particle) { mParticles.Add((TObject*)particle); return (StJetParticle*)mParticles.Last(); }

private:
  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;

  float mPt;
  float mEta;
  float mPhi;
  float mE;
  float mDetEta;
  float mRt;
  float mArea;
  float mAreaError;

  map<string, float> mUeDensity;

  TRef mVertex;
  TRefArray mTracks;
  TRefArray mTowers;
  TRefArray mParticles;

  ClassDef(StJetCandidate,5);
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
