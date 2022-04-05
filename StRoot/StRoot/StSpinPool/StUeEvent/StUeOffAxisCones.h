#ifndef STUEOFFAXISCONES
#define STUEOFFAXISCONES

#include "TObject.h"
#include "TRefArray.h"

#include "StSpinPool/StJetEvent/StJetTrack.h"
#include "StSpinPool/StJetEvent/StJetTower.h"
#include "StSpinPool/StJetEvent/StJetParticle.h"

class StUeOffAxisCones : public TObject{
 public:
 StUeOffAxisCones() : mPt(0.), mEta(999), mPhi(3.15), mRadius(99.), mMult(0)
	{
  }
  ~StUeOffAxisCones(){
  }

  float pt() const{ return mPt;}
  float eta() const{ return mEta;}
  float phi() const{ return mPhi;}
  float radius() const{ return mRadius;}
  float rpho() const {
    if(mRadius > 0.) return mPt/(3.14159*mRadius*mRadius);
    else return 0.;
  }
  float mult() const { return mMult;}

  StJetTrack* track(int i) const { return (StJetTrack*)mTracks.At(i); }
  StJetTower* tower(int i) const { return (StJetTower*)mTowers.At(i); }
  StJetParticle* particle(int i) const { return (StJetParticle*)mParticles.At(i); }

  int numberOfTracks() const { return mTracks.GetEntriesFast(); }
  int numberOfTowers() const { return mTowers.GetEntriesFast(); }
  int numberOfParticles() const { return mParticles.GetEntriesFast(); }

  const TRefArray& tracks() const { return mTracks; }
  const TRefArray& towers() const { return mTowers; }
  const TRefArray& particles() const { return mParticles; }

  void setPt(float pt) { mPt = pt; }
  void setEtaPhi(float eta, float phi) { mEta = eta; mPhi = phi;}
  void setRadius(float radius) { mRadius = radius; }
  void setMult(int mult) { mMult = mult; }
  StJetTrack* addTrack(StJetTrack* track) { mTracks.Add((TObject*)track); return (StJetTrack*)mTracks.Last(); }
  StJetTower* addTower(StJetTower* tower) { mTowers.Add((TObject*)tower); return (StJetTower*)mTowers.Last(); }
  StJetParticle* addParticle(StJetParticle* particle) { mParticles.Add((TObject*)particle); return (StJetParticle*)mParticles.Last(); }
 private:
  float mPt;
  float mEta;
  float mPhi;

  float mRadius;
  int mMult;

  TRefArray mTracks;
  TRefArray mTowers;
  TRefArray mParticles;

  ClassDef(StUeOffAxisCones, 1);
};
#endif
