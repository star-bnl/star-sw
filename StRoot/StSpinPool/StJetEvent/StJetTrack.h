// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#ifndef ST_JET_TRACK_H
#define ST_JET_TRACK_H

#include "TMath.h"
#include "StJetElement.h"

class StJetTrack : public StJetElement {
public:
  StJetTrack()
    : StJetElement()
    , mFlag(0)
    , mCharge(0)
    , mNHits(0)
    , mNHitsFit(0)
    , mNHitsPoss(0)
    , mNHitsDedx(0)
    , mDedx(0)
    , mExitTowerId(0)
    , mExitDetectorId(0)
    , mChi2(0)
    , mChi2Prob(0)
    , mBeta(0)
    , mNSigmaPion(0)
    , mNSigmaKaon(0)
    , mNSigmaProton(0)
    , mNSigmaElectron(0)
  {
  }

  friend class StjeJetEventTreeWriter;
  friend class StJetMaker2009;
  friend class StUEMaker2009;

  short flag()                const { return mFlag; }
  short charge()              const { return mCharge; }
  short nHits()               const { return mNHits; }
  short nHitsFit()            const { return mNHitsFit; }
  short nHitsPoss()           const { return mNHitsPoss; }
  short nHitsDedx()           const { return mNHitsDedx; }
  float dEdx()                const { return mDedx; }
  const TVector3& exitPoint() const { return mExitPoint; }
  short exitTowerId()         const { return mExitTowerId; }
  short exitDetectorId()      const { return mExitDetectorId; }
  const TVector3& dca()       const { return mDca; }
  float dcaD()                const { return mDcaD; }
  float dcaXY()               const { return dcaD(); }
  float dcaZ()                const { return dca().z(); }
  float chi2()                const { return mChi2; }
  float chi2prob()            const { return mChi2Prob; }
  float beta()                const { return mBeta; }
  const TVector3& firstPoint() const { return mFirstPoint; }
  const TVector3&  lastPoint() const { return mLastPoint;  }
  float nSigmaPion()           const { return mNSigmaPion; }
  float nSigmaKaon()           const { return mNSigmaKaon; }
  float nSigmaProton()         const { return mNSigmaProton; }
  float nSigmaElectron()       const { return mNSigmaElectron; }
  float m()                    const { return (beta() > 0 && beta() < 1) ? momentum().Mag()*TMath::Sqrt(1/(beta()*beta())-1) : -999; }

private:
  short    mFlag;
  short    mCharge;
  short    mNHits;
  short    mNHitsFit;
  short    mNHitsPoss;
  short    mNHitsDedx;
  float    mDedx;
  TVector3 mExitPoint;
  short    mExitTowerId;
  short    mExitDetectorId;
  TVector3 mDca;
  float    mDcaD;
  float    mChi2;
  float    mChi2Prob;
  float    mBeta;
  TVector3 mFirstPoint;
  TVector3 mLastPoint;
  float    mNSigmaPion;
  float    mNSigmaKaon;
  float    mNSigmaProton;
  float    mNSigmaElectron;

  ClassDef(StJetTrack,5);
};

#endif // ST_JET_TRACK_H
