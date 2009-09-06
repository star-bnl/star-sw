// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 August 2009
//

#ifndef ST_JET_TRACK_H
#define ST_JET_TRACK_H

class StMuTrackEmu;

#include "StJetElement.h"

class StJetTrack : public StJetElement {
public:
  StJetTrack();
  StJetTrack(const StMuTrackEmu* track);

  short flag()                const { return mFlag; }
  short charge()              const { return mCharge; }
  short nHits()               const { return mNHits; }
  short nHitsFit()            const { return mNHitsFit; }
  short nHitsPoss()           const { return mNHitsPoss; }
  short nHitsDedx()           const { return mNHitsDedx; }
  float dEdx()                const { return mDedx; }
  float nSigmaPion()          const { return mNSigmaPion; }
  float nSigmaKaon()          const { return mNSigmaKaon; }
  float nSigmaProton()        const { return mNSigmaProton; }
  float nSigmaElectron()      const { return mNSigmaElectron; }
  const TVector3& exitPoint() const { return mExitPoint; }
  short exitTowerId()         const { return mExitTowerId; }
  short exitDetectorId()      const { return mExitDetectorId; }
  float dca()                 const { return mDca; }
  float dcaZ()                const { return mDcaZ; }
  float dcaD()                const { return mDcaD; }

private:
  short mFlag;
  short mCharge;
  short mNHits;
  short mNHitsFit;
  short mNHitsPoss;
  short mNHitsDedx;
  float mDedx;
  float mNSigmaPion;
  float mNSigmaKaon;
  float mNSigmaProton;
  float mNSigmaElectron;
  TVector3 mExitPoint;
  short mExitTowerId;
  short mExitDetectorId;
  float mDca;
  float mDcaZ;
  float mDcaD;

  ClassDef(StJetTrack, 1);
};

#endif // ST_JET_TRACK_H
