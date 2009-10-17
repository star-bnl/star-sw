//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 5 September 2009
//

#include "StJetCandidate.h"
#include "StJetTrack.h"

ClassImp(StJetTrack);

StJetTrack::StJetTrack()
  : mFlag(0)
  , mCharge(0)
  , mNHits(0)
  , mNHitsFit(0)
  , mNHitsPoss(0)
  , mNHitsDedx(0)
  , mDedx(0)
  , mNSigmaPion(0)
  , mNSigmaKaon(0)
  , mNSigmaProton(0)
  , mNSigmaElectron(0)
  , mExitPoint(0,0,0)
  , mExitTowerId(0)
  , mExitDetectorId(0)
  , mDca(0)
  , mDcaZ(0)
  , mDcaD(0)
{
  mPt = mEta = mPhi = 0;
}
