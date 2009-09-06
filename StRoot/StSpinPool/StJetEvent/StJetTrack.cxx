//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 5 September 2009
//

#include "StJetMaker/emulator/StMuTrackEmu.h"
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
  , mExitDetectorId(0)
  , mDca(0)
  , mDcaZ(0)
  , mDcaD(0)
{
  mPt = mEta = mPhi = 0;
}

StJetTrack::StJetTrack(const StMuTrackEmu* track)
  : StJetElement(track->id(), track->detectorId())
  , mFlag(track->flag())
  , mCharge(track->charge())
  , mNHits(track->nHits())
  , mNHitsFit(track->nHitsFit())
  , mNHitsPoss(track->nHitsPoss())
  , mNHitsDedx(track->nHitsDedx())
  , mDedx(track->dEdx())
  , mNSigmaPion(track->nSigmaPion())
  , mNSigmaKaon(track->nSigmaKaon())
  , mNSigmaProton(track->nSigmaProton())
  , mNSigmaElectron(track->nSigmaElectron())
  , mExitDetectorId(0)
  , mDca(track->Tdca())
  , mDcaZ(track->dcaZ())
  , mDcaD(track->dcaD())
{
  TVector3 mom(track->px(), track->py(), track->pz());
  mPt  = mom.Pt();
  mEta = mom.Eta();
  mPhi = mom.Phi();
  mExitPoint.SetPtEtaPhi(track->bemcRadius(), track->etaext(), track->phiext());
}
