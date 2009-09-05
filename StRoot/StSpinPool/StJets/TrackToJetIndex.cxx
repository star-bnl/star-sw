// $Id: TrackToJetIndex.cxx,v 1.5 2009/09/05 18:18:05 pibero Exp $
#include "StJet.h"
#include "TrackToJetIndex.h"

ClassImp(TrackToJetIndex)

TrackToJetIndex::TrackToJetIndex(int jetIndex, int trackIndex, StDetectorId detId, StJet* jet)
  : mJetIndex(jetIndex)
  , mTrackIndex(trackIndex)
  , mDetId(detId)
  , mCharge(0)
  , mNhits(0)
  , mNhitsPoss(0)
  , mNhitsDedx(0)
  , mNhitsFit(0)
  , mNsigmaPion(0)
  , mTdca(0)
  , mTdcaz(0)
  , mTdcaxy(0)
  , metaext(0)
  , mphiext(0)
  , mdEdx(0)
  , mTrackId(0)
  , mJet(jet)
{
}

TVector3 TrackToJetIndex::localMomentum() const
{
  TVector3 longUnit = jet()->Vect().Unit();
  TVector3 normUnit = TVector3(0,0,1).Cross(longUnit).Unit();
  TVector3 sideUnit = longUnit.Cross(normUnit);
  TVector3 mom = momentum();
  return TVector3(mom.Dot(sideUnit),mom.Dot(normUnit),mom.Dot(longUnit));
}

double TrackToJetIndex::frag() const
{
  return Vect().Mag() / jet()->Vect().Mag();
}
