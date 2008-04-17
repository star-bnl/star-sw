// $Id: StMuTrackFourVec.cxx,v 1.4 2008/04/17 20:12:12 tai Exp $
#include "StMuTrackFourVec.h"

#include "Stiostream.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StarClassLibrary/StParticleTypes.hh"

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, StLorentzVectorF P, Int_t i, StDetectorId detId)
  : mTrack(t)
  , mVec(P)
  , index(i)
  , mDetId(detId)
  , mCharge(!t ? 0 : (double)t->charge())
{
}

StMuTrackFourVec::StMuTrackFourVec() 
  : mTrack(NULL)
  , index(0)
  , mDetId(kUnknownId)
  , mCharge(0)
{

}

void StMuTrackFourVec::Init(StMuTrack *track, StLorentzVectorF P, Int_t i, StDetectorId id)
{
  index = i;
  mVec = P;
  mTrack = track;
  mDetId = id;
  mCharge = !track ? 0 : (double)track->charge();
}
