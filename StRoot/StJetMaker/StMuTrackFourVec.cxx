//StMuTrackFourVec.cxx
//M.L. Miller (Yale Software)
//07/02

#include "StMuTrackFourVec.h"

#include "Stiostream.h"

#include "StarClassLibrary/StParticleTypes.hh"

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, StLorentzVectorF P, Int_t i, StDetectorId detId)
  : mTrack(t)
  , mVec(P)
  , index(i)
  , mDetId(detId)
{
}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t)
  : mTrack(t)
  , mVec(StLorentzVectorF( t->momentum().massHypothesis(StPionPlus::instance()->mass() ), t->momentum()))
  , index(-1)
  , mDetId(kUnknownId)
{

}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, Int_t i)
  : mTrack(t)
  , mVec(StLorentzVectorF( t->momentum().massHypothesis(StPionPlus::instance()->mass() ), t->momentum()))
  , index(i)
  , mDetId(kUnknownId)
{

}

StMuTrackFourVec::StMuTrackFourVec() 
  : mTrack(NULL)
  , index(0)
  , mDetId(kUnknownId)
{

}

void StMuTrackFourVec::Init(StMuTrack *track, StLorentzVectorF P, Int_t i, StDetectorId id)
{
  index = i;
  mVec = P;
  mTrack = track;
  mDetId = id;
}
