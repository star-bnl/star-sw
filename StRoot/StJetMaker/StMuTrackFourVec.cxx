//StMuTrackFourVec.cxx
//M.L. Miller (Yale Software)
//07/02

//std
#include "Stiostream.h"

//SCL
#include "StarClassLibrary/StParticleTypes.hh"

//local
#include "StMuTrackFourVec.h"

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, StLorentzVectorF P, Int_t i)
: mTrack(t), mVec(P), index(i)
{
}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t) : mTrack(t)
{
    index = -1;
    StThreeVectorF mom = t->momentum();
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, Int_t i) : mTrack(t), index(i)
{
    index = i;
    StThreeVectorF mom = t->momentum();
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}

StMuTrackFourVec::StMuTrackFourVec() : mTrack(NULL), index(0)
{
  
}

void StMuTrackFourVec::Init(StMuTrack *track, StLorentzVectorF P, Int_t i)
{
  index = i;
  mVec = P;
  mTrack = track;
}
