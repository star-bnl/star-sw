//StMuTrackFourVec.cxx
//M.L. Miller (Yale Software)
//07/02

//std
#include "Stiostream.h"
#include <string>
using namespace std;

//SCL
#include "StarClassLibrary/StParticleTypes.hh"

//local
#include "StMuTrackFourVec.h"

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, StLorentzVectorF P, Int_t i, StDetectorId detId)
    : mTrack(t), mVec(P), index(i), mDetId(detId)
{
}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t) : mTrack(t)
{
    index = -1;
    mDetId = kUnknownId;
    StThreeVectorF mom = t->momentum();
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, Int_t i) : mTrack(t), index(i)
{
    mDetId = kUnknownId;
    index = i;
    StThreeVectorF mom = t->momentum();
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}

StMuTrackFourVec::StMuTrackFourVec() : mTrack(NULL), index(0)
{
    mDetId = kUnknownId;
}

void StMuTrackFourVec::Init(StMuTrack *track, StLorentzVectorF P, Int_t i, StDetectorId id)
{
  index = i;
  mVec = P;
  mTrack = track;
  mDetId = id;
  //cout <<(*this)<<endl;
}
