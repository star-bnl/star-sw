//StMuTrackFourVec.cxx
//M.L. Miller (Yale Software)
//07/02

//std
#include <iostream>

//SCL
#include "StarClassLibrary/StParticleTypes.hh"

//local
#include "StMuTrackFourVec.h"

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t) : mTrack(t)
{
    index = -1;
    StThreeVectorF mom = t->momentum();
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}

StMuTrackFourVec::StMuTrackFourVec(StMuTrack* t, Int_t i) : mTrack(t)
{
    index = i;
    StThreeVectorF mom = t->momentum();
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}

