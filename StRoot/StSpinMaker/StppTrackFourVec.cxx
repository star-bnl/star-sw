//StppTrackFourVec.cxx
//M.L. Miller (Yale Software)
//07/02

//std
#include "Stiostream.h"

//SCL
#include "StarClassLibrary/StParticleTypes.hh"

//local
#include "StppTrackFourVec.h"

StppTrackFourVec::StppTrackFourVec(StppTrack* t) : mTrack(t)
{
    double pt = t->pt;
    double phi = t->phi0;
    double px = pt*cos(phi);
    double py = pt*sin(phi);
    double pz = t->p *sinh(t->eta);
    
    StThreeVectorF mom( px, py, pz);
    //everything is a pion for now!!!!
    mVec = StLorentzVectorF( mom.massHypothesis(StPionPlus::instance()->mass() ) , mom);
}
