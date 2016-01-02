#include <limits>
#include <cmath>

#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/SystemOfUnits.h"

#include "StMixerPair.h"
#include "StMixerTrack.h"

ClassImp(StMixerPair)

// _________________________________________________________
StMixerPair::StMixerPair(): mLorentzVector(StLorentzVectorF()), mDecayVertex(StThreeVectorF()),
    mPointingAngle(std::numeric_limits<float>::quiet_NaN()), mDecayLength(std::numeric_limits<float>::quiet_NaN()),
    mParticle1Dca(std::numeric_limits<float>::quiet_NaN()), mParticle2Dca(std::numeric_limits<float>::quiet_NaN()),
    mParticle1Mom(StThreeVectorF()), mParticle2Mom(StThreeVectorF()),
    mDcaDaughters(std::numeric_limits<float>::max()), mCosThetaStar(std::numeric_limits<float>::quiet_NaN()) {
}

// _________________________________________________________
StMixerPair::StMixerPair(StMixerPair const * t) : mLorentzVector(t->mLorentzVector), mDecayVertex(t->mDecayVertex),
    mPointingAngle(t->mPointingAngle), mDecayLength(t->mDecayLength),
    mParticle1Dca(t->mParticle1Dca), mParticle2Dca(t->mParticle2Dca),
    mParticle1Mom(t->mParticle1Mom), mParticle2Mom(t->mParticle2Mom),
    mDcaDaughters(t->mDcaDaughters), mCosThetaStar(t->mCosThetaStar) {
}

// _________________________________________________________
StMixerPair::StMixerPair(StMixerTrack const& particle1, StMixerTrack const& particle2,
                         float p1MassHypo, float p2MassHypo,
                         StThreeVectorF const& vtx1, StThreeVectorF const& vtx2, float const bField) :  mLorentzVector(StLorentzVectorF()), mDecayVertex(StThreeVectorF()),
    mPointingAngle(std::numeric_limits<float>::quiet_NaN()), mDecayLength(std::numeric_limits<float>::quiet_NaN()),
    mParticle1Dca(std::numeric_limits<float>::quiet_NaN()), mParticle2Dca(std::numeric_limits<float>::quiet_NaN()),
    mParticle1Mom(particle1.gMom()), mParticle2Mom(particle2.gMom()),
    mDcaDaughters(std::numeric_limits<float>::max()), mCosThetaStar(std::numeric_limits<float>::quiet_NaN()) {
    // -- Create pair out of 2 tracks
    //     prefixes code:
    //      p1 means particle 1
    //      p2 means particle 2
    //      pair means particle1-particle2  pair

    StThreeVectorF dVtx = vtx1 -vtx2;

    StPhysicalHelixD p1Helix(particle1.gMom(), particle1.origin(),bField*kilogauss, particle1.charge());
    StPhysicalHelixD p2Helix(particle2.gMom(), particle2.origin() + dVtx, bField*kilogauss,  particle2.charge());

    // -- move origins of helices to the primary vertex origin
    p1Helix.moveOrigin(p1Helix.pathLength(vtx1));
    p2Helix.moveOrigin(p2Helix.pathLength(vtx1));

    // -- use straight lines approximation to get point of DCA of particle1-particle2 pair
    StThreeVectorF const p1Mom = p1Helix.momentum(bField * kilogauss);
    StThreeVectorF const p2Mom = p2Helix.momentum(bField * kilogauss);

    StPhysicalHelixD const p1StraightLine(p1Mom, p1Helix.origin(), 0, particle1.charge());
    StPhysicalHelixD const p2StraightLine(p2Mom, p2Helix.origin(), 0, particle2.charge());

    pair<double, double> const ss = p1StraightLine.pathLengths(p2StraightLine);
    StThreeVectorF const p1AtDcaToP2 = p1StraightLine.at(ss.first);
    StThreeVectorF const p2AtDcaToP1 = p2StraightLine.at(ss.second);

    // -- calculate DCA of particle1 to particle2 at their DCA
    mDcaDaughters = (p1AtDcaToP2 - p2AtDcaToP1).mag();

    // -- calculate Lorentz vector of particle1-particle2 pair
    StThreeVectorF const p1MomAtDca = p1Helix.momentumAt(ss.first,  bField * kilogauss);
    StThreeVectorF const p2MomAtDca = p2Helix.momentumAt(ss.second, bField * kilogauss);

    StLorentzVectorF const p1FourMom(p1MomAtDca, p1MomAtDca.massHypothesis(p1MassHypo));
    StLorentzVectorF const p2FourMom(p2MomAtDca, p2MomAtDca.massHypothesis(p2MassHypo));

    mLorentzVector = p1FourMom + p2FourMom;

    // -- calculate cosThetaStar
    StLorentzVectorF const pairFourMomReverse(-mLorentzVector.px(), -mLorentzVector.py(), -mLorentzVector.pz(), mLorentzVector.e());
    StLorentzVectorF const p1FourMomStar = p1FourMom.boost(pairFourMomReverse);
    mCosThetaStar = std::cos(p1FourMomStar.vect().angle(mLorentzVector.vect()));

    // -- calculate decay vertex (secondary or tertiary)
    mDecayVertex = (p1AtDcaToP2 + p2AtDcaToP1) * 0.5 ;

    // -- calculate pointing angle and decay length with respect to primary vertex
    StThreeVectorF const vtxToV0 = mDecayVertex - vtx1;
    mPointingAngle = vtxToV0.angle(mLorentzVector.vect());
    mDecayLength = vtxToV0.mag();

    // -- calculate DCA of tracks to primary vertex
    mParticle1Dca = (p1Helix.origin() - vtx1).mag();
    mParticle2Dca = (p2Helix.origin() - vtx1).mag();
}


