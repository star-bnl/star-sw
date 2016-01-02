#include <limits>
#include <cmath>

#include "StHFPair.h"

#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/SystemOfUnits.h"
#include "StPicoDstMaker/StPicoTrack.h"

ClassImp(StHFPair)

// _________________________________________________________
StHFPair::StHFPair(): mLorentzVector(StLorentzVectorF()), mDecayVertex(StThreeVectorF()),
  mPointingAngle(std::numeric_limits<float>::quiet_NaN()), mDecayLength(std::numeric_limits<float>::quiet_NaN()),
  mParticle1Dca(std::numeric_limits<float>::quiet_NaN()), mParticle2Dca(std::numeric_limits<float>::quiet_NaN()),
  mParticle1Idx(std::numeric_limits<unsigned short>::max()), mParticle2Idx(std::numeric_limits<unsigned short>::max()),
  mDcaDaughters(std::numeric_limits<float>::max()), mCosThetaStar(std::numeric_limits<float>::quiet_NaN()) {
}

// _________________________________________________________
StHFPair::StHFPair(StHFPair const * t) : mLorentzVector(t->mLorentzVector), mDecayVertex(t->mDecayVertex),
   mPointingAngle(t->mPointingAngle), mDecayLength(t->mDecayLength),
   mParticle1Dca(t->mParticle1Dca), mParticle2Dca(t->mParticle2Dca),
   mParticle1Idx(t->mParticle1Idx), mParticle2Idx(t->mParticle2Idx),
   mDcaDaughters(t->mDcaDaughters), mCosThetaStar(t->mCosThetaStar) {
}

// _________________________________________________________
StHFPair::StHFPair(StPicoTrack const * const particle1, StPicoTrack const * const particle2,
		   float p1MassHypo, float p2MassHypo, unsigned short const p1Idx, unsigned short const p2Idx,
		   StThreeVectorF const & vtx, float const bField, bool const useStraightLine) : 
  mLorentzVector(StLorentzVectorF()), mDecayVertex(StThreeVectorF()),
  mPointingAngle(std::numeric_limits<float>::quiet_NaN()), mDecayLength(std::numeric_limits<float>::quiet_NaN()),
  mParticle1Dca(std::numeric_limits<float>::quiet_NaN()), mParticle2Dca(std::numeric_limits<float>::quiet_NaN()),
  mParticle1Idx(p1Idx), mParticle2Idx(p2Idx),
  mDcaDaughters(std::numeric_limits<float>::max()), mCosThetaStar(std::numeric_limits<float>::quiet_NaN()) {
  // -- Create pair out of 2 tracks
  //     prefixes code:
  //      p1 means particle 1
  //      p2 means particle 2
  //      pair means particle1-particle2  pair

  if ((!particle1 || !particle2) || (particle1->id() == particle2->id())) {
    mParticle1Idx = std::numeric_limits<unsigned short>::max();
    mParticle2Idx = std::numeric_limits<unsigned short>::max();
    return;
  }

  StPhysicalHelixD p1Helix = particle1->dcaGeometry().helix();
  StPhysicalHelixD p2Helix = particle2->dcaGeometry().helix();

  // -- move origins of helices to the primary vertex origin
  p1Helix.moveOrigin(p1Helix.pathLength(vtx));
  p2Helix.moveOrigin(p2Helix.pathLength(vtx));

  // -- use straight lines approximation to get point of DCA of particle1-particle2 pair
  StThreeVectorF const p1Mom = p1Helix.momentum(bField * kilogauss);
  StThreeVectorF const p2Mom = p2Helix.momentum(bField * kilogauss);
  StPhysicalHelixD const p1StraightLine(p1Mom, p1Helix.origin(), 0, particle1->charge());
  StPhysicalHelixD const p2StraightLine(p2Mom, p2Helix.origin(), 0, particle2->charge());

  pair<double, double> const ss = (useStraightLine) ? p1StraightLine.pathLengths(p2StraightLine) : p1Helix.pathLengths(p2Helix);
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
  //    if decay vertex is a tertiary vertex
  //    -> only rough estimate -> needs to be updated after secondary vertex is found
  StThreeVectorF const vtxToV0 = mDecayVertex - vtx;
  mPointingAngle = vtxToV0.angle(mLorentzVector.vect());
  mDecayLength = vtxToV0.mag();

  // -- calculate DCA of tracks to primary vertex
  //    if decay vertex is a tertiary vertex
  //    -> only rough estimate -> needs to be updated after secondary vertex is found
  mParticle1Dca = (p1Helix.origin() - vtx).mag();
  mParticle2Dca = (p2Helix.origin() - vtx).mag();
}

// _________________________________________________________
StHFPair::StHFPair(StPicoTrack const * const particle1, StHFPair const * const particle2,
		   float p1MassHypo, float p2MassHypo, unsigned short const p1Idx, unsigned short const p2Idx,
		   StThreeVectorF const & vtx, float const bField, bool const useStraightLine) :
  mLorentzVector(StLorentzVectorF()), mDecayVertex(StThreeVectorF()),
  mPointingAngle(std::numeric_limits<float>::quiet_NaN()), mDecayLength(std::numeric_limits<float>::quiet_NaN()),
  mParticle1Dca(std::numeric_limits<float>::quiet_NaN()), mParticle2Dca(std::numeric_limits<float>::quiet_NaN()),
  mParticle1Idx(p1Idx), mParticle2Idx(p2Idx),
  mDcaDaughters(std::numeric_limits<float>::max()), mCosThetaStar(std::numeric_limits<float>::quiet_NaN()) {
  // -- Create pair out of a particle and and a pair
  //     prefixes code:
  //      p1 means particle 1
  //      p2 means particle 2 - which is a pair of tertiaryP1 and tertiaryP2
  //      pair means particle1-particle2  pair
  
  // -- assume incoming pair always to be neutral 
  float p2Charge = 0.;

  // -- checking that particle1 and tertiary particle (=pair) exists, and particle1 id different from both used to reconstruct tertiary pair
  if ((!particle1 || !particle2) || (particle1->id() == particle2->particle1Idx()) || ( particle1->id() == particle2->particle2Idx())) {
    mParticle1Idx = std::numeric_limits<unsigned short>::max();
    mParticle2Idx = std::numeric_limits<unsigned short>::max();
    return;
  }

  StPhysicalHelixD p1Helix = particle1->dcaGeometry().helix();
  StThreeVectorF const p2Mom(particle2->px(),particle2->py(),particle2->pz());
  StThreeVectorF const p2Origin(particle2->v0x(),particle2->v0y(),particle2->v0z());
  StPhysicalHelixD p2Helix(p2Mom,p2Origin,bField*kilogauss, p2Charge);

  // --move origins of helices to the primary vertex origin
  p1Helix.moveOrigin(p1Helix.pathLength(vtx));
  p2Helix.moveOrigin(p2Helix.pathLength(vtx));

  // --use straight lines approximation to get point of DCA of particle1-particle2 pair
  StThreeVectorF const p1Mom = p1Helix.momentum(bField * kilogauss);
  StPhysicalHelixD const p1StraightLine(p1Mom, p1Helix.origin(), 0, particle1->charge());
  StPhysicalHelixD const p2StraightLine(p2Mom, p2Helix.origin(), 0, p2Charge);
  
  pair<double, double> const ss = (useStraightLine) ? p1StraightLine.pathLengths(p2StraightLine) : p1Helix.pathLengths(p2Helix);
  StThreeVectorF const p1AtDcaToP2 = p1StraightLine.at(ss.first);
  StThreeVectorF const p2AtDcaToP1 = p2StraightLine.at(ss.second);

  // -- calculate DCA of particle1 to particl2 at their DCA
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

  // -- calculate decay vertex (secondary) 
  mDecayVertex = (p1AtDcaToP2 + p2AtDcaToP1) * 0.5 ;

  // -- calculate pointing angle and decay length with respect to primary vertex
  StThreeVectorF const vtxToV0 = mDecayVertex - vtx;
  mPointingAngle = vtxToV0.angle(mLorentzVector.vect());
  mDecayLength = vtxToV0.mag();
   
  // -- calculate DCA of tracks to primary vertex
  mParticle1Dca = (p1Helix.origin() - vtx).mag();
  mParticle2Dca = (p2Helix.origin() - vtx).mag();
}
// _________________________________________________________
float StHFPair::pointingAngle(StThreeVectorF const & vtx2) const{
  // -- Overloaded function recalculates pointing angle given secondary vertex
  StThreeVectorF const vtx2ToTertiary(mDecayVertex - vtx2);
  float const nPointingAngle = vtx2ToTertiary.angle(mLorentzVector.vect());
  return nPointingAngle;
}
// _________________________________________________________
float StHFPair::decayLength(StThreeVectorF const & vtx2) const{
  // -- Overloaded function recalculates decayLength given secondary vertex
  StThreeVectorF const vtx2ToTertiary(mDecayVertex - vtx2); 
  float const nDecayLength = vtx2ToTertiary.mag();  
  return nDecayLength;
}
// _________________________________________________________
float StHFPair::particle1Dca(StPicoTrack const * p1track, StThreeVectorF const & vtx2) const{
  // -- Overloaded function recalculates daughter dca 2 updated vertex
  StPhysicalHelixD p1Helix = p1track->dcaGeometry().helix();
  // -- move origins of helices to the primary vertex origin
  p1Helix.moveOrigin(p1Helix.pathLength(vtx2));
  float const nParticle1Dca = (p1Helix.origin() - vtx2).mag();
  return nParticle1Dca;
}
// _________________________________________________________
float StHFPair::particle2Dca(StPicoTrack const * p2track, StThreeVectorF const & vtx2) const{
  // -- Overloaded function recalculates daughter dca 2 updated vertex
  StPhysicalHelixD p2Helix = p2track->dcaGeometry().helix();
  // -- move origins of helices to the primary vertex origin
  p2Helix.moveOrigin(p2Helix.pathLength(vtx2));
  float const nParticle2Dca = (p2Helix.origin() - vtx2).mag();
  return nParticle2Dca;
}


