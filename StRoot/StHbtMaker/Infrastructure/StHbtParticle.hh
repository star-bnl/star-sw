/***************************************************************************
 *
 * $Id: StHbtParticle.hh,v 1.6 2000/02/26 19:04:52 laue Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *   Particle objects are part of the PicoEvent, which is what is
 *   stored in the EventMixingBuffers
 *   A Track object gets converted to a Particle object if it
 *   passes the ParticleCut of an Analysis
 *
 ***************************************************************************
 *
 * $Log: StHbtParticle.hh,v $
 * Revision 1.6  2000/02/26 19:04:52  laue
 * Some unnecessary includes removed.
 * StThreeVectorD replace by StHbtThreeVector.
 * StHbtCoulomb modified to compile without Root (ClassDef embraced into
 *   #ifdef __ROOT__  ..... #endif)
 * StHbtParticle now returns references (FourMomentum(),Helix(),
 *   DecayVertexPosiion())
 *
 * Revision 1.5  1999/12/11 15:58:29  lisa
 * Add vertex decay position datum and accessor to StHbtParticle to allow pairwise cuts on seperation of V0s
 *
 * Revision 1.4  1999/09/17 22:38:02  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.3  1999/09/01 19:04:54  lisa
 * update Particle class AND add parity cf and Randys Coulomb correction
 *
 * Revision 1.2  1999/07/06 22:33:23  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtParticle_hh
#define StHbtParticle_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StPhysicalHelixD.hh"

class StHbtParticle{
public:
  StHbtParticle();
  StHbtParticle(const StHbtTrack* const hbtTrack, const double& mass);
  StHbtParticle(const StHbtV0* const hbtV0, const double& mass);
  ~StHbtParticle();

  const StHbtLorentzVector& FourMomentum() const;

  StPhysicalHelixD& Helix();

  const StHbtThreeVector& DecayVertexPosition() const;

private:
  StHbtLorentzVector mFourMomentum;
  StPhysicalHelixD mHelix;
  StHbtThreeVector mDecayVertexV0;

};

inline const StHbtLorentzVector& StHbtParticle::FourMomentum() const {return mFourMomentum;}
inline StPhysicalHelixD& StHbtParticle::Helix() {return mHelix;}
inline const StHbtThreeVector& StHbtParticle::DecayVertexPosition() const {return mDecayVertexV0;}
#endif
