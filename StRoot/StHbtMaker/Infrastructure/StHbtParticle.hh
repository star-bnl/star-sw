/***************************************************************************
 *
 * $Id: StHbtParticle.hh,v 1.4 1999/09/17 22:38:02 lisa Exp $
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

  StHbtLorentzVector FourMomentum() const;

  StPhysicalHelixD& Helix();


private:
  StHbtLorentzVector mFourMomentum;
  StPhysicalHelixD mHelix;

};

inline StHbtLorentzVector StHbtParticle::FourMomentum() const {return mFourMomentum;}
inline StPhysicalHelixD& StHbtParticle::Helix() {return mHelix;}

#endif
