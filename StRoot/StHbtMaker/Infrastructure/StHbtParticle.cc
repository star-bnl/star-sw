/***************************************************************************
 *
 * $Id: StHbtParticle.cc,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * $Log: StHbtParticle.cc,v $
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#include "StHbtMaker/Infrastructure/StHbtParticle.hh"

//_____________________
StHbtParticle::StHbtParticle(){
  /* no-op for default */
}
//_____________________
StHbtParticle::~StHbtParticle(){
  /* no-op */
}
//_____________________
StHbtParticle::StHbtParticle(const StHbtTrack* hbtTrack,const double& mass){
  // I know there is a better way to do this...
  StThreeVector<double> temp = hbtTrack->P();
  mFourMomentum.setVect(temp);
  double ener = sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
}
//_____________________
