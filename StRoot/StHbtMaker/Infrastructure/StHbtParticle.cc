/***************************************************************************
 *
 * $Id: StHbtParticle.cc,v 1.7 2000/04/03 16:21:51 laue Exp $
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
 * Revision 1.7  2000/04/03 16:21:51  laue
 * some include files changed
 * Multi track cut added
 *
 * Revision 1.6  1999/12/11 15:58:29  lisa
 * Add vertex decay position datum and accessor to StHbtParticle to allow pairwise cuts on seperation of V0s
 *
 * Revision 1.5  1999/09/17 22:38:02  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.4  1999/09/01 19:04:53  lisa
 * update Particle class AND add parity cf and Randys Coulomb correction
 *
 * Revision 1.3  1999/07/06 22:33:23  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.2  1999/06/29 17:50:27  fisyak
 * formal changes to account new StEvent, does not complie yet
 *
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
StHbtParticle::StHbtParticle(const StHbtTrack* const hbtTrack,const double& mass){
  // I know there is a better way to do this...
  StHbtThreeVector temp = hbtTrack->P();
  mFourMomentum.setVect(temp);
  double ener = sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);
  mMap[0] = hbtTrack->TopologyMap(0);
  mMap[1] = hbtTrack->TopologyMap(1);
  mNhits = hbtTrack->NHits();

  mHelix = hbtTrack->Helix();
}
//_____________________
StHbtParticle::StHbtParticle(const StHbtV0* const hbtV0,const double& mass){
  // I know there is a better way to do this...
  StHbtThreeVector temp = hbtV0->momV0();
  mFourMomentum.setVect(temp);
  double ener = sqrt(temp.mag2()+mass*mass);
  mFourMomentum.setE(ener);

  mDecayVertexV0 = hbtV0->decayVertexV0();
  //  mHelix = hbtTrack->Helix(); ?? what to do with mHelix for a Particle coming from a V0?
}
//_____________________
