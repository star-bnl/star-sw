/***************************************************************************
 *
 * $Id: StHbtParticle.hh,v 1.1.1.1 1999/06/29 16:02:57 lisa Exp $
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
 * Revision 1.1.1.1  1999/06/29 16:02:57  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/

#ifndef StHbtParticle_hh
#define StHbtParticle_hh

#include "StLorentzVector.hh"
#include "StHbtMaker/Infrastructure/StHbtTrack.hh"

class StHbtParticle{
public:
  StHbtParticle();
  StHbtParticle(const StHbtTrack* hbtTrack, const double& mass);
  ~StHbtParticle();

  StLorentzVector<double> FourMomentum() const;

private:
  StLorentzVector<double> mFourMomentum;
};

inline StLorentzVector<double> StHbtParticle::FourMomentum() const {return mFourMomentum;}


#endif
