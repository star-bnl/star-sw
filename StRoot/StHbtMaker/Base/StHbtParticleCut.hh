/***************************************************************************
 *
 * $Id: StHbtParticleCut.hh,v 1.3 1999/09/17 22:37:59 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 * Note:    Users DO NOT inherit from this class!
 *          The base classes StHbtTrackCut and StHbtV0Cut inherit from this,
 *          and users inherit from those
 * 
 ***************************************************************************
 *
 * $Log: StHbtParticleCut.hh,v $
 * Revision 1.3  1999/09/17 22:37:59  lisa
 * first full integration of V0s into StHbt framework
 *
 * Revision 1.2  1999/07/06 22:33:19  lisa
 * Adjusted all to work in pro and new - dev itself is broken
 *
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#ifndef StHbtParticleCut_hh
#define StHbtParticleCut_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"

class StHbtParticleCut{

public:

  StHbtParticleCut(){/* no-op */};   // default constructor. - Users should write their own
  virtual ~StHbtParticleCut(){/* no-op */};  // destructor

  virtual StHbtString Report() =0;    // user-written method to return string describing cuts

  double Mass(){return mMass;};       // mass of the particle being selected
  virtual void SetMass(const double& mass) {mMass = mass;};

  virtual StHbtParticleType Type()=0;


protected:
  double mMass;
  //  StHbtParticleType mType;            // tells whether cut is on Tracks or V0's
};

#endif
