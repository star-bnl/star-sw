/***************************************************************************
 *
 * $Id: StHbtParticleCut.hh,v 1.1.1.1 1999/06/29 16:02:56 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 *     Users inherit from this class and must add their own quantities       
 * 
 ***************************************************************************
 *
 * $Log: StHbtParticleCut.hh,v $
 * Revision 1.1.1.1  1999/06/29 16:02:56  lisa
 * Installation of StHbtMaker
 *
 **************************************************************************/


#ifndef StHbtParticleCut_hh
#define StHbtParticleCut_hh

#include "StHbtMaker/Infrastructure/StHbtTrack.hh"
#include <string>

class StHbtParticleCut{

public:

  StHbtParticleCut(){/* no-op */};   // default constructor. - Users should write their own
  virtual ~StHbtParticleCut(){/* no-op */};  // destructor

  virtual bool Pass(const StHbtTrack* particle)=0;  // true if passes, false if not

  double Mass(){return mMass;};       // mass of the particle being selected
  virtual void SetMass(const double& mass) {mMass = mass;};

  virtual string Report() =0;    // user-written method to return string describing cuts

protected:
  double mMass;

};

#endif
