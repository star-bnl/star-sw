/***************************************************************************
 *
 * $Id: 
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 * Users inherit from this class to make particular V0Cuts.
 *     Note that V0Cut is a derived class of ParticleCut
 * 
 ***************************************************************************
 *
 * $Log:
 **************************************************************************/


#ifndef StHbtV0Cut_hh
#define StHbtV0Cut_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtV0.hh"
#include "StHbtMaker/Base/StHbtParticleCut.h"

class StHbtV0Cut : public StHbtParticleCut {

public:

  StHbtV0Cut(){/* no-op */};                       // default constructor. - Users should write their own
  StHbtV0Cut(const StHbtV0Cut&);                         // copy constructor
  virtual ~StHbtV0Cut(){/* no-op */};              // destructor

  virtual bool Pass(const StHbtV0* )=0;               // true if passes, false if not

  virtual StHbtParticleType Type(){return hbtV0;}
  virtual StHbtV0Cut* Clone() { return 0;}

#ifdef __ROOT__
  ClassDef(StHbtV0Cut, 0)
#endif
};

inline StHbtV0Cut::StHbtV0Cut(const StHbtV0Cut& c) : StHbtParticleCut(c) { /* no-op */ } 

#endif
