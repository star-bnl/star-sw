/***************************************************************************
 *
 * $Id: 
 *
 * Author: Frakn Laue, BNL
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 * Users inherit from this class to make particular XiCuts.
 *     Note that XiCut is a derived class of ParticleCut
 * 
 ***************************************************************************
 *
 * $Log:
 **************************************************************************/


#ifndef StHbtXiCut_hh
#define StHbtXiCut_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtXi.hh"
#include "StHbtMaker/Base/StHbtParticleCut.h"

class StHbtXiCut : public StHbtParticleCut {

public:

  StHbtXiCut(){/* no-op */};                       // default constructor. - Users should write their own
  StHbtXiCut(const StHbtXiCut&);                         // copy constructor
  virtual ~StHbtXiCut(){/* no-op */};              // destructor

  virtual bool Pass(const StHbtXi* )=0;               // true if passes, false if not

  virtual StHbtParticleType Type(){return hbtXi;}
  virtual StHbtXiCut* Clone() { return 0;}

#ifdef __ROOT__
  ClassDef(StHbtXiCut, 0)
#endif
};

inline StHbtXiCut::StHbtXiCut(const StHbtXiCut& c) : StHbtParticleCut(c) { /* no-op */ } 

#endif
