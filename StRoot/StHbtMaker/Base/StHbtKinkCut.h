/***************************************************************************
 *
 * $Id: StHbtKinkCut.h,v 1.1 2001/05/25 23:23:58 lisa Exp $
 *
 * Author: Mike Lisa, Ohio State, lisa@mps.ohio-state.edu
 ***************************************************************************
 *
 * Description: part of STAR HBT Framework: StHbtMaker package
 *     base class for particle-wise cuts
 * Users inherit from this class to make particular KinkCuts.
 *     Note that KinkCut is a derived class of ParticleCut
 * 
 ***************************************************************************
 *
 * $Log: StHbtKinkCut.h,v $
 * Revision 1.1  2001/05/25 23:23:58  lisa
 * Added in StHbtKink stuff
 *
 **************************************************************************/


#ifndef StHbtKinkCut_hh
#define StHbtKinkCut_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtKink.hh"
#include "StHbtMaker/Base/StHbtParticleCut.h"

class StHbtKinkCut : public StHbtParticleCut {

public:

  StHbtKinkCut(){/* no-op */};                       // default constructor. - Users should write their own
  StHbtKinkCut(const StHbtKinkCut&);                         // copy constructor
  virtual ~StHbtKinkCut(){/* no-op */};              // destructor

  virtual bool Pass(const StHbtKink* )=0;               // true if passes, false if not

  virtual StHbtParticleType Type(){return hbtKink;}
  virtual StHbtKinkCut* Clone() { return 0;}

#ifdef __ROOT__
  ClassDef(StHbtKinkCut, 0)
#endif
};

inline StHbtKinkCut::StHbtKinkCut(const StHbtKinkCut& c) : StHbtParticleCut(c) { /* no-op */ } 

#endif
