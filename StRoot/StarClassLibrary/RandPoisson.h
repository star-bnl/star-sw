/***************************************************************************
 *
 * $Id: RandPoisson.h,v 1.1 1999/01/30 03:59:00 fisyak Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *            RandPoisson.h,v 1.5 1998/02/05 00:29:04
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                         --- RandPoisson ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Class defining methods for shooting numbers according to the Poisson
 * distribution, given a mean (Algorithm taken from "W.H.Press et al.,
 * Numerical Recipes in C, Second Edition".
 * Default mean value is set to 1, value used for operator()().
 *
 **************************************************************************
 *
 * $Log: RandPoisson.h,v $
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:39  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef RandPoisson_h
#define RandPoisson_h 1

#include "Random.h"

class RandPoisson : public HepRandom {

public:

  inline RandPoisson ( HepRandomEngine& anEngine );
  inline RandPoisson ( HepRandomEngine* anEngine );
  // These constructors should be used to instantiate a RandPoisson
  // distribution object defining a local engine for it.
  // The static generator will be skeeped using the non-static methods
  // defined below.
  // If the engine is passed by pointer the corresponding engine object
  // will be deleted by the RandPoisson destructor.
  // If the engine is passed by reference the corresponding engine object
  // will not be deleted by the RandPoisson destructor.

  virtual ~RandPoisson();
  // Destructor

  // Static methods to shoot random values using the static generator

  static  long shoot( HepDouble m=1.0 );

  static  void shootArray ( const HepInt size, long* vect, HepDouble m=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( vector<long>&, HepDouble m=1.0 );
#else
    static  void shootArray ( vector<long, allocator<long> >&, HepDouble m=1.0 );
#endif
  //  Static methods to shoot random values using a given engine
  //  by-passing the static generator.

  static  long shoot( HepRandomEngine* anEngine, HepDouble m=1.0 );

  static  void shootArray ( HepRandomEngine* anEngine,
                            const HepInt size, long* vect, HepDouble m=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( HepRandomEngine*,
			      vector<long>&, HepDouble m=1.0 );
#else
    static  void shootArray ( HepRandomEngine*,
			      vector<long, allocator<long> >&, HepDouble m=1.0 );
#endif
  //  Methods using the localEngine to shoot random values, by-passing
  //  the static generator.

  long  fire( HepDouble m=1.0 );

  void fireArray ( const HepInt size, long* vect, HepDouble m=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void fireArray ( vector<long>&, HepDouble m=1.0 );
#else
    void fireArray ( vector<long,allocator<long> >&, HepDouble m=1.0 );
#endif
  HepDouble operator()();
  

protected:

  static  HepDouble getOldMean() {return HepRandom::getTheGenerator()->getOldMean();}

  static  HepDouble getMaxMean() {return HepRandom::getTheGenerator()->getMaxMean();}

  static  void setOldMean( HepDouble val ){HepRandom::getTheGenerator()->setOldMean(val);}

  static  HepDouble* getPStatus() {return HepRandom::getTheGenerator()->getPStatus();}

  static void setPStatus(HepDouble sq, HepDouble alxm, HepDouble g) {
     HepRandom::getTheGenerator()->setPStatus(sq, alxm, g);
  }

private:

  HepRandomEngine* localEngine;
  HepBoolean deleteEngine;
  HepDouble status[3], oldm;
  const HepDouble meanMax;
};

// ------------------
// Inlined functions
// ------------------

inline RandPoisson::RandPoisson(HepRandomEngine & anEngine)
: localEngine(&anEngine), deleteEngine(false),
  oldm(-1.0), meanMax(2.0E9) {
  status[0] = status[1] = status[2] = 0.;
}

inline RandPoisson::RandPoisson(HepRandomEngine * anEngine)
: localEngine(anEngine), deleteEngine(true),
  oldm(-1.0), meanMax(2.0E9) {
  status[0] = status[1] = status[2] = 0.;
}

#endif
