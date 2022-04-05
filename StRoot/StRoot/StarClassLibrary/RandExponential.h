/***************************************************************************
 *
 * $Id: RandExponential.h,v 1.2 2003/09/02 17:59:34 perev Exp $
 *
 * Author:  Gabriele Cosmo - Created: 5th September 1995
 *          modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *             RandExponential.h,v 1.3 1997/08/12 00:38:39
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- RandExponential ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Class defining methods for shooting exponential distributed random
 * values, given a mean (default mean = 1).
 * Default mean is used for operator()().
 *
 *
 ***************************************************************************
 *
 * $Log: RandExponential.h,v $
 * Revision 1.2  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef RandExponential_h
#define RandExponential_h 1

#include "Random.h"

class RandExponential : public HepRandom {

public:

  inline RandExponential ( HepRandomEngine& anEngine );
  inline RandExponential ( HepRandomEngine* anEngine );
  // These constructors should be used to instantiate a RandExponential
  // distribution object defining a local engine for it.
  // The static generator will be skeeped using the non-static methods
  // defined below.
  // If the engine is passed by pointer the corresponding engine object
  // will be deleted by the RandExponential destructor.
  // If the engine is passed by reference the corresponding engine object
  // will not be deleted by the RandExponential destructor.

  virtual ~RandExponential();
  // Destructor

  // Static methods to shoot random values using the static generator

  static  inline HepDouble shoot();

  static  inline HepDouble shoot( HepDouble mean );

  static  void shootArray ( const HepInt size, HepDouble* vect,
                            HepDouble mean=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( vector<HepDouble>&,
                            HepDouble mean=1.0 );
#else
    static  void shootArray ( vector<HepDouble, allocator<HepDouble> >&,
			      HepDouble mean=1.0 );
#endif
  //  Static methods to shoot random values using a given engine
  //  by-passing the static generator.

  static  inline HepDouble shoot( HepRandomEngine* anEngine );

  static  inline HepDouble shoot( HepRandomEngine* anEngine, HepDouble mean );

  static  void shootArray ( HepRandomEngine* anEngine, const HepInt size,
                            HepDouble* vect, HepDouble mean=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( HepRandomEngine*,
			      vector<HepDouble>&, HepDouble mean=1.0 );
#else
    static  void shootArray ( HepRandomEngine*,
			      vector<HepDouble,allocator<HepDouble> >&, HepDouble mean=1.0 );
#endif
  //  Methods using the localEngine to shoot random values, by-passing
  //  the static generator.

  inline HepDouble fire();

  inline HepDouble fire( HepDouble mean );

  void fireArray ( const HepInt size, HepDouble* vect, HepDouble mean=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void fireArray ( vector<HepDouble>&, HepDouble mean=1.0 );
#else
    void fireArray ( vector<HepDouble, allocator<HepDouble> >&, HepDouble mean=1.0 );
#endif
  HepDouble operator()();
  
private:

  HepRandomEngine* localEngine;
  HepBoolean deleteEngine;

};

// ------------------
// Inlined functions
// ------------------

inline RandExponential::RandExponential(HepRandomEngine & anEngine)
: localEngine(&anEngine), deleteEngine(false) {}

inline RandExponential::RandExponential(HepRandomEngine * anEngine)
: localEngine(anEngine), deleteEngine(true) {}

inline HepDouble RandExponential::shoot() {
  return -::log(HepRandom::getTheGenerator()->flat());
}

inline HepDouble RandExponential::shoot(HepDouble mean) {
  return -::log(HepRandom::getTheGenerator()->flat())*mean;
}

//-------------

inline HepDouble RandExponential::shoot(HepRandomEngine* anEngine) {
  return -::log(anEngine->flat());
}

inline HepDouble RandExponential::shoot(HepRandomEngine* anEngine,
                                        HepDouble mean) {
  return -::log(anEngine->flat())*mean;
}

//-------------

inline HepDouble RandExponential::fire() {
  return -::log(localEngine->flat());
}

inline HepDouble RandExponential::fire(HepDouble mean) {
  return -::log(localEngine->flat())*mean;
}

#endif
