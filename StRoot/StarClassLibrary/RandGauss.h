/***************************************************************************
 *
 * $Id: RandGauss.h,v 1.1 1999/01/30 03:59:00 fisyak Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *                RandGauss.h,v 1.3 1997/08/12 00:38:44
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                          --- RandGauss ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Class defining methods for shooting gaussian distributed random values,
 * given a mean (deafult=0) or specifying also a deviation (default=1).
 * Gaussian random numbers are generated two at the time, so every
 * other time shoot is called the number returned is the one generated the
 * time before.
 * Default values are used for operator()().
 *
 ***************************************************************************
 *
 * $Log: RandGauss.h,v $
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:38  ullrich
 * Initial Revision
 *
 **************************************************************************/

#ifndef RandGauss_h
#define RandGauss_h 1

#include "Random.h"

class RandGauss : public HepRandom {

public:

  inline RandGauss ( HepRandomEngine& anEngine );
  inline RandGauss ( HepRandomEngine* anEngine );
  // These constructors should be used to instantiate a RandGauss
  // distribution object defining a local engine for it.
  // The static generator will be skeeped using the non-static methods
  // defined below.
  // If the engine is passed by pointer the corresponding engine object
  // will be deleted by the RandGauss destructor.
  // If the engine is passed by reference the corresponding engine object
  // will not be deleted by the RandGauss destructor.

  virtual ~RandGauss();
  // Destructor

  // Static methods to shoot random values using the static generator

  static  HepDouble shoot();

  static  inline HepDouble shoot( HepDouble mean, HepDouble stdDev );

  static  void shootArray ( const HepInt size, HepDouble* vect,
                            HepDouble mean=0.0, HepDouble stdDev=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( vector<HepDouble>&,
                            HepDouble mean=0.0, HepDouble stdDev=1.0 );
#else
    static  void shootArray ( vector<HepDouble, allocator<HepDouble> >&,
                            HepDouble mean=0.0, HepDouble stdDev=1.0 );
#endif
  //  Static methods to shoot random values using a given engine
  //  by-passing the static generator.

  static  HepDouble shoot( HepRandomEngine* anEngine );

  static  inline HepDouble shoot( HepRandomEngine* anEngine, 
                                  HepDouble mean, HepDouble stdDev );

  static  void shootArray ( HepRandomEngine* anEngine, const HepInt size,
                            HepDouble* vect, HepDouble mean=0.0,
                            HepDouble stdDev=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( HepRandomEngine*,
			      vector<HepDouble>&,
			      HepDouble mean=0.0, HepDouble stdDev=1.0 );
#else
    static  void shootArray ( HepRandomEngine*,
			      vector<HepDouble, allocator<HepDouble> >&,
			      HepDouble mean=0.0, HepDouble stdDev=1.0 );
#endif
  //  Methods using the localEngine to shoot random values, by-passing
  //  the static generator.

  HepDouble fire();

  inline HepDouble fire( HepDouble mean, HepDouble stdDev );
  
  void fireArray ( const HepInt size, HepDouble* vect,
                   HepDouble mean=0.0, HepDouble stdDev=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void fireArray ( vector<HepDouble>&,
                   HepDouble mean=0.0, HepDouble stdDev=1.0 );
#else
    void fireArray ( vector<HepDouble, allocator<HepDouble> >&,
		     HepDouble mean=0.0, HepDouble stdDev=1.0 );
#endif
  HepDouble operator()();

protected:

  static  HepBoolean getFlag() {return HepRandom::getTheGenerator()->getFlag();}

  static  void setFlag( HepBoolean val ){HepRandom::getTheGenerator()->setFlag(val);}

  static  HepDouble getVal() {return HepRandom::getTheGenerator()->getVal();}

  static  void setVal( HepDouble nextVal ){HepRandom::getTheGenerator()->setVal(nextVal);}

private:

  HepRandomEngine* localEngine;
  HepBoolean deleteEngine, set;
  HepDouble nextGauss;

};

// ------------------
// Inlined functions
// ------------------

inline RandGauss::RandGauss(HepRandomEngine & anEngine)
: localEngine(&anEngine), deleteEngine(false), set(false),
  nextGauss(0.0) {}

inline RandGauss::RandGauss(HepRandomEngine * anEngine)
: localEngine(anEngine), deleteEngine(true), set(false),
  nextGauss(0.0) {}

inline HepDouble RandGauss::shoot(HepDouble mean, HepDouble stdDev) {
  return shoot()*stdDev + mean;
}

inline HepDouble RandGauss::shoot(HepRandomEngine* anEngine,
                                  HepDouble mean, HepDouble stdDev) {
  return shoot(anEngine)*stdDev + mean;
}

inline HepDouble RandGauss::fire(HepDouble mean, HepDouble stdDev) {
  return fire()*stdDev + mean;
}

#endif
