/***************************************************************************
 *
 * $Id: RandBreitWigner.h,v 1.1 1999/01/30 03:58:59 fisyak Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- RandBreitWigner ---
 *                           class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Class defining methods for shooting numbers according to the
 * Breit-Wigner distribution algorithms (plain or mean^2).
 * Default values are set: mean=1, gamma=.2, cut=1.
 * Plain algorithm is used for shootArray() and fireArray().
 * Plain algorithm with default values is used for operator()(). 
 *
 ***************************************************************************
 *
 * $Log: RandBreitWigner.h,v $
 * Revision 1.1  1999/01/30 03:58:59  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:35  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef RandBreitWigner_h
#define RandBreitWigner_h 1

#include "RandFlat.h"

class RandBreitWigner : public HepRandom {

public:

  inline RandBreitWigner ( HepRandomEngine& anEngine );
  inline RandBreitWigner ( HepRandomEngine* anEngine );
  // These constructors should be used to instantiate a RandBreitWigner
  // distribution object defining a local engine for it.
  // The static generator will be skeeped using the non-static methods
  // defined below.
  // If the engine is passed by pointer the corresponding engine object
  // will be deleted by the RandBreitWigner destructor.
  // If the engine is passed by reference the corresponding engine object
  // will not be deleted by the RandBreitWigner destructor.

  virtual ~RandBreitWigner();
  // Destructor

  // Static methods to shoot random values using the static generator

  static  HepDouble shoot( HepDouble a=1.0, HepDouble b=0.2 );

  static  HepDouble shoot( HepDouble a, HepDouble b, HepDouble c );

  static  HepDouble shootM2( HepDouble a=1.0, HepDouble b=0.2 );

  static  HepDouble shootM2( HepDouble a, HepDouble b, HepDouble c );

  static  void shootArray ( const HepInt size, HepDouble* vect,
                            HepDouble a=1.0, HepDouble b=0.2, HepDouble c=1.0 );

#ifndef ST_NO_TEMPLATE_DEF_ARGS
     static  void shootArray ( vector<HepDouble>& vec, HepDouble a=1.0,
			       HepDouble b=0.2,  HepDouble c=1.0 );
#else
    static  void shootArray ( vector<HepDouble, allocator<HepDouble> >& vec,
			      HepDouble a=1.0,
			      HepDouble b=0.2,  HepDouble c=1.0 );
#endif
  //  Static methods to shoot random values using a given engine
  //  by-passing the static generator.

  static  HepDouble shoot( HepRandomEngine* anEngine, HepDouble a=1.0,
                           HepDouble=0.2 );
  static  HepDouble shoot( HepRandomEngine* anEngine, HepDouble a,
                           HepDouble b, HepDouble c );
  static  HepDouble shootM2( HepRandomEngine* anEngine, HepDouble a=1.0,
                             HepDouble b=0.2 );
  static  HepDouble shootM2( HepRandomEngine* anEngine, HepDouble a,
                             HepDouble b, HepDouble c );
  static  void shootArray ( HepRandomEngine* anEngine,
                            const HepInt size, HepDouble* vect,
                            HepDouble a=1.0, HepDouble b=0.2, HepDouble c=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    static  void shootArray ( HepRandomEngine*,
			      vector<HepDouble>&, HepDouble a=1.0,
			      HepDouble b=0.2, HepDouble c=1.0 );
#else
    static  void shootArray ( HepRandomEngine*,
			      vector<HepDouble, allocator<HepDouble> >&,
			      HepDouble a=1.0,
			      HepDouble b=0.2, HepDouble c=1.0 );
#endif
  //  Methods using the localEngine to shoot random values, by-passing
  //  the static generator.

  HepDouble fire( HepDouble a=1.0, HepDouble b=0.2 );

  HepDouble fire( HepDouble a, HepDouble b, HepDouble c );

  HepDouble fireM2( HepDouble a=1.0, HepDouble b=0.2 );

  HepDouble fireM2( HepDouble a, HepDouble b, HepDouble c );

  void fireArray ( const HepInt size, HepDouble* vect,
                   HepDouble a=1.0, HepDouble b=0.2, HepDouble c=1.0 );
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void fireArray ( vector<HepDouble>&,
		     HepDouble a=1.0, HepDouble b=0.2, HepDouble c=1.0 );
#else
    void fireArray ( vector<HepDouble, allocator<HepDouble> >&,
		     HepDouble a=1.0, HepDouble b=0.2, HepDouble c=1.0 );
#endif
  HepDouble operator()();
  
private:

  HepRandomEngine* localEngine;
  HepBoolean deleteEngine;

};

// ------------------
// Inlined functions
// ------------------

inline RandBreitWigner::RandBreitWigner(HepRandomEngine & anEngine)
: localEngine(&anEngine), deleteEngine(false) {}

inline RandBreitWigner::RandBreitWigner(HepRandomEngine * anEngine)
: localEngine(anEngine), deleteEngine(true) {}

#endif
