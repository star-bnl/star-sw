/***************************************************************************
 *
 * $Id: JamesRandom.h,v 1.1 1999/01/30 03:58:59 fisyak Exp $
 *
 * Author:  Gabriele Cosmo - Created: 5th September 1995
 *          modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *              JamesRandom.h,v 1.6 1997/07/12 21:05:56
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- HepJamesRandom ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * HepJamesRandom implements the algorithm by Marsaglia-Zaman RANMAR
 * described in "F.James, Comp. Phys. Comm. 60 (1990) 329" and implemented
 * in FORTRAN77 as part of the MATHLIB HEP library for pseudo-random
 * numbers generation.
 * This is the default random engine invoked by each distribution unless
 * the user sets a different one.
 *
 ***************************************************************************
 *
 * $Log: JamesRandom.h,v $
 * Revision 1.1  1999/01/30 03:58:59  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:33  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef HepJamesRandom_h
#define HepJamesRandom_h 1

#include "RandomEngine.h"

class HepJamesRandom: public HepRandomEngine {

public:

  HepJamesRandom(long seed = 19780503);
  ~HepJamesRandom();
  // Constructor and destructor.

  HepJamesRandom(const HepJamesRandom &p);
  // Copy constructor

  HepJamesRandom & operator = (const HepJamesRandom &p);
  // Overloaded assignment operator, to retrieve the engine status.

  HepDouble flat();
  // Returns a pseudo random number between 0 and 1 
  // (excluding the end points)

  void flatArray (const HepInt size, HepDouble* vect);
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void flatArray (vector<HepDouble>&);
#else
    void flatArray (vector<HepDouble, allocator<HepDouble> >&);
#endif
  // Fills the array "vect" of specified size with flat random values.

  void setSeed(long seed, HepInt dum=0);
  // Sets the state of the algorithm according to seed.

  void setSeeds(const long * seeds, HepInt dum=0);
  // Sets the state of the algorithm according to the zero terminated
  // array of seeds. Only the first seed is used.

  void saveStatus() const;
  // Saves on file JamesRand.conf the current engine status.

  void restoreStatus();
  // Reads from file JamesRand.conf the last saved engine status
  // and restores it.

  void showStatus() const;
  // Dumps the engine status on the screen.

private:

  // Members defining the current status of the generator.
  HepDouble u[97];
  HepDouble c, cd, cm;
  HepDouble *pi97, *pj97;

};

#endif
