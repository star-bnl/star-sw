/***************************************************************************
 *
 * $Id: RanecuEngine.h,v 1.2 1999/12/07 23:43:04 ullrich Exp $
 *
 * Author: Gabriele Cosmo - Created: 2nd February 1996
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *           RanecuEngine.h,v 1.5 1997/10/16 20:40:27
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                         --- RanecuEngine ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * RANECU Random Engine - algorithm originally written in FORTRAN77
 *                        as part of the MATHLIB HEP library.
 * Seeds are taken from SeedTable given an index, the getSeed() method
 * returns the current index of SeedTable, while the getSeeds() method
 * returns a pointer to the local table of seeds at the current index.
 *
 ***************************************************************************
 *
 * $Log: RanecuEngine.h,v $
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:43  ullrich
 * Initial Revision
 *
 **************************************************************************/

#ifndef RanecuEngine_h
#define RanecuEngine_h 1

#include "RandomEngine.h"

class RanecuEngine : public HepRandomEngine {

public:

  RanecuEngine(HepInt index=0);
  ~RanecuEngine();
  // Constructor and destructor.

  RanecuEngine(const RanecuEngine &p);
  // Copy constructor

  RanecuEngine & operator = (const RanecuEngine &p);
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
  // Fills an array "vect" of specified size with flat random values.

  void setSeed (long index, HepInt dum=0);
  // Sets the state of the algorithm according to "index", the position
  // in the table of seeds.

  void setSeeds (const long* seeds, HepInt index=-1);
  // Sets the state of the algorithm according to the array of seeds
  // "seeds" containing two seed values to be stored in the table at
  // "index" position.

  void saveStatus() const;
  // Saves on file Ranecu.conf the current engine status.

  void restoreStatus();
  // Reads from file Ranecu.conf the last saved engine status
  // and restores it.

  void showStatus() const;
  // Dumps the engine status on the screen.

private:

  // Members defining the current state of the generator.

  long table[215][2];
  HepInt seq;
  const HepInt ecuyer_a, ecuyer_b, ecuyer_c, ecuyer_d, ecuyer_e, ecuyer_f;
  const HepInt shift1, shift2;
  const HepInt maxSeq;
  const HepDouble prec;

};

#endif
