/***************************************************************************
 *
 * $Id: RandEngine.h,v 1.1 1999/01/30 03:58:59 fisyak Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *               RandEngine.h,v 1.3 1997/07/12 21:05:57
 *
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                         --- RandEngine ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Random engine using rand() and srand() functions from C standard
 * library to implement the flat() basic distribution and for setting
 * seeds.
 *
 ***************************************************************************
 *
 * $Log: RandEngine.h,v $
 * Revision 1.1  1999/01/30 03:58:59  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:36  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef RandEngine_h
#define RandEngine_h 1

#include "RandomEngine.h"

class RandEngine : public HepRandomEngine {

public:

  RandEngine(long seed = 19780503);
  ~RandEngine();
  // Constructor and destructor

  RandEngine(const RandEngine &p);
  // Copy constructor

  RandEngine & operator = (const RandEngine &p);
  // Overloaded assignment operator, to retrieve the engine status.

  HepDouble flat();
  // It returns a pseudo random number between 0 and 1,
  // according to the standard stdlib random function rand()
  // but excluding the end points.

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
  // Saves on file Rand.conf the current engine status.

  void restoreStatus();
  // Reads from file Rand.conf the last saved engine status
  // and restores it.

  void showStatus() const;
  // Dumps the engine status on the screen.

private:

  const HepDouble mx;
  long seq;

};

#endif
