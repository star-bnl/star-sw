/***************************************************************************
 *
 * $Id: RandomEngine.h,v 1.4 2003/09/02 17:59:34 perev Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                        --- HepRandomEngine ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * Is the abstract class defining the interface for each random engine. It
 * implements the getSeed() and getSeeds() methods which return the initial
 * seed value and the initial array of seeds respectively. It defines 7
 * pure virtual functions: flat(), flatArray(), setSeed(), setSeeds(),
 * saveStatus(), restoreStatus() and showStatus(), which are implemented by
 * the concrete random engines each one inheriting from this abstract class.
 * Many concrete random engines can be defined and added to the structure,
 * simply making them inheriting from HepRandomEngine and defining the six
 * functions flat(), flatArray(), setSeed(), setSeeds(), saveStatus(),
 * restoreStatus() and showStatus() in such a way that flat() and
 * flatArray() return double random values ranging between ]0,1[.
 * All the random engines have a default seed value already set but they
 * can be instantiated with a different seed value set up by the user.
 * The seed can be changed using a static method defined in HepRandom.
 *
 ***************************************************************************
 *
 * $Log: RandomEngine.h,v $
 * Revision 1.4  2003/09/02 17:59:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  1999/12/21 15:13:56  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/03/07 15:02:50  wenaus
 * missing std
 *
 * Revision 1.1  1999/01/30 03:59:01  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef HepRandomEngine_h
#define HepRandomEngine_h 1

#include <Stiostream.h>
#include "Stiostream.h"
#include <math.h>
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

#include "StGlobals.hh"

class HepRandomEngine {

public:

  HepRandomEngine();
  virtual ~HepRandomEngine();
  // Constructor and destructor

  inline HepBoolean operator==(const HepRandomEngine& engine);
  inline HepBoolean operator!=(const HepRandomEngine& engine);
  // Overloaded operators, ==, !=

  virtual HepDouble flat() = 0;
  // Should return a pseudo random number between 0 and 1 
  // (excluding the end points)

  virtual void flatArray(const HepInt size, HepDouble* vect) = 0;
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    virtual void flatArray(vector<HepDouble>&) = 0;
#else
    virtual void flatArray(vector<HepDouble,allocator<HepDouble> >&) = 0;
#endif
  // Fills an array "vect" of specified size with flat random values.

  virtual void setSeed(long seed, HepInt) = 0;
  // Should initialise the status of the algorithm according to seed.

  virtual void setSeeds(const long * seeds, HepInt) = 0;
  // Should initialise the status of the algorithm according to the zero terminated
  // array of seeds. It is allowed to ignore one or many seeds in this array.

  virtual void saveStatus() const = 0;
  // Should save on a file specific to the instantiated engine in use
  // the current status.

  virtual void restoreStatus() = 0;
  // Should read from a file (specific to the instantiated engine in use)
  // and restore the last saved engine configuration.

  virtual void showStatus() const = 0;
  // Should dump the current engine status on the screen.

  long getSeed() const { return theSeed; }
  // Gets the current seed.

  const long* getSeeds() const { return theSeeds; }
  // Gets the current array of seeds.

  void getTableSeeds(long* seeds, HepInt index) const;
  // Gets back seed values stored in the table, given the index.

protected:

  long theSeed;
  const long* theSeeds;
  static const long seedTable[215][2];
};

inline HepBoolean HepRandomEngine::operator==(const HepRandomEngine& engine) {
  return (this==&engine) ? true : false;
}

inline HepBoolean HepRandomEngine::operator!=(const HepRandomEngine& engine) {
  return (this!=&engine) ? true : false;
}

inline void HepRandomEngine::getTableSeeds(long* seeds, HepInt index) const
{
  if ((index >= 0) && (index < 215)) {
    seeds[0] = seedTable[index][0];
    seeds[1] = seedTable[index][1];
  }
  else seeds = NULL;
}

#endif
