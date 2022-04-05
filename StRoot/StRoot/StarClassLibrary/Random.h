/***************************************************************************
 *
 * $Id: Random.h,v 1.2 1999/12/07 23:43:04 ullrich Exp $
 *
 * Author:  Gabriele Cosmo - Created: 5th Sep 1995
 *          modified SCL bl
 ***************************************************************************
 *
 * Description:
 *             Random.h,v 1.7 1997/10/16 20:40:24
 * -----------------------------------------------------------------------
 *                           HEP Random
 *                          --- HepRandom ---
 *                          class header file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 * An object of this class is instantiated by default within the HEP Random
 * module and uses an instantiated HepJamesRandom engine as default algorithm
 * for pseudo-random number generation. HepRandom defines a static private
 * data member theGenerator and a set of static inlined methods to manipulate
 * it. By means of theGenerator the user can change the underlying engine
 * algorithm, get and set the seeds and use any kind of defined random
 * distribution.
 * Distribution classes inherit from HepRandom and define both static and
 * not-static interfaces.
 *
 ***************************************************************************
 *
 * $Log: Random.h,v $
 * Revision 1.2  1999/12/07 23:43:04  ullrich
 * Modified to get rid of warnings on Linux.
 *
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef HepRandom_h
#define HepRandom_h 1

#include <vector>

#include "RandomEngine.h"

class HepRandom {

public:

  HepRandom();
  HepRandom(long seed);
  // Contructors with and without a seed using the default engine
  // (JamesRandom).

  HepRandom(HepRandomEngine & algorithm);
  HepRandom(HepRandomEngine * algorithm);
  // Constructor taking an alternative engine as argument. If a pointer is
  // given the corresponding object will be deleted by the HepRandom
  // destructor.
  
  virtual ~HepRandom();
  // Destructor

  inline HepDouble flat();
  // Returns the flat value ( interval ]0.1[ ).

  inline HepDouble flat (HepRandomEngine* theNewEngine);
  // Returns a flat value, given a defined Random Engine.

  inline void flatArray(const HepInt size, HepDouble* vect);
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    void flatArray(vector<HepDouble>&);
#else
    void flatArray(vector<HepDouble, allocator<HepDouble> >&);
#endif
    // Fills "vect" array of flat random values, given the size.

  inline void flatArray(HepRandomEngine* theNewEngine, 
                        const HepInt size, HepDouble* vect);
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    inline void flatArray(HepRandomEngine* theNewEngine, 
			  vector<HepDouble>& vec);
#else
    inline void flatArray(HepRandomEngine* theNewEngine, 
			  vector<HepDouble, allocator<HepDouble> >& vec);
#endif
  // Fills "vect" array of flat random values, given the size
  // and a defined Random Engine.

  virtual HepDouble operator()();
  // To get a flat random number using the operator ().

  // --------------------------------------------------
  // Static member functions using the static generator
  // --------------------------------------------------

     static void setTheSeed(long seed, HepInt lux=3);
     // The static definition for setSeed().

     static long getTheSeed();
     // The static definition for getSeed().

     static void setTheSeeds(const long* seeds, HepInt aux=-1);
     // The static definition for setSeeds().

     static const long* getTheSeeds();
     // The static definition for getSeeds().

     static void getTheTableSeeds (long* seeds, HepInt index);
     // The static definition for getTableSeeds().

     static HepRandom * getTheGenerator() { return theGenerator; }
     // Return the current static generator.

     static void setTheEngine (HepRandomEngine* theNewEngine);
     // The static definition for setEngine()

     static HepRandomEngine * getTheEngine();
     // The static definition for getEngine().

     static void saveEngineStatus();
     // The static definition for saveStatus().

     static void restoreEngineStatus();
     // The static definition for restoreStatus().

     static void showEngineStatus();
     // The static definition for showStatus().

  // ------------------------------------------------------------
  // Utility methods to get/set attributes for Gauss distribution
  // ------------------------------------------------------------

     void setFlag( HepBoolean val ){set = val;}

     HepBoolean getFlag() {return set;}

     void setVal( HepDouble nextVal ){nextGauss = nextVal;}

     HepDouble getVal() {return nextGauss;}

  // --------------------------------------------------------------
  // Utility methods to get/set attributes for Poisson distribution
  // --------------------------------------------------------------

     void setOldMean( HepDouble val ){oldm = val;}

     HepDouble getOldMean() {return oldm;}

     HepDouble getMaxMean() {return meanMax;}

     void setPStatus(HepDouble sq, HepDouble alxm, HepDouble g) {
        status[0] = sq; status[1] = alxm; status[2] = g;
     }

     HepDouble* getPStatus() {return status;}

private:       // -------- Methods ---------

  inline void setSeed(long seed, HepInt lux);
  // (Re)Initializes the generator with a seed.

  inline long getSeed() const;
  // Gets the current seed of the current generator.

  inline void setSeeds(const long* seeds, HepInt aux);
  // (Re)Initializes the generator with a zero terminated list of seeds.

  inline const long* getSeeds () const;
  // Gets the current array of seeds of the current generator.

  inline void getTableSeeds (long* seeds, HepInt index) const;
  // Gets the array of seeds from the global seedTable at "index" position.

  void setEngine (HepRandomEngine* engine) { theEngine = engine; }
  // To set the underlying algorithm object

  HepRandomEngine * getEngine() const { return theEngine; }
  // Returns a pointer to the underlying algorithm object.

  void saveStatus() const;
  // Saves to file the current status of the current engine.

  void restoreStatus();
  // Restores a saved status (if any) for the current engine.

  void showStatus() const;
  // Dumps the current engine status on screen.

protected:     // -------- Data members ---------

  HepRandomEngine * theEngine;
  // The corresponding algorithm.

  HepBoolean deleteEngine;
  // True if the engine should be deleted on destruction.

  HepBoolean set;

  HepDouble status[3], oldm;
  HepDouble nextGauss;
  // For Gaussian random numbers which are generated two at a time.

  const HepDouble meanMax;
  // For Poisson, to re-initialize if mean is the same as the previous.

private:       // -------- Data members ---------

  static HepRandom * theGenerator;
  // The common shared static generator
};

inline void HepRandom::setSeed(long seed, HepInt lux) {
  theEngine->setSeed(seed,lux);
}

inline void HepRandom::setSeeds(const long* seeds, HepInt aux) {
  theEngine->setSeeds(seeds,aux);
}

inline long HepRandom::getSeed() const {
  return theEngine->getSeed();
}

inline const long* HepRandom::getSeeds() const {
  return theEngine->getSeeds();
}

inline void HepRandom::getTableSeeds(long* seeds, HepInt index) const {
  theEngine->getTableSeeds(seeds,index);
}

inline void HepRandom::saveStatus() const {
  theEngine->saveStatus();
}

inline void HepRandom::restoreStatus() {
  theEngine->restoreStatus();
}

inline void HepRandom::showStatus() const {
  theEngine->showStatus();
}

inline HepDouble HepRandom::flat() {
  return theEngine->flat();
}

inline void HepRandom::flatArray(const HepInt size, HepDouble* vect) {
  theEngine->flatArray(size,vect);
}

#ifdef ST_NO_TEMPLATE_DEF_ARGS
inline void HepRandom::flatArray(vector<HepDouble, allocator<HepDouble> >& vec)
#else
inline void HepRandom::flatArray(vector<HepDouble>& vec)
#endif
{    
    theEngine->flatArray(vec);
}

inline HepDouble HepRandom::flat(HepRandomEngine* theNewEngine)
{
  return theNewEngine->flat();
}

inline void HepRandom::flatArray(HepRandomEngine* theNewEngine, 
                                 const HepInt size, HepDouble* vect)
{
  theNewEngine->flatArray(size,vect);
}

#ifdef ST_NO_TEMPLATE_DEF_ARGS
inline void HepRandom::flatArray(HepRandomEngine* theNewEngine, 
                                 vector<HepDouble, allocator<HepDouble> >& vec)
#else
inline void HepRandom::flatArray(HepRandomEngine* theNewEngine, 
                                 vector<HepDouble>& vec)
#endif
{
  theNewEngine->flatArray(vec);
}

#endif
