/***************************************************************************
 *
 * $Id: Random.cc,v 1.1 1999/01/30 03:59:00 fisyak Exp $
 *
 * Author: Gabriele Cosmo - Created: 5th September 1995
 *         modified for SCL bl
 ***************************************************************************
 *
 * Description:
 *               Random.cc,v 1.15 1997/08/12 00:38:47
 * -----------------------------------------------------------------------
 *                             HEP Random
 *                          --- HepRandom ---
 *                      class implementation file
 * -----------------------------------------------------------------------
 * This file is part of Geant4 (simulation toolkit for HEP).
 *
 ***************************************************************************
 *
 * $Log: Random.cc,v $
 * Revision 1.1  1999/01/30 03:59:00  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:29:10  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "JamesRandom.h"
#include "Random.h"

// -----------------------------
// Static members initialisation
// -----------------------------

HepJamesRandom theMainEngine;
HepRandom aGenerator(theMainEngine);
HepRandom* HepRandom::theGenerator = &aGenerator;

//---------------------------- HepRandom ---------------------------------

HepRandom::HepRandom()
: theEngine(new HepJamesRandom), deleteEngine(true),
  set(false), oldm(-1.0), nextGauss(0.0), meanMax(2.0E9) {
  for (HepInt i=0; i<3; ++i)
    status[i] = 0.;
}

HepRandom::HepRandom(long seed)
: theEngine(new HepJamesRandom(seed)), deleteEngine(true),
  set(false), oldm(-1.0), nextGauss(0.0), meanMax(2.0E9) {
  for (HepInt i=0; i<3; ++i)
    status[i] = 0.;
}

HepRandom::HepRandom(HepRandomEngine & algorithm)
: theEngine(&algorithm), deleteEngine(false),
  set(false), oldm(-1.0), nextGauss(0.0), meanMax(2.0E9) {
    // called always because it is static
    for (HepInt i=0; i<3; ++i)
    status[i] = 0.;
}

HepRandom::HepRandom(HepRandomEngine * algorithm)
: theEngine(algorithm), deleteEngine(true),
  set(false), oldm(-1.0), nextGauss(0.0), meanMax(2.0E9) {
  for (HepInt i=0; i<3; ++i)
    status[i] = 0.;
}

HepRandom::~HepRandom() {
  if ( deleteEngine ) delete theEngine;
}

HepDouble HepRandom::operator()() {
  return flat();
}

// --------------------------
// Static methods definitions
// --------------------------

void HepRandom::setTheSeed(long seed, HepInt lux)
{
  theGenerator->setSeed(seed,lux);
}

long HepRandom::getTheSeed()
{
  return theGenerator->getSeed();
}

void HepRandom::setTheSeeds(const long* seeds, HepInt aux)
{
  theGenerator->setSeeds(seeds,aux);
}

const long* HepRandom::getTheSeeds ()
{
  return theGenerator->getSeeds();
}

void HepRandom::getTheTableSeeds (long* seeds, HepInt index)
{
  theGenerator->getTableSeeds(seeds, index);
}

void HepRandom::setTheEngine (HepRandomEngine* theNewEngine)
{
  theGenerator->setEngine(theNewEngine);
}

HepRandomEngine * HepRandom::getTheEngine()
{
  return theGenerator->getEngine();
}  

void HepRandom::saveEngineStatus()
{
  theGenerator->saveStatus();
}  

void HepRandom::restoreEngineStatus()
{
  theGenerator->restoreStatus();
}  

void HepRandom::showEngineStatus()
{
  theGenerator->showStatus();
}  
