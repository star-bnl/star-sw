#ifndef RANLUXINT_H
#define RANLUXINT_H

#ifdef GARFIELD_HEED_INTERFACE

#include "Random.hh"

namespace Heed {

inline double SRANLUX() { return Garfield::RndmUniform(); }

}

#else

#ifdef USE_CPP_SRANLUX

#include "wcpplib/random/WRandomEngine.h"
//#include <CLHEP/Random/RandomEngine.h>

// using namespace CLHEP;  // uncomment if below is CLHEP variant.

extern WHepRandomEngine& random_engine;

inline double SRANLUX(void) { return random_engine.flat(); }

#endif  // for  ifdef USE_CPP_RANLUX

#endif

#endif
