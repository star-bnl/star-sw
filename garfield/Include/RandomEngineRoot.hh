#ifndef G_RANDOM_ENGINE_ROOT_H
#define G_RANDOM_ENGINE_ROOT_H

#include <TRandom3.h>

#include "RandomEngine.hh"

namespace Garfield {

/// ROOT random number generator.

class RandomEngineRoot : public RandomEngine {

 public:
  // Constructor
  RandomEngineRoot();
  // Destructor
  ~RandomEngineRoot();
  // Call the random number generator
  double Draw() { return rng.Rndm(); }
  // Initialise the random number generator
  void Seed(unsigned int s);

 private:
  TRandom3 rng;
};
}

#endif
