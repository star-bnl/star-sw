// Abstract base class for random number generators

#ifndef G_RANDOM_ENGINE_H
#define G_RANDOM_ENGINE_H

namespace Garfield {

class RandomEngine {

 public:
  // Constructor
  RandomEngine() {}
  // Destructor
  virtual ~RandomEngine() {}

  // Draw a random number
  virtual double Draw() = 0;
  // Initialise the random number generator
  virtual void Seed(unsigned int s) = 0;
};
}

#endif
