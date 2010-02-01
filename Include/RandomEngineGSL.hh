// GSL random number generator

#ifndef G_RANDOM_ENGINE_GSL_H
#define G_RANDOM_ENGINE_GSL_H

#include <gsl/gsl_errno.h>
#include <gsl/gsl_rng.h>

#include "RandomEngine.hh"

namespace Garfield {

class RandomEngineGSL : public RandomEngine {

  public:
    // Constructor
    RandomEngineGSL();
    // Destructor    
    ~RandomEngineGSL();    
    // Call the random number generator
    double Draw() {return gsl_rng_uniform(rng);}
    // Initialise the random number generator
    void Seed(unsigned int s);
    
  private:
    gsl_rng* rng;
  
};

}

#endif
