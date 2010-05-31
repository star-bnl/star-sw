#include <iostream>
#include "RandomEngineGSL.hh"

namespace Garfield {

RandomEngineGSL randomEngine;

RandomEngineGSL::RandomEngineGSL() {

  // Setup the random generator
  const gsl_rng_type* t;
  gsl_rng_env_setup();
  t = gsl_rng_default;
  rng = gsl_rng_alloc(t);

  std::cout << "RandomEngineGSL:" << std::endl;
  std::cout << "    Generator type: " << gsl_rng_name(rng) << std::endl;
  std::cout << "    Seed: " << gsl_rng_default_seed << std::endl;

}

RandomEngineGSL::~RandomEngineGSL() {

  gsl_rng_free(rng);
  
}

void
RandomEngineGSL::Seed(unsigned int s) {

  gsl_rng_set(rng, s);
  
}

}

