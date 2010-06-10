#include <iostream>
#include "RandomEngineRoot.hh"

namespace Garfield {

RandomEngineRoot randomEngine;

RandomEngineRoot::RandomEngineRoot() : rng(0) {

  std::cout << "RandomEngineRoot:" << std::endl;
  std::cout << "    Generator type: TRandom3" << std::endl;
  std::cout << "    Seed: " << rng.GetSeed() << std::endl;

}

void
RandomEngineRoot::Seed(unsigned int s) {

  rng.SetSeed(s);
  
}

}

