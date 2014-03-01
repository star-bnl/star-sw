#include <iostream>
#include "RandomEngineRoot.hh"

namespace Garfield {

RandomEngineRoot randomEngine;

RandomEngineRoot::RandomEngineRoot() : rng(0) {

  std::cout << "RandomEngineRoot:\n";
  std::cout << "    Generator type: TRandom3\n";
  std::cout << "    Seed: " << rng.GetSeed() << "\n";
}

RandomEngineRoot::~RandomEngineRoot() {}

void RandomEngineRoot::Seed(unsigned int s) {

  rng.SetSeed(s);
  std::cout << "RandomEngineRoot::Seed:\n";
  std::cout << "    Seed: " << rng.GetSeed() << "\n";
}
}
