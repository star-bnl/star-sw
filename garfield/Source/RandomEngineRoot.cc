#include <iostream>
#include "RandomEngineRoot.hh"

namespace Garfield {

RandomEngineRoot randomEngine;

RandomEngineRoot::RandomEngineRoot() 
  : RandomEngine(), 
    m_rng(0) {

  std::cout << "RandomEngineRoot:\n"
            << "    Generator type: TRandom3\n"
            << "    Seed: " << m_rng.GetSeed() << "\n";
}

RandomEngineRoot::~RandomEngineRoot() {}

void RandomEngineRoot::Seed(const unsigned int s) {

  m_rng.SetSeed(s);
  std::cout << "RandomEngineRoot::Seed:\n"
            << "    Seed: " << m_rng.GetSeed() << "\n";
}
}
