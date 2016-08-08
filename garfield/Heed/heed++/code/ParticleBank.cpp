#include "heed++/code/ParticleBank.h"

namespace Heed {

void treat_particle_bank(const int s_erase) {
  mfunname("void treat_particle_bank(int s_erase)");
  std::list<ActivePtr<gparticle> >::iterator it = particle_bank.begin();
  while (it != particle_bank.end()) {
    (*it).get()->fly();
    if (s_erase == 1) {
      it = particle_bank.erase(it);
    } else {
      ++it;
    }
  }
}

}
