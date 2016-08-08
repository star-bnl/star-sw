#ifndef PARTICLEBANK_H
#define PARTICLEBANK_H

#include <list>
#include "wcpplib/geometry/gparticle.h"

namespace Heed {

extern std::list<ActivePtr<gparticle> > particle_bank;

void treat_particle_bank(const int s_erase = 1);  // s_erase is signature
// that the particle should be deleted from particle bank after the tracking.
// Otherwise (0) it is not deleted, which makes possible, for example,
// further printing or other actions

}

#endif
