#ifndef PARTICLEBANK_H
#define PARTICLEBANK_H

#include "wcpplib/safetl/AbsList.h"
#include "wcpplib/geometry/gparticle.h"

extern AbsList< ActivePtr< gparticle > > particle_bank;



void treat_particle_bank(int s_erase = 1); // s_erase is signature
// that the particle should be delepet from particle bank after the tracking.
// Otherwise (0) it is not deleted, which makes possible, for example,
// further printing or other actions



#endif
