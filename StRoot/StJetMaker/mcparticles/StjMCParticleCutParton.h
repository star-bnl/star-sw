// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 20 Jan 2012
//
// Select particles for parton jets
//

#ifndef STJ_MCPARTICLE_CUT_PARTON_H
#define STJ_MCPARTICLE_CUT_PARTON_H

#include <cmath>
#include "StjMCParticleCut.h"

class StjMCParticleCutParton : public StjMCParticleCut {
public:
  static int mstu72;		// set by StjMCMuDst
  static int mstu73;		// set by StjMCMuDst

  bool operator()(const StjMCParticle& particle)
  {
    return !(particle.mcparticleId > mstu72 && particle.mcparticleId <= mstu73 && particle.firstMotherId <= mstu72 && particle.status != 51 && particle.pt > 0.0001 && fabs(particle.eta) < 5);
  }

  ClassDef(StjMCParticleCutParton,1)
};

#endif // STJ_MCPARTICLE_CUT_PARTON_H
