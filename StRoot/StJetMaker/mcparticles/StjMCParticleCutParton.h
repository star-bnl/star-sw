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

#include "StjMCParticleCut.h"

class StjMCParticleCutParton : public StjMCParticleCut {
public:
  static int mstu72;		// set by StjMCMuDst
  static int mstu73;		// set by StjMCMuDst

  bool operator()(const StjMCParticle& particle)
  {
    return particle.mcparticleId <= mstu72 || particle.mcparticleId > mstu73;
  }

  ClassDef(StjMCParticleCutParton,1)
};

#endif // STJ_MCPARTICLE_CUT_PARTON_H
