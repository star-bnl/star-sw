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

  //
  // I want the fragmented partons from the hard scattering
  // including initial and final state radiation, but without
  // the underlying event contribution. That is the physics
  // object that closely matches theoretical NLO calculations.
  // PYTHIA 6.4 CDF tune A assigns parent 0 to any parton from
  // the underlying event. However, PYTHIA 6.4 Perugia-0 tune
  // assigns the originating proton as parent (1 or 2) to any
  // parton from the underlying event. To handle both tunes,
  // I reject any parton with parent 0, 1 or 2.
  //
  bool operator()(const StjMCParticle& particle)
  {
    return !(particle.mcparticleId  >  mstu72 &&
	     particle.mcparticleId  <= mstu73 &&
	     particle.firstMotherId <= mstu72 &&
	     particle.firstMotherId != 0 &&
	     particle.firstMotherId != 1 &&
	     particle.firstMotherId != 2 &&
	     particle.status != 51 &&
	     particle.pt > 0.0001 && fabs(particle.eta) < 5.0);
  }

  ClassDef(StjMCParticleCutParton,1)
};

#endif // STJ_MCPARTICLE_CUT_PARTON_H
