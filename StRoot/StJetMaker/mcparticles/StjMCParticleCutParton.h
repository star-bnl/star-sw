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
  bool operator()(const StjMCParticle&);

  ClassDef(StjMCParticleCutParton,1)
};

#endif // STJ_MCPARTICLE_CUT_PARTON_H
