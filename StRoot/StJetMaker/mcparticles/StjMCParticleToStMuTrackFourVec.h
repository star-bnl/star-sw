// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 26 Jan 2011
//

#ifndef STJ_MC_PARTICLE_TO_ST_MU_TRACK_FOUR_VEC_H
#define STJ_MC_PARTICLE_TO_ST_MU_TRACK_FOUR_VEC_H

#include "TLorentzVector.h"
#include "StMuTrackFourVec.h"
#include "StjMCParticleList.h"

struct StjMCParticleToStMuTrackFourVec {
  StMuTrackFourVec* operator()(const StjMCParticle& mcparticle) const
  {
    TLorentzVector p;
    p.SetPtEtaPhiM(mcparticle.pt,mcparticle.eta,mcparticle.phi,mcparticle.m);
    StMcTrackEmu* mctrack = new StMcTrackEmu;
    mctrack->_pt     = mcparticle.pt;
    mctrack->_eta    = mcparticle.eta;
    mctrack->_phi    = mcparticle.phi;
    mctrack->_m      = mcparticle.m;
    mctrack->_e      = mcparticle.e;
    mctrack->_id     = mcparticle.mcparticleId;
    mctrack->_pdg    = mcparticle.pdg;
    mctrack->_status = mcparticle.status;
    mctrack->_firstMother   = mcparticle.firstMotherId;
    mctrack->_lastMother    = mcparticle.lastMotherId;
    mctrack->_firstDaughter = mcparticle.firstDaughterId;
    mctrack->_lastDaughter  = mcparticle.lastDaughterId;
    return new StMuTrackFourVec(0,0,mctrack,p,0,mcparticle.mcparticleId-1,kUnknownId);
  }
};

#endif	// STJ_MC_PARTICLE_TO_ST_MU_TRACK_FOUR_VEC_H
