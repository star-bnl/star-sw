// $Id: StjMCParticleToFourVec.cxx,v 1.1 2008/11/27 07:40:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjMCParticleToFourVec.h"

ClassImp(StjMCParticleToFourVec)

#include "StjMCParticleList.h"
#include "StjFourVecList.h"

StjFourVec StjMCParticleToFourVec::operator()(const StjMCParticle& mcparticle)
{
  StjFourVec ret;
  ret.runNumber   = mcparticle.runNumber;
  ret.eventId     = mcparticle.eventId;
  ret.type        = 0;     
  ret.detectorId  = 0;
  ret.mcparticleId = mcparticle.mcparticleId;
  ret.trackId     = 0;
  ret.towerId     = 0;
  ret.vertexZ     = mcparticle.vertexZ;

  ret.pt  = mcparticle.pt;
  ret.eta = mcparticle.eta;
  ret.phi = mcparticle.phi;
  ret.m   = mcparticle.m;
  return ret;
}
