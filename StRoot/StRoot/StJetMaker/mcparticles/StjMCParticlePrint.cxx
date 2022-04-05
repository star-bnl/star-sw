// $Id: StjMCParticlePrint.cxx,v 1.1 2008/11/27 07:40:06 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjMCParticlePrint.h"

#include <iostream>

ClassImp(StjMCParticlePrint)

using namespace std;

void StjMCParticlePrint::operator()(const StjMCParticleList &mcList)
{
  for(StjMCParticleList::const_iterator it = mcList.begin(); it != mcList.end(); ++it) {
    print(*it);
  }
}

void StjMCParticlePrint::print(const StjMCParticle& mc)
{
  cout 
    << mc.runNumber         << " "
    << mc.eventId           << " "
    << mc.mcparticleId      << " "
    << mc.pdg               << " "
    << mc.firstMotherId     << " "
    << mc.lastMotherId      << " "
    << mc.firstDaughterId   << " "
    << mc.lastDaughterId    << " "
    << mc.pt                << " "
    << mc.eta               << " "
    << mc.phi               << " "
    << mc.m                 << " "
    << mc.e                 << " "
    << mc.status            << " "
    << mc.vertexZ           << " "
    << endl;

}
