// $Id: StJetMCMuDst.cxx,v 1.1 2008/07/22 05:06:46 tai Exp $

#include <StJetMCMuDst.h>

#include <StSpinPool/StMCAsymMaker/StMCAsymMaker.h>

#include <tables/St_particle_Table.h>

#include <TLorentzVector.h>

namespace StSpinJet {

MCParticleList StJetMCMuDst::getMCPartilceList()
{
  StMcEvent* mcEvent = (StMcEvent*)mSimuMaker->GetDataSet("StMcEvent");
  
  const St_particle* particleTabPtr = mSimuMaker->particleTable();
  const particle_st* particleTable = particleTabPtr->GetTable();

  MCParticleList theList;

  for (int i=0; i<particleTabPtr->GetNRows();++i) {
		
    MCParticle particle;
    particle.status          = particleTable[i].isthep;
    particle.mcparticleId    = i + 1;
    particle.pdg             = particleTable[i].idhep;
    particle.firstMotherId   = particleTable[i].jmohep[0];
    particle.lastMotherId    = particleTable[i].jmohep[1];
    particle.firstDaughterId = particleTable[i].jdahep[0];
    particle.lastDaughterId  = particleTable[i].jdahep[1];

    TLorentzVector p4(particleTable[i].phep[0], particleTable[i].phep[1], particleTable[i].phep[2], particleTable[i].phep[3]);
    particle.pt  = p4.Pt();
    particle.eta = p4.Eta();
    particle.phi = p4.Phi();
    particle.m   = p4.M();

    theList.push_back(particle);
  }

  return theList;
}

}

