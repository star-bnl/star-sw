// $Id: StJetMCMuDst.cxx,v 1.3 2008/07/24 21:37:05 tai Exp $

#include <StJetMCMuDst.h>

#include <StMaker.h>

#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>

#include <TDataSet.h>
#include <TDataSetIter.h>

#include <tables/St_particle_Table.h>

#include <TLorentzVector.h>

namespace StSpinJet {

MCParticleList StJetMCMuDst::getMCPartilceList()
{

  TDataSet *Event = _maker->GetDataSet("geant");

  TDataSetIter geantDstI(Event);
  const St_particle* particleTabPtr = (St_particle*)geantDstI("particle");
  const particle_st* particleTable = particleTabPtr->GetTable();

  MCParticleList theList;

  StMuDstMaker *uDstMaker = dynamic_cast<StMuDstMaker*>(_maker->GetMaker("MuDst"));

  MCParticle particle;
  particle.runNumber = uDstMaker->muDst()->event()->runId();
  particle.eventId = uDstMaker->muDst()->event()->eventId();

  for (int i = 0; i < particleTabPtr->GetNRows(); ++i) {
		
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

