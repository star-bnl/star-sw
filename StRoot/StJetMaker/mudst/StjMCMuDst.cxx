// $Id: StjMCMuDst.cxx,v 1.12 2012/01/21 17:36:11 pibero Exp $

#include <TDataSet.h>
#include <TDataSetIter.h>
#include <TLorentzVector.h>

#include <StMaker.h>
#include <tables/St_g2t_event_Table.h>
#include <tables/St_g2t_vertex_Table.h>
#include <tables/St_g2t_pythia_Table.h>
#include <tables/St_particle_Table.h>

#include <StjMCParticleCutParton.h>
#include <StjMCMuDst.h>

StjPrimaryVertex StjMCMuDst::getMCVertex() const
{
  StjPrimaryVertex vertex;
  if (TDataSet* Event = _maker->GetDataSet("geant")) {
    TDataSetIter geantDstI(Event);
    if (const St_g2t_vertex* g2t_vertex_descriptor = (const St_g2t_vertex*)geantDstI("g2t_vertex"))
      if (const g2t_vertex_st* g2t_vertex_table = g2t_vertex_descriptor->GetTable())
	{
	  const float* geantvertex = g2t_vertex_table[0].ge_x;
	  vertex.mPosition.SetXYZ(geantvertex[0],geantvertex[1],geantvertex[2]);
	}
  }
  return vertex;
}

StjMCParticleList StjMCMuDst::getMCParticleList()
{
  StjMCParticleList theList;

  if (TDataSet* Event = _maker->GetDataSet("geant")) {
    TDataSetIter geantDstI(Event);

    // Get run number and event number from table g2t_event
    int runNumber   = -1;
    int eventNumber = -1;

    if (const St_g2t_event* g2t_event_descriptor = (const St_g2t_event*)geantDstI("g2t_event")) {
      if (const g2t_event_st* g2t_event_table = g2t_event_descriptor->GetTable()) {
	runNumber   = g2t_event_table->n_run;
	eventNumber = g2t_event_table->n_event;
      }
    }

    // Get Pythia variables
    if (St_g2t_pythia* g2t_pythia = (St_g2t_pythia*)geantDstI("g2t_pythia")) {
      if (g2t_pythia_st* pythiaTable = g2t_pythia->GetTable()) {
	StjMCParticleCutParton::mstu72 = pythiaTable->mstu72;
	StjMCParticleCutParton::mstu73 = pythiaTable->mstu73;
      }
    }

    // Get particles from table particle
    if (const St_particle* particle_descriptor = (const St_particle*)geantDstI("particle")) {
      if (const particle_st* particle_table = particle_descriptor->GetTable()) {
	for (int i = 0; i < particle_descriptor->GetNRows(); ++i) {
	  StjMCParticle particle;
	  particle.runNumber       = runNumber;
	  particle.eventId         = eventNumber;	
	  particle.status          = particle_table[i].isthep;
	  particle.mcparticleId    = i+1;
	  particle.pdg             = particle_table[i].idhep;
	  particle.firstMotherId   = particle_table[i].jmohep[0];
	  particle.lastMotherId    = particle_table[i].jmohep[1];
	  particle.firstDaughterId = particle_table[i].jdahep[0];
	  particle.lastDaughterId  = particle_table[i].jdahep[1];
	  particle.vertexZ         = particle_table[i].vhep[2];

	  TLorentzVector p4(particle_table[i].phep);
	  particle.pt  = p4.Pt();
	  particle.eta = p4.Eta();
	  particle.phi = p4.Phi();
	  particle.m   = p4.M();
	  particle.e   = p4.E();

	  theList.push_back(particle);
	}
      }
    }
  }

  return theList;
}
