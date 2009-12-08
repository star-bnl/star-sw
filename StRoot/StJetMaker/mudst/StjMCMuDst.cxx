// $Id: StjMCMuDst.cxx,v 1.7 2009/12/08 14:54:35 pibero Exp $

#include <StjMCMuDst.h>

#include <StMaker.h>

#include <TDataSet.h>
#include <TDataSetIter.h>

#include <tables/St_g2t_event_Table.h>
#include <tables/St_g2t_vertex_Table.h>
#include <tables/St_particle_Table.h>

#include <TLorentzVector.h>

StjMCParticleList StjMCMuDst::getMCParticleList()
{
  StjMCParticleList theList;

  if (TDataSet* Event = _maker->GetDataSet("geant")) {
    TDataSetIter geantDstI(Event);

    // Get run number and event number from table g2t_event
    int runNumber   = -999;
    int eventNumber = -999;
    if (const St_g2t_event* g2t_event_descriptor = (const St_g2t_event*)geantDstI("g2t_event"))
      if (const g2t_event_st* g2t_event_table = g2t_event_descriptor->GetTable()) {
	runNumber   = g2t_event_table->n_run;
	eventNumber = g2t_event_table->n_event;
      }

    // Get primary vertex from table g2t_vertex
    double vertexZ = -999;
    if (const St_g2t_vertex* g2t_vertex_descriptor = (const St_g2t_vertex*)geantDstI("g2t_vertex"))
      if (const g2t_vertex_st* g2t_vertex_table = g2t_vertex_descriptor->GetTable())
	vertexZ = g2t_vertex_table[0].ge_x[2];

    // Get particles from table particle
    const St_particle* particle_descriptor = (const St_particle*)geantDstI("particle");
    if (particle_descriptor) {
      const particle_st* particle_table = particle_descriptor->GetTable();
      if (particle_table) {
	StjMCParticle particle;

	particle.runNumber = runNumber;
	particle.eventId   = eventNumber;
	particle.vertexZ   = vertexZ;

	for (int i = 0; i < particle_descriptor->GetNRows(); ++i) {
	  // Final state particles only
	  if (particle_table[i].isthep == 1) {
	    particle.status          = particle_table[i].isthep;
	    particle.mcparticleId    = i + 1;
	    particle.pdg             = particle_table[i].idhep;
	    particle.firstMotherId   = particle_table[i].jmohep[0];
	    particle.lastMotherId    = particle_table[i].jmohep[1];
	    particle.firstDaughterId = particle_table[i].jdahep[0];
	    particle.lastDaughterId  = particle_table[i].jdahep[1];

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
  }

  return theList;
}

