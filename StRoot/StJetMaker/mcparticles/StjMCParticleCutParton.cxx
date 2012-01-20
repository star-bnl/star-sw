//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 20 Jan 2012
//
// Select particles for partone jets
//

#include "StMaker.h"
#include "tables/St_g2t_pythia_Table.h"
#include "StjMCParticleCutParton.h"

ClassImp(StjMCParticleCutParton)

bool StjMCParticleCutParton::operator()(const StjMCParticle& particle)
{
  TDataSet* geant = StMaker::GetChain()->GetDataSet("geant");
  TDataSetIter geantIter(geant);
  St_g2t_pythia* g2t_pythia = (St_g2t_pythia*)geantIter("g2t_pythia");
  g2t_pythia_st* pythiaTable = g2t_pythia->GetTable();
  return particle.mcparticleId <= pythiaTable->mstu72 || particle.mcparticleId > pythiaTable->mstu73;
}
