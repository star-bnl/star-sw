//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 15 Dec 2009
//

// ROOT includes
#include "TChain.h"

// STAR includes
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_vertex_Table.h"

#include "g2t/St_g2t_get_event_Module.h"
#include "g2t/St_g2t_get_pythia_Module.h"
#include "g2t/St_g2t_particle_Module.h"

#include "StSpinPool/StJetSkimEvent/StPythiaEvent.h"

// Local includes
#include "St_pythia_Maker.h"

ClassImp(St_pythia_Maker)

int St_pythia_Maker::Init()
{
  assert(!mFileName.IsNull());
  mChain = new TChain("PythiaTree");
  mChain->Add(mFileName);
  assert(mChain);
  mEvent = 0;
  mChain->SetBranchAddress("PythiaBranch",&mEvent);
  AddConst(new TDataSet("geant"));
  return StMaker::Init();
}

int St_pythia_Maker::Make()
{
  if (!mChain->GetEntry(GetNumber()-1)) return kStEOF;
  assert(mEvent);

  StEvtHddr* header = GetEvtHddr();
  header->SetRunNumber(mEvent->runId());
  header->SetEventNumber(mEvent->eventId());

  TDataSet* geant = GetDataSet("geant");
  assert(geant);
  geant->Delete();

  St_g2t_event* g2t_event = new St_g2t_event("g2t_event",1);
  geant->Add(g2t_event);
  g2t_event_st* eventTable = g2t_event->GetTable();
  eventTable->n_run = mEvent->runId();
  eventTable->n_event = mEvent->eventId();

  St_g2t_vertex* g2t_vertex = new St_g2t_vertex("g2t_vertex",1);
  geant->Add(g2t_vertex); 
  g2t_vertex_st* vertexTable = g2t_vertex->GetTable();
  mEvent->vertex().GetXYZ(vertexTable->ge_x);

  St_particle* particle = new St_particle("particle",mEvent->numberOfParticles());
  geant->Add(particle);

  St_g2t_pythia* g2t_pythia = new St_g2t_pythia("g2t_pythia",1);
  geant->Add(g2t_pythia);
  g2t_pythia_st* pythiaTable = g2t_pythia->GetTable();
  pythiaTable->subprocess_id = mEvent->processId();
  pythiaTable->mand_s = mEvent->s();
  pythiaTable->mand_t = mEvent->t();
  pythiaTable->mand_u = mEvent->u();
  pythiaTable->hard_p = mEvent->pt();
  pythiaTable->cos_th = mEvent->cosTheta();
  pythiaTable->bjor_1 = mEvent->x1();
  pythiaTable->bjor_2 = mEvent->x2();
  pythiaTable->mstu72 = mEvent->mstu72();
  pythiaTable->mstu73 = mEvent->mstu73();
  pythiaTable->mstp111 = mEvent->mstp111();

  // Track loop
  for (int i = 0; i < mEvent->numberOfParticles(); ++i) {
    particle_st part;

    part.isthep    = mEvent->particle(i)->GetStatusCode();
    part.idhep     = mEvent->particle(i)->GetPdgCode();
    part.jmohep[0] = mEvent->particle(i)->GetFirstMother();
    part.jmohep[1] = mEvent->particle(i)->GetSecondMother();
    part.jdahep[0] = mEvent->particle(i)->GetFirstDaughter();
    part.jdahep[1] = mEvent->particle(i)->GetLastDaughter();

    TLorentzVector v;
    mEvent->particle(i)->ProductionVertex(v);
    v.GetXYZT(part.vhep);

    TLorentzVector p;
    mEvent->particle(i)->Momentum(p);
    p.GetXYZT(part.phep);

    particle->AddAt(&part,i);
  } // End track loop

  return kStOk;
}
