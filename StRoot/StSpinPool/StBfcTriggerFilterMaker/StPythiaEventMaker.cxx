//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 May 2010
//

// ROOT
#include "TFile.h"
#include "TTree.h"

// STAR
#include "StSpinPool/StJetSkimEvent/StPythiaEvent.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_g2t_pythia_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_particle_Table.h"

// Local
#include "StPythiaEventMaker.h"

ClassImp(StPythiaEventMaker);

void StPythiaEventMaker::Clear(Option_t* option)
{
  mPythiaEvent->Clear(option);
}

int StPythiaEventMaker::Init()
{
  assert(!mFileName.IsNull());
  mFile = TFile::Open(mFileName,"recreate");
  assert(mFile);
  mPythiaEvent = new StPythiaEvent;
  mTree = new TTree("PythiaTree","Pythia Record");
  mTree->Branch("PythiaBranch","StPythiaEvent",&mPythiaEvent);
  return kStOk;
}

int StPythiaEventMaker::Make()
{
  getEvent();
  getPythia();
  getVertex();
  getParticles();

  LOG_DEBUG << *mPythiaEvent << endm;

  mTree->Fill();

  return kStOk;
}

int StPythiaEventMaker::Finish()
{
  mFile->Write();
  mFile->Close();
  return kStOk;
}

void StPythiaEventMaker::getEvent()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_g2t_event* eventDescriptor = (St_g2t_event*)iter("g2t_event");
    if (eventDescriptor) {
      g2t_event_st* eventTable = (g2t_event_st*)eventDescriptor->GetTable();
      if (eventTable) {
	mPythiaEvent->setRunId(eventTable->n_run);
	mPythiaEvent->setEventId(eventTable->n_event);
      }
    }
  }
}

void StPythiaEventMaker::getPythia()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_g2t_pythia* pythiaDescriptor = (St_g2t_pythia*)iter("g2t_pythia");
    if (pythiaDescriptor) {
      g2t_pythia_st* pythiaTable = (g2t_pythia_st*)pythiaDescriptor->GetTable();
      if (pythiaTable) {
	mPythiaEvent->setProcessId(pythiaTable->subprocess_id);
	mPythiaEvent->setS(pythiaTable->mand_s);
	mPythiaEvent->setT(pythiaTable->mand_t);
	mPythiaEvent->setU(pythiaTable->mand_u);
	mPythiaEvent->setPt(pythiaTable->hard_p);
	mPythiaEvent->setCosTheta(pythiaTable->cos_th);
	mPythiaEvent->setX1(pythiaTable->bjor_1);
	mPythiaEvent->setX2(pythiaTable->bjor_2);
      }
    }
  }
}

void StPythiaEventMaker::getVertex()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_g2t_vertex* vertexDescriptor = (St_g2t_vertex*)iter("g2t_vertex");
    if (vertexDescriptor) {
      g2t_vertex_st* vertexTable = (g2t_vertex_st*)vertexDescriptor->GetTable();
      if (vertexTable) {
	mPythiaEvent->setVertex(TVector3(vertexTable[0].ge_x));
      }
    }
  }
}

void StPythiaEventMaker::getParticles()
{
  TDataSet* geant = GetDataSet("geant");
  if (geant) {
    TDataSetIter iter(geant);
    St_particle* particleDescriptor = (St_particle*)iter("particle");
    if (particleDescriptor) {
      particle_st* particleTable = (particle_st*)particleDescriptor->GetTable();
      if (particleTable) {
	for (int i = 0; i < particleDescriptor->GetNRows(); ++i) {
	  mPythiaEvent->addParticle(TParticle(particleTable[i].idhep, // pdg
					      particleTable[i].isthep, // status
					      particleTable[i].jmohep[0], // mother1
					      particleTable[i].jmohep[1], // mother2
					      particleTable[i].jdahep[0], // daughter1
					      particleTable[i].jdahep[1], // daughter2
					      TLorentzVector(particleTable[i].phep), // momentum and energy
					      TLorentzVector(particleTable[i].vhep))); // production vertex and time
	}
      }
    }
  }
}

ostream& operator<<(ostream& out, const TVector3& v)
{
  return out << v.x() << '\t' << v.y() << '\t' << v.z();
}

ostream& operator<<(ostream& out, const TParticle& particle)
{
  out << " name=" << particle.GetName()
      << " pdg=" << particle.GetPdgCode()
      << " sta=" << particle.GetStatusCode()
      << " mo1=" << particle.GetMother(0)
      << " mo2=" << particle.GetMother(1)
      << " da1=" << particle.GetDaughter(0)
      << " da2=" << particle.GetDaughter(1)
      << " pt=" << particle.Pt()
      << " pz=" << particle.Pz()
      << " eta=" << particle.Eta()
      << " phi=" << particle.Phi()
      << " e=" << particle.Energy();
  return out;
}

ostream& operator<<(ostream& out, const StPythiaEvent& pythiaEvent)
{
  out << "Pythia Record\n"
      << "run:\t"      << pythiaEvent.runId()      << '\n'
      << "event:\t"    << pythiaEvent.eventId()    << '\n'
      << "vertex:\t"   << pythiaEvent.vertex()     << '\n'
      << "pid:\t"      << pythiaEvent.processId()  << '\n'
      << "s:\t"        << pythiaEvent.s()          << '\n'
      << "t:\t"        << pythiaEvent.t()          << '\n'
      << "u:\t"        << pythiaEvent.u()          << '\n'
      << "pt:\t"       << pythiaEvent.pt()         << '\n'
      << "cosTheta:\t" << pythiaEvent.cosTheta()   << '\n'
      << "x1:\t"       << pythiaEvent.x1()         << '\n'
      << "x2:\t"       << pythiaEvent.x2()         << '\n';
  out << Form("%5s%14s%7s%9s%7s%9s%9s%9s%9s%9s\n",
	      "line","particle/jet","status","PDG","mother","pt","eta","phi","E","m");
  for (int i = 0; i < pythiaEvent.numberOfParticles(); ++i) {
    const TParticle* particle = pythiaEvent.particle(i);
    out << Form("%5d%14s%7d%9d%7d%9.3f%9.3f%9.3f%9.3f%9.3f\n",
		i+1,particle->GetName(),particle->GetStatusCode(),particle->GetPdgCode(),particle->GetFirstMother(),
		particle->Pt(),particle->Eta(),particle->Phi(),particle->Energy(),particle->GetCalcMass());
  }
  return out;
}
