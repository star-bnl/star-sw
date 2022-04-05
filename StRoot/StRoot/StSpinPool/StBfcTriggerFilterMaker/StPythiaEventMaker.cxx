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
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"

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
  //getAsymmetries();

  if (Debug()) { LOG_DEBUG << *mPythiaEvent << endm; }

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

//  mPythiaEvent->setRunId(GetRunNumber());
//  mPythiaEvent->setEventId(GetEventNumber());
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
	mPythiaEvent->setMstu72(pythiaTable->mstu72);
	mPythiaEvent->setMstu73(pythiaTable->mstu73);
	mPythiaEvent->setMstp111(pythiaTable->mstp111);
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

void StPythiaEventMaker::getAsymmetries()
{
  float s = mPythiaEvent->s();
  float t = mPythiaEvent->t();
  float u = mPythiaEvent->u();
  int pid = mPythiaEvent->processId();
  int flavor1 = mPythiaEvent->particle(4)->GetPdgCode();
  int flavor2 = mPythiaEvent->particle(5)->GetPdgCode();
  int flavor3 = mPythiaEvent->particle(6)->GetPdgCode();
  int flavor4 = mPythiaEvent->particle(7)->GetPdgCode();
  float x1 = mPythiaEvent->x1();
  float x2 = mPythiaEvent->x2();
  float Q2 = mPythiaEvent->Q2();

  mPythiaEvent->setDF1(StPythiaEvent::LO,StMCAsymMaker::get_polPDF_LO(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::NLO,StMCAsymMaker::get_polPDF_NLO(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::ZERO,StMCAsymMaker::get_polPDF_NLO_g0(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::MAX,StMCAsymMaker::get_polPDF_NLO_gmax(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::MIN,StMCAsymMaker::get_polPDF_NLO_gmin(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M015,StMCAsymMaker::get_polPDF_NLO_m015(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M030,StMCAsymMaker::get_polPDF_NLO_m030(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M045,StMCAsymMaker::get_polPDF_NLO_m045(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M060,StMCAsymMaker::get_polPDF_NLO_m060(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M075,StMCAsymMaker::get_polPDF_NLO_m075(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M090,StMCAsymMaker::get_polPDF_NLO_m090(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::M105,StMCAsymMaker::get_polPDF_NLO_m105(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::P030,StMCAsymMaker::get_polPDF_NLO_p030(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::P045,StMCAsymMaker::get_polPDF_NLO_p045(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::P060,StMCAsymMaker::get_polPDF_NLO_p060(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::P070,StMCAsymMaker::get_polPDF_NLO_p070(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::GS_NLOA,StMCAsymMaker::get_polPDF_NLO_GSA(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::GS_NLOB,StMCAsymMaker::get_polPDF_NLO_GSB(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::GS_NLOC,StMCAsymMaker::get_polPDF_NLO_GSC(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::DSSV,StMCAsymMaker::get_polPDF_NLO_DSSV(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::LSS1,StMCAsymMaker::get_polPDF_NLO_LSS1(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::LSS2,StMCAsymMaker::get_polPDF_NLO_LSS2(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::LSS3,StMCAsymMaker::get_polPDF_NLO_LSS3(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::AAC1,StMCAsymMaker::get_polPDF_NLO_AAC1(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::AAC2,StMCAsymMaker::get_polPDF_NLO_AAC2(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::AAC3,StMCAsymMaker::get_polPDF_NLO_AAC3(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::BB1,StMCAsymMaker::get_polPDF_NLO_BB1(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::BB2,StMCAsymMaker::get_polPDF_NLO_BB2(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::DNS1,StMCAsymMaker::get_polPDF_NLO_DNS1(flavor1,x1,Q2));
  mPythiaEvent->setDF1(StPythiaEvent::DNS2,StMCAsymMaker::get_polPDF_NLO_DNS2(flavor1,x1,Q2));

  mPythiaEvent->setDF2(StPythiaEvent::LO,StMCAsymMaker::get_polPDF_LO(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::NLO,StMCAsymMaker::get_polPDF_NLO(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::ZERO,StMCAsymMaker::get_polPDF_NLO_g0(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::MAX,StMCAsymMaker::get_polPDF_NLO_gmax(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::MIN,StMCAsymMaker::get_polPDF_NLO_gmin(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M015,StMCAsymMaker::get_polPDF_NLO_m015(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M030,StMCAsymMaker::get_polPDF_NLO_m030(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M045,StMCAsymMaker::get_polPDF_NLO_m045(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M060,StMCAsymMaker::get_polPDF_NLO_m060(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M075,StMCAsymMaker::get_polPDF_NLO_m075(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M090,StMCAsymMaker::get_polPDF_NLO_m090(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::M105,StMCAsymMaker::get_polPDF_NLO_m105(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::P030,StMCAsymMaker::get_polPDF_NLO_p030(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::P045,StMCAsymMaker::get_polPDF_NLO_p045(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::P060,StMCAsymMaker::get_polPDF_NLO_p060(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::P070,StMCAsymMaker::get_polPDF_NLO_p070(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::GS_NLOA,StMCAsymMaker::get_polPDF_NLO_GSA(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::GS_NLOB,StMCAsymMaker::get_polPDF_NLO_GSB(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::GS_NLOC,StMCAsymMaker::get_polPDF_NLO_GSC(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::DSSV,StMCAsymMaker::get_polPDF_NLO_DSSV(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::LSS1,StMCAsymMaker::get_polPDF_NLO_LSS1(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::LSS2,StMCAsymMaker::get_polPDF_NLO_LSS2(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::LSS3,StMCAsymMaker::get_polPDF_NLO_LSS3(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::AAC1,StMCAsymMaker::get_polPDF_NLO_AAC1(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::AAC2,StMCAsymMaker::get_polPDF_NLO_AAC2(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::AAC3,StMCAsymMaker::get_polPDF_NLO_AAC3(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::BB1,StMCAsymMaker::get_polPDF_NLO_BB1(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::BB2,StMCAsymMaker::get_polPDF_NLO_BB2(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::DNS1,StMCAsymMaker::get_polPDF_NLO_DNS1(flavor2,x2,Q2));
  mPythiaEvent->setDF2(StPythiaEvent::DNS2,StMCAsymMaker::get_polPDF_NLO_DNS2(flavor2,x2,Q2));

  mPythiaEvent->setF1(StPythiaEvent::LO,StMCAsymMaker::get_unpolPDF_LO(flavor1,x1,Q2));
  mPythiaEvent->setF1(StPythiaEvent::NLO,StMCAsymMaker::get_unpolPDF_NLO(flavor1,x1,Q2));

  mPythiaEvent->setF2(StPythiaEvent::LO,StMCAsymMaker::get_unpolPDF_LO(flavor2,x2,Q2));
  mPythiaEvent->setF2(StPythiaEvent::NLO,StMCAsymMaker::get_unpolPDF_NLO(flavor2,x2,Q2));

  mPythiaEvent->setPartonALL(StMCAsymMaker::getPartonicALL(s,t,u,pid,flavor1,flavor2,flavor3,flavor4));
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
