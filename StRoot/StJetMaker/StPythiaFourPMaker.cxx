//std
#include <iostream>
using namespace std;

#include "StPythiaFourPMaker.h"

//SCL
#include "StarClassLibrary/SystemOfUnits.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//StEvent
#include "StDetectorId.h"

//McEvent
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"

//Table stuff
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "tables/St_g2t_event_Table.h"
#include "tables/St_particle_Table.h"
#include "tables/St_g2t_pythia_Table.h"

//Get Pythia record 
#include "StSpinPool/StMCAsymMaker/StMCAsymMaker.h"

//StJetMaker
#include "emulator/StMuTrackFourVec.h"
#include "StPythia/StPythiaMuTrackFourVec.h"

#include <TLorentzVector.h>

using namespace StSpinJet;

ClassImp(StPythiaFourPMaker)
    
StPythiaFourPMaker::StPythiaFourPMaker(const char* name, StMCAsymMaker* sim,  StMcEventMaker* mc) 
: StFourPMaker(name), mSimuMaker(sim), mMcEventMaker(mc)
{ }

void StPythiaFourPMaker::Clear(Option_t* opt)
{
  for (Pythia4Vec::iterator it=mVec.begin(); it!=mVec.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  mVec.clear();
  tracks.clear();
  return;
}

MCParticleList StPythiaFourPMaker::getMCPartilceList()
{
  StMcEvent* mcEvent = (StMcEvent*) GetDataSet("StMcEvent");
  
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

Int_t StPythiaFourPMaker::Make()
{
  MCParticleList theList = getMCPartilceList();

  for(MCParticleList::const_iterator it = theList.begin(); it != theList.end(); ++it) {

    if((*it).status != 1) continue;
    
    TLorentzVector p4_;
    p4_.SetPtEtaPhiM((*it).pt, (*it).eta, (*it).phi, (*it).m);
			
    if (fabs((*it).eta) < 5.0) {
				
      StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4_, 0, (*it).mcparticleId - 1, kUnknownId);
				
      mVec.push_back(pmu);
      tracks.push_back(pmu);
				
    }
  }
	
  return kStOK;
}







