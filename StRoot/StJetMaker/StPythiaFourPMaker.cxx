// $Id: StPythiaFourPMaker.cxx,v 1.4 2008/07/22 05:06:23 tai Exp $
#include "StPythiaFourPMaker.h"

#include "emulator/StMuTrackFourVec.h"
#include "emulator/StPythiaMuTrackFourVec.h"

#include "mudst/StJetMCMuDst.h"

#include <TLorentzVector.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;
    
ClassImp(StPythiaFourPMaker)

StPythiaFourPMaker::StPythiaFourPMaker(const char* name, StMCAsymMaker* sim,  StMcEventMaker* mc)
: StFourPMaker(name)
{
  _mc = new StJetMCMuDst(sim, mc);
}

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

Int_t StPythiaFourPMaker::Init()
{
  return kStOK;
}

Int_t StPythiaFourPMaker::Make()
{
  MCParticleList theList = _mc->getMCPartilceList();

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







