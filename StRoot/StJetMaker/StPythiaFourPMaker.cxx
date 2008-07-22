// $Id: StPythiaFourPMaker.cxx,v 1.5 2008/07/22 05:48:27 tai Exp $
#include "StPythiaFourPMaker.h"

#include "emulator/StMuTrackFourVec.h"

#include "mudst/StJetMCMuDst.h"

#include <TLorentzVector.h>

#include <iostream>

using namespace std;
using namespace StSpinJet;
    
ClassImp(StPythiaFourPMaker)

Int_t StPythiaFourPMaker::Init()
{
  _mc = new StJetMCMuDst(this);

  return kStOK;
}

void StPythiaFourPMaker::Clear(Option_t* opt)
{
  for (FourList::iterator it = tracks.begin(); it != tracks.end(); ++it) {
    delete (*it);
  }

  tracks.clear();

  return;
}

Int_t StPythiaFourPMaker::Make()
{
  MCParticleList theList = _mc->getMCPartilceList();

  for(MCParticleList::const_iterator it = theList.begin(); it != theList.end(); ++it) {

    if((*it).status != 1) continue;
    
    TLorentzVector p4_;
    p4_.SetPtEtaPhiM((*it).pt, (*it).eta, (*it).phi, (*it).m);
			
    if (fabs((*it).eta) >= 5.0) continue;
    
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4_, 0, (*it).mcparticleId - 1, kUnknownId);
				
    tracks.push_back(pmu);
				
  }
	
  return kStOK;
}







