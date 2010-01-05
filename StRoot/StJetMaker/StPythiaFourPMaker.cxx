// $Id: StPythiaFourPMaker.cxx,v 1.13 2009/09/04 17:29:54 pibero Exp $
#include "StPythiaFourPMaker.h"

#include "StMuTrackFourVec.h"

#include "StjMCMuDst.h"

#include "StjMCParticleListCut.h"
#include "StjMCParticleCut.h"
#include "StjMCParticleCutEta.h"
#include "StjMCParticleCutStatus.h"

#include <TLorentzVector.h>

#include <iostream>

using namespace std;
    
ClassImp(StPythiaFourPMaker)

Int_t StPythiaFourPMaker::Init()
{
  _mc = new StjMCMuDst(this);
  _cut = new StjMCParticleListCut();
  _cut->addCut(new StjMCParticleCutEta(-5.0, 5.0));
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
  StjMCParticleList theList = _mc->getMCParticleList();

  theList = (*_cut)(theList);

  for(StjMCParticleList::const_iterator it = theList.begin(); it != theList.end(); ++it) {

    TLorentzVector p4_;
    p4_.SetPtEtaPhiM((*it).pt, (*it).eta, (*it).phi, (*it).m);

    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, 0, p4_, 0, (*it).mcparticleId - 1, kUnknownId);
				
    tracks.push_back(pmu);
  }
	
  return kStOK;
}







