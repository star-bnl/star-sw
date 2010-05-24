// $Id: StPythiaFourPMaker.cxx,v 1.16 2010/05/24 23:11:35 pibero Exp $
#include "StPythiaFourPMaker.h"

#include "StMuTrackFourVec.h"

#include "StjMCMuDst.h"

#include "StjMCParticleList.h"
#include "StjMCParticleListCut.h"
#include "StjMCParticleCut.h"
#include "StjMCParticleCutEta.h"
#include "StjMCParticleCutStatus.h"

#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

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
  for (size_t iNode = 0; iNode < _vertexNodes.size(); ++iNode) {
    VertexNode& node = _vertexNodes[iNode];
    if (node.vertex) {
      delete node.vertex;
      node.vertex = 0;
    }
    FourList& tracks = node.tracks;
    for (FourList::iterator it = tracks.begin(); it != tracks.end(); ++it) {
      delete *it;
      *it = 0;
    }
    tracks.clear();
  }
  _vertexNodes.clear();
}

Int_t StPythiaFourPMaker::Make()
{
  StMuPrimaryVertex* pv = new StMuPrimaryVertex;
  pv->setPosition(_mc->getMCVertex());

  _vertexNodes.push_back(VertexNode());
  _vertexNodes[0].vertex = pv;
  FourList& tracks = _vertexNodes[0].tracks;

  StjMCParticleList theList = _mc->getMCParticleList();
  theList = (*_cut)(theList);

  for (StjMCParticleList::const_iterator it = theList.begin(); it != theList.end(); ++it) {
    TLorentzVector p;
    p.SetPtEtaPhiM(it->pt,it->eta,it->phi,it->m);
    StMcTrackEmu* mctrack = new StMcTrackEmu;
    mctrack->_pt     = it->pt;
    mctrack->_eta    = it->eta;
    mctrack->_phi    = it->phi;
    mctrack->_m      = it->m;
    mctrack->_e      = it->e;
    mctrack->_id     = it->mcparticleId;
    mctrack->_pdg    = it->pdg;
    mctrack->_status = it->status;
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0,0,mctrack,p,0,it->mcparticleId-1,kUnknownId);
    tracks.push_back(pmu);
  }

  return kStOK;
}
