// $Id: StPythiaFourPMaker.cxx,v 1.19 2011/01/27 20:43:12 pibero Exp $
#include "StPythiaFourPMaker.h"

#include "StMuTrackFourVec.h"

#include "StjMCMuDst.h"

#include "StjMCParticleList.h"
#include "StjMCParticleListCut.h"
#include "StjMCParticleCut.h"
#include "StjMCParticleCutEta.h"
#include "StjMCParticleCutStatus.h"
#include "StjMCParticleToStMuTrackFourVec.h"

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
  const TVector3& v = _mc->getMCVertex().position();
  StMuPrimaryVertex* pv = new StMuPrimaryVertex;
  pv->setPosition(StThreeVectorF(v.x(),v.y(),v.z()));
  _vertexNodes.push_back(VertexNode());
  _vertexNodes[0].vertex = pv;
  StjMCParticleList theList = (*_cut)(_mc->getMCParticleList());
  FourList& tracks = _vertexNodes[0].tracks;
  transform(theList.begin(),theList.end(),back_inserter(tracks),StjMCParticleToStMuTrackFourVec());

  return kStOK;
}
