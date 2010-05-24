// $Id: StBET4pMaker.cxx,v 1.15 2010/05/24 17:42:26 pibero Exp $
#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"
#include "StBET4pMakerImpBuilder.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"

#include "StjeTrackListToStMuTrackFourVecList.h"
#include "StjeTowerEnergyListToStMuTrackFourVecList.h"

#include "StjeBemcEnergySumCalculator.h"
#include "StjeBemcEnergySumCalculatorBuilder.h"

#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StjTowerEnergyCorrectionForTracksMip.h"

#include <iostream>

using namespace std;

ClassImp(StBET4pMaker)
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix, StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks)
  : StFourPMaker(name)
  , _entryMaker(0)
  , _uDstMaker(uDstMaker), _doTowerSwapFix(doTowerSwapFix)
  , _correctTowerEnergyForTracks(correctTowerEnergyForTracks ? correctTowerEnergyForTracks : new StjTowerEnergyCorrectionForTracksMip)
  , _useTPC(true), _useBEMC(true), _useEEMC(false)
  , _use2003Cuts(false), _use2005Cuts(false), _use2006Cuts(false), _use2009Cuts(false)
  , _useBEMCEnergySum(true)
  , _useRandomSelector(false)
  , _useBEMCEnergyVariation(false), _bemcEnergyVariationRatio(0.05)
  , _imp(0)
  , _bemcEnergySumCalculator(0)
  , _track2four(*(new StjeTrackListToStMuTrackFourVecList))
  , _energy2four(*(new StjeTowerEnergyListToStMuTrackFourVecList))
  , _randomSelectorProb(1.0), _randomSelectorAt(false)
  , _randomSelectorSeed(0)
{ }

Int_t StBET4pMaker::Init()
{
  StBET4pMakerImpBuilder impBuilder;
  _imp = impBuilder.build(_useTPC, _useBEMC, _useEEMC, _use2003Cuts, _use2005Cuts, _use2006Cuts, _use2009Cuts, _useBEMCEnergyVariation, _bemcEnergyVariationRatio, _useRandomSelector, _uDstMaker, _doTowerSwapFix, _correctTowerEnergyForTracks, _randomSelectorProb, _randomSelectorAt, _randomSelectorSeed);
  _imp->Init();

  StjeBemcEnergySumCalculatorBuilder bemcEnergySumCalculatorBuilder;
  _bemcEnergySumCalculator = bemcEnergySumCalculatorBuilder.build(_useBEMCEnergySum && _useBEMC, _use2003Cuts, _use2005Cuts, _uDstMaker, _doTowerSwapFix);
  _bemcEnergySumCalculator->Init();

  return StMaker::Init();
}


void StBET4pMaker::Clear(Option_t* opt)
{
  for (size_t i = 0; i < _vertexNodes.size(); ++i) {
    VertexNode& node = _vertexNodes[i];
    node.vertex = 0;
    for (size_t j = 0; j < node.tracks.size(); ++j) {
      delete node.tracks[j];
      node.tracks[j] = 0;
    }
    node.tracks.clear();
  }

  _vertexNodes.clear();

  _bemcEnergySumCalculator->Clear();

  return StMaker::Clear(opt);
}

Int_t StBET4pMaker::Make()
{
  if(isBemcCorrupted()) return kStOk;

  _bemcEnergySumCalculator->Make();

  if (_bemcEnergySumCalculator->sumEmcEt() > 500.) return kStOk;

  // Save current vertex index
  int currentVertexIndex = StMuDst::currentVertexIndex();

  // Loop over primary vertices and get those with positive rank
  for (unsigned int vertexIndex = 0; vertexIndex < StMuDst::numberOfPrimaryVertices(); ++vertexIndex) {
    StMuDst::setVertexIndex(vertexIndex);
    StMuPrimaryVertex* vertex = StMuDst::primaryVertex(vertexIndex);
    if (vertex->ranking() > 0) {
      _vertexNodes.push_back(VertexNode());
      VertexNode& node = _vertexNodes.back();
      node.vertex = vertex;

      pair<StjTrackList,StjTowerEnergyList> trackAndEnergyList = _imp->getTrackAndEnergyList();

      FourList tpc4pList = _track2four(trackAndEnergyList.first);
      node.tracks.insert(node.tracks.end(),tpc4pList.begin(),tpc4pList.end());

      FourList energy4pList = _energy2four(trackAndEnergyList.second);
      node.tracks.insert(node.tracks.end(),energy4pList.begin(),energy4pList.end());
    }
  }

  // Restore current vertex index
  if (StMuDst::currentVertexIndex() != currentVertexIndex) StMuDst::setVertexIndex(currentVertexIndex);

  return kStOk;
}

bool StBET4pMaker::isBemcCorrupted() const
{
  if(StEmcADCtoEMaker* adc2e = dynamic_cast<StEmcADCtoEMaker*>(const_cast<StBET4pMaker*>(this)->GetMaker("Eread")))
    return adc2e->isCorrupted();
    
  return false;
}

int StBET4pMaker::nDylanPoints() const
{ 
  return _bemcEnergySumCalculator->nDylanPoints();
}

double StBET4pMaker::sumEmcEt() const 
{ 
  return _bemcEnergySumCalculator->sumEmcEt();
}
