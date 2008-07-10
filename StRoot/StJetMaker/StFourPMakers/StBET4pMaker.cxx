// $Id: StBET4pMaker.cxx,v 1.58 2008/07/10 07:42:05 tai Exp $

#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"

#include "StJetTPCMuDst.h"
#include "StJetBEMCMuDst.h"
#include "StJetEEMCMuDst.h"

#include "StJetTPCTrackCut.h"
#include "StJetBEMCEnergyCut.h"

#include "CorrectTowerEnergyForTracks.h"

#include "BemcEnergySumCalculator.h"
#include "BemcEnergySumCalculatorBuilder.h"

#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

ClassImp(StBET4pMaker)
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : StFourPMaker(name, 0)
  , _uDstMaker(uDstMaker), _doTowerSwapFix(doTowerSwapFix)
  , _useTPC(true), _useBEMC(true), _useEndcap(false)
  , _use2003Cuts(false), _use2005Cuts(false), _use2006Cuts(false)
  , _imp(0)
  , _bemcEnergySumCalculator(0)
{

}

Int_t StBET4pMaker::Init()
{
  StJetTPCMuDst*  tpc  = new StJetTPCMuDst(_uDstMaker);
  StJetBEMCMuDst* bemc = new StJetBEMCMuDst(_uDstMaker, _doTowerSwapFix);
  StJetEEMCMuDst* eemc = new StJetEEMCMuDst(_uDstMaker);

  StJetTPCTrackCut*   tpcCut  = new StJetTPCTrackCut();
  StJetBEMCEnergyCut* bemcCut = new StJetBEMCEnergyCut();

  CorrectTowerEnergyForTracks* correctTowerEnergyForTracks = new CorrectTowerEnergyForTracks();

  _imp = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);


  _imp->setUseTPC(_useTPC);
  _imp->setUseBEMC(_useBEMC);
  _imp->setUseEndcap(_useEndcap);
  bemcCut->setUse2003Cuts(_use2003Cuts);
  bemcCut->setUse2005Cuts(_use2005Cuts);
  tpcCut->setUse2006Cuts(_use2006Cuts);

  tpc->Init();
  bemc->Init();
  if(_useEndcap) eemc->Init();

  BemcEnergySumCalculatorBuilder bemcEnergySumCalculatorBuilder;
  _bemcEnergySumCalculator = bemcEnergySumCalculatorBuilder.build(_useBEMC, _use2003Cuts, _use2005Cuts, _uDstMaker, _doTowerSwapFix);

  return StMaker::Init();
}


void StBET4pMaker::Clear(Option_t* opt)
{
  _imp->Clear(opt);

  _bemcEnergySumCalculator->Clear();

  return StMaker::Clear(opt);
}

Int_t StBET4pMaker::Make()
{
  if(isBemcCorrupted()) return kStOk;

  _bemcEnergySumCalculator->Make();

  if (_bemcEnergySumCalculator->sumEmcEt() > 200.) return kStOk;

  _imp->Make();


  return StMaker::Make();
}

FourList &StBET4pMaker::getTracks()
{
  return _imp->getTracks();
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
