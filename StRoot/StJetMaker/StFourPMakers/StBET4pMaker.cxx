// $Id: StBET4pMaker.cxx,v 1.54 2008/07/10 01:20:24 tai Exp $

#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"

#include "StJetTPCMuDst.h"
#include "StJetTPCTrackCut.h"
#include "StJetBEMCMuDst.h"
#include "StJetBEMCEnergyCut.h"
#include "StJetEEMCMuDst.h"

#include "BemcEnergySumCalculator.h"

#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

ClassImp(StBET4pMaker)
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : StFourPMaker(name, 0)
  , _useEndcap(false)
  , _use2003Cuts(false)
  , _use2005Cuts(false)
  , _use2006Cuts(false)
  , _tpc(new StJetTPCMuDst(uDstMaker))
  , _tpcCut(new StJetTPCTrackCut())
  , _eemc(new StJetEEMCMuDst(uDstMaker))
  , _bemc(new StJetBEMCMuDst(uDstMaker, doTowerSwapFix))
  , _bemcCut(new StJetBEMCEnergyCut)
  , _correctTowerEnergyForTracks(new CorrectTowerEnergyForTracks())
  , _imp(new StBET4pMakerImp(_tpc, _tpcCut, _bemc, _bemcCut, _correctTowerEnergyForTracks, _eemc))
  , _bemcEnergySumCalculator(new BemcEnergySumCalculator(_bemc, _bemcCut))
{

}

Int_t StBET4pMaker::Init()
{
  _imp->setUseEndcap(_useEndcap);
  _bemcCut->setUse2003Cuts(_use2003Cuts);
  _bemcCut->setUse2005Cuts(_use2005Cuts);
  _tpcCut->setUse2006Cuts(_use2006Cuts);

  StEEmcDbMaker* mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
  if(mEeDb) _eemc->Init(mEeDb);

  return StMaker::Init();
}


void StBET4pMaker::Clear(Option_t* opt)
{
  _imp->Clear(opt);

  _bemcEnergySumCalculator->Clear();

  return StMaker::Clear(opt);
}

FourList &StBET4pMaker::getTracks()
{
  return _imp->getTracks();
}

Int_t StBET4pMaker::Make()
{
  if(isBemcCorrupted()) return kStOk;

  _bemcEnergySumCalculator->Make();

  if (_bemcEnergySumCalculator->sumEmcEt() > 200.) return kStOk;

  _imp->Make();


  return StMaker::Make();
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
