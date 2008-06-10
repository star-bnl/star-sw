// $Id: StBET4pMaker.cxx,v 1.42 2008/06/10 08:07:08 tai Exp $

#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"

#include "CollectChargedTracksFromTPC.h"
#include "BemcEnergySumCalculator.h"

#include "StEventTypes.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

ClassImp(StBET4pMaker)
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : StFourPMaker(name, 0)
  , _bemcTables(new StBemcTables(doTowerSwapFix))
  , _collectChargedTracksFromTPC(new CollectChargedTracksFromTPC(uDstMaker))
  , _collectEnergyDepositsFromBEMC(new CollectEnergyDepositsFromBEMC(uDstMaker, _bemcTables))
  , _collectEnergyDepositsFromEEMC(new CollectEnergyDepositsFromEEMC(uDstMaker))
  , _imp(new StBET4pMakerImp(uDstMaker, _collectChargedTracksFromTPC, _collectEnergyDepositsFromBEMC, _collectEnergyDepositsFromEEMC))
  , _bemcEnergySumCalculator(new BemcEnergySumCalculator(_collectEnergyDepositsFromBEMC))
{

}

void StBET4pMaker::setUseEndcap(bool v)
{ 
  _imp->setUseEndcap(v);
}

void StBET4pMaker::setUse2003Cuts(bool v)
{ 
  _collectEnergyDepositsFromBEMC->setUse2003Cuts(v);
}

void StBET4pMaker::setUse2005Cuts(bool v)
{
  _collectEnergyDepositsFromBEMC->setUse2005Cuts(v);
}

void StBET4pMaker::setUse2006Cuts(bool v)
{ 
  _collectChargedTracksFromTPC->setUse2006Cuts(v);
}

Int_t StBET4pMaker::InitRun(Int_t runId)
{
  _bemcTables->loadTables((StMaker*)this);

  return kStOk;
}

Int_t StBET4pMaker::Init()
{
  StEEmcDbMaker* mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");

  _imp->Init(mEeDb);
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
