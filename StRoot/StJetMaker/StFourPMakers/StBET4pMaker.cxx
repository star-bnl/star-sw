// $Id: StBET4pMaker.cxx,v 1.39 2008/06/10 06:08:21 tai Exp $

#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"

#include "CollectChargedTracksFromTPC.h"

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
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
  , _collectChargedTracksFromTPC(new CollectChargedTracksFromTPC(uDstMaker))
  , _collectEnergyDepositsFromBEMC(new CollectEnergyDepositsFromBEMC(uDstMaker, _bemcTables))
  , _imp(new StBET4pMakerImp(uDstMaker, _collectChargedTracksFromTPC, _collectEnergyDepositsFromBEMC))
{

}

void StBET4pMaker::setUseEndcap(bool v)   { _imp->setUseEndcap(v); }

void StBET4pMaker::setUse2003Cuts(bool v)
{ 
  //  _imp->setUse2003Cuts(v);
  _collectEnergyDepositsFromBEMC->setUse2003Cuts(v);
}

void StBET4pMaker::setUse2005Cuts(bool v)
{
  //  _imp->setUse2005Cuts(v);
  _collectEnergyDepositsFromBEMC->setUse2005Cuts(v);
}

void StBET4pMaker::setUse2006Cuts(bool v)
{ 
  _collectChargedTracksFromTPC->setUse2006Cuts(v);
  //_imp->setUse2006Cuts(v);
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

  return StMaker::Clear(opt);
}

FourList &StBET4pMaker::getTracks()
{
  return _imp->getTracks();
}

Int_t StBET4pMaker::Make()
{
  if(isBemcCorrupted()) return kStOk;

  TowerEnergyDepositList bemcEnergyDepositList = _collectEnergyDepositsFromBEMC->Do();

  mSumEmcEt = sumEnergyOverBemcTowers(0.4, bemcEnergyDepositList);

  mDylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, bemcEnergyDepositList);

  if (mSumEmcEt > 200.) return kStOk;

  _imp->Make();

  //  mSumEmcEt = _imp->sumEmcEt();
  //  mDylanPoints = _imp->nDylanPoints();

  return StMaker::Make();
}

bool StBET4pMaker::isBemcCorrupted() const
{
  if(StEmcADCtoEMaker* adc2e = dynamic_cast<StEmcADCtoEMaker*>(const_cast<StBET4pMaker*>(this)->GetMaker("Eread")))
    return adc2e->isCorrupted();
    
  return false;
}

double StBET4pMaker::sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList)
{
  double ret(0.0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret += (*it).energy;

  return ret;
}

int StBET4pMaker::numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList)
{
  int ret(0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret ++;

  return ret;
}
