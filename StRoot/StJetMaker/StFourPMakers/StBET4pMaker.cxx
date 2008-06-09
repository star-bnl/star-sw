// $Id: StBET4pMaker.cxx,v 1.35 2008/06/09 22:30:35 tai Exp $

#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"


#include "StEventTypes.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

#include <iostream>

using namespace std;

ClassImp(StBET4pMaker)
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : StFourPMaker(name, 0)
  , mTables(new StBemcTables(doTowerSwapFix))
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
  , _imp(new StBET4pMakerImp(uDstMaker, mTables))
{
  cout <<"StBET4pMaker::StBET4pMaker()"<<endl;
}

void StBET4pMaker::setUseEndcap(bool v)   { _imp->setUseEndcap(v); }
void StBET4pMaker::setUse2003Cuts(bool v) { _imp->setUse2003Cuts(v); }
void StBET4pMaker::setUse2005Cuts(bool v) { _imp->setUse2005Cuts(v); }
void StBET4pMaker::setUse2006Cuts(bool v) { _imp->setUse2006Cuts(v); }

Int_t StBET4pMaker::InitRun(Int_t runId)
{
  mTables->loadTables((StMaker*)this);

  _imp->InitRun(runId);
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

  _imp->Make();

  mSumEmcEt = _imp->sumEmcEt();
  mDylanPoints = _imp->nDylanPoints();

  return StMaker::Make();
}

bool StBET4pMaker::isBemcCorrupted() const
{
  if(StEmcADCtoEMaker* adc2e = dynamic_cast<StEmcADCtoEMaker*>(const_cast<StBET4pMaker*>(this)->GetMaker("Eread")))
    return adc2e->isCorrupted();
    
  return false;
}
