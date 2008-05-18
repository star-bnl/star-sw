// $Id: StBET4pMaker.cxx,v 1.30 2008/05/18 23:44:20 tai Exp $

#include "StBET4pMaker.h"
#include "StBET4pMakerImp.h"


#include "StEventTypes.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEEmcDbMaker/StEEmcDbMaker.h"

ClassImp(StBET4pMaker)
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : StFourPMaker(name, 0)
  , mCorrupt(false)
  , mTables(new StBemcTables(doTowerSwapFix))
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
  , _imp(new StBET4pMakerImp(name, uDstMaker, doTowerSwapFix))
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

  _imp->InitRun(runId, mTables);
  return kStOk;
}

Int_t StBET4pMaker::Init()
{
  StEEmcDbMaker* mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");

  StEmcADCtoEMaker* adc2e = (StEmcADCtoEMaker*)GetMaker("Eread");

  _imp->Init(mEeDb, adc2e);
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
  StEvent* event = dynamic_cast<StEvent*>(GetInputDS("StEvent"));
  _imp->Make(event);

  mSumEmcEt = _imp->sumEmcEt();
  mDylanPoints = _imp->nDylanPoints();
  mCorrupt =  _imp->bemcCorrupt();

  return StMaker::Make();
}

