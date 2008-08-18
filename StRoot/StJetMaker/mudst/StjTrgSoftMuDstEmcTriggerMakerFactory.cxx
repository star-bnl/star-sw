// $Id: StjTrgSoftMuDstEmcTriggerMakerFactory.cxx,v 1.1 2008/08/18 06:37:26 tai Exp $
#include "StjTrgSoftMuDstEmcTriggerMakerFactory.h"

ClassImp(StjTrgSoftMuDstEmcTriggerMakerFactory)

#include "StjTrgSoftMuDstEmcTriggerMaker.h"

#include "StjTrgSoftGetAdcEt.h"
#include "StjBEMCMuDst.h"

StjTrgSoft* StjTrgSoftMuDstEmcTriggerMakerFactory::create()
{
  StjBEMC* bemc = new StjBEMCMuDst(_uDstMaker);
  StjTrgSoftGetAdcEt* getAdcEt = new StjTrgSoftGetAdcEt(bemc, _bemcJpTowerMap);
  StjTrgSoft* ret = new StjTrgSoftMuDstEmcTriggerMaker(_emcTrigMaker, getAdcEt);
  return ret;
}

