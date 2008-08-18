// $Id: StjTrgMuDstSoftwareEmcTriggerMakerFactory.cxx,v 1.2 2008/08/18 06:20:45 tai Exp $
#include "StjTrgMuDstSoftwareEmcTriggerMakerFactory.h"

ClassImp(StjTrgMuDstSoftwareEmcTriggerMakerFactory)

#include "StjTrgMuDstSoftwareEmcTriggerMaker.h"

#include "StjTrgMuDstSoftwareGetAdcEt.h"
#include "StjBEMCMuDst.h"

StjTrgMuDstSoftware* StjTrgMuDstSoftwareEmcTriggerMakerFactory::create()
{
  StjBEMC* bemc = new StjBEMCMuDst(_uDstMaker);
  StjTrgMuDstSoftwareGetAdcEt* getAdcEt = new StjTrgMuDstSoftwareGetAdcEt(bemc, _bemcJpTowerMap);
  StjTrgMuDstSoftware* ret = new StjTrgMuDstSoftwareEmcTriggerMaker(_emcTrigMaker, getAdcEt);
  return ret;
}

