// $Id: StjTrgMuDstSoftwareTriggerSimuMakerFactory.cxx,v 1.2 2008/08/18 06:20:47 tai Exp $
#include "StjTrgMuDstSoftwareTriggerSimuMakerFactory.h"

ClassImp(StjTrgMuDstSoftwareTriggerSimuMakerFactory)

#include "StjTrgMuDstSoftwareTriggerSimuMaker.h"

#include "StjTrgMuDstSoftwareGetAdcEt.h"
#include "StjBEMCMuDst.h"

StjTrgMuDstSoftware* StjTrgMuDstSoftwareTriggerSimuMakerFactory::create()
{
  return new StjTrgMuDstSoftwareTriggerSimuMaker(_simuTrig, new StjTrgMuDstSoftwareGetAdcEt(new StjBEMCMuDst(_uDstMaker), _bemcJpTowerMap));
}

