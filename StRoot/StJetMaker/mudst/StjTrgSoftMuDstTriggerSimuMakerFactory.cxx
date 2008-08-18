// $Id: StjTrgSoftMuDstTriggerSimuMakerFactory.cxx,v 1.1 2008/08/18 06:37:27 tai Exp $
#include "StjTrgSoftMuDstTriggerSimuMakerFactory.h"

ClassImp(StjTrgSoftMuDstTriggerSimuMakerFactory)

#include "StjTrgSoftMuDstTriggerSimuMaker.h"

#include "StjTrgSoftGetAdcEt.h"
#include "StjBEMCMuDst.h"

StjTrgSoft* StjTrgSoftMuDstTriggerSimuMakerFactory::create()
{
  return new StjTrgSoftMuDstTriggerSimuMaker(_simuTrig, new StjTrgSoftGetAdcEt(new StjBEMCMuDst(_uDstMaker), _bemcJpTowerMap));
}

