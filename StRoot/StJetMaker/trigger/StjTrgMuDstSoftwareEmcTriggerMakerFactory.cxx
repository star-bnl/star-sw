// $Id: StjTrgMuDstSoftwareEmcTriggerMakerFactory.cxx,v 1.1 2008/08/08 23:12:26 tai Exp $
#include "StjTrgMuDstSoftwareEmcTriggerMakerFactory.h"

#include "StjTrgMuDstSoftwareEmcTriggerMaker.h"

StjTrgMuDstSoftware* StjTrgMuDstSoftwareEmcTriggerMakerFactory::create()
{
  return new StjTrgMuDstSoftwareEmcTriggerMaker(_emcTrigMaker);
}

