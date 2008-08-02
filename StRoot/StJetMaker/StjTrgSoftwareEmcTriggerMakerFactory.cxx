// $Id: StjTrgSoftwareEmcTriggerMakerFactory.cxx,v 1.2 2008/08/02 19:22:30 tai Exp $
#include "StjTrgSoftwareEmcTriggerMakerFactory.h"

#include "StjTrgSoftwareEmcTriggerMaker.h"

StjTrgSoftware* StjTrgSoftwareEmcTriggerMakerFactory::create()
{
  return new StjTrgSoftwareEmcTriggerMaker(_emcTrigMaker);
}

