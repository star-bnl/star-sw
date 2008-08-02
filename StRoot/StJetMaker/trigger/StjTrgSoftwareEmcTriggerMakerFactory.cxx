// $Id: StjTrgSoftwareEmcTriggerMakerFactory.cxx,v 1.1 2008/08/02 22:21:33 tai Exp $
#include "StjTrgSoftwareEmcTriggerMakerFactory.h"

#include "StjTrgSoftwareEmcTriggerMaker.h"

StjTrgSoftware* StjTrgSoftwareEmcTriggerMakerFactory::create()
{
  return new StjTrgSoftwareEmcTriggerMaker(_emcTrigMaker);
}

