// $Id: StjTrgSoftwareEmcTriggerMakerFactory.cxx,v 1.1 2008/08/02 04:07:39 tai Exp $
#include "StjTrgSoftwareEmcTriggerMakerFactory.h"

#include "StjTrgSoftwareEmcTriggerMaker.h"

StJetTrgSoftware* StJetTrgSoftwareEmcTriggerMakerFactory::create()
{
  return new StJetTrgSoftwareEmcTriggerMaker(_emcTrigMaker);
}

