// $Id: StJetTrgSoftwareEmcTriggerMakerFactory.cxx,v 1.1 2008/07/24 02:14:49 tai Exp $
#include "StJetTrgSoftwareEmcTriggerMakerFactory.h"

#include "StJetTrgSoftwareEmcTriggerMaker.h"

StJetTrgSoftware* StJetTrgSoftwareEmcTriggerMakerFactory::create()
{
  return new StJetTrgSoftwareEmcTriggerMaker(_emcTrigMaker);
}

