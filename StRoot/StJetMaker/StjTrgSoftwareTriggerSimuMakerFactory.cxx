// $Id: StjTrgSoftwareTriggerSimuMakerFactory.cxx,v 1.1 2008/08/02 04:08:16 tai Exp $
#include "StjTrgSoftwareTriggerSimuMakerFactory.h"

#include "StjTrgSoftwareTriggerSimuMaker.h"

StJetTrgSoftware* StJetTrgSoftwareTriggerSimuMakerFactory::create()
{
  return new StJetTrgSoftwareTriggerSimuMaker(_simuTrig);
}

