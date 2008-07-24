// $Id: StJetTrgSoftwareTriggerSimuMakerFactory.cxx,v 1.1 2008/07/24 02:14:50 tai Exp $
#include "StJetTrgSoftwareTriggerSimuMakerFactory.h"

#include "StJetTrgSoftwareTriggerSimuMaker.h"

StJetTrgSoftware* StJetTrgSoftwareTriggerSimuMakerFactory::create()
{
  return new StJetTrgSoftwareTriggerSimuMaker(_simuTrig);
}

