// $Id: StjTrgSoftwareTriggerSimuMakerFactory.cxx,v 1.2 2008/08/02 19:22:31 tai Exp $
#include "StjTrgSoftwareTriggerSimuMakerFactory.h"

#include "StjTrgSoftwareTriggerSimuMaker.h"

StjTrgSoftware* StjTrgSoftwareTriggerSimuMakerFactory::create()
{
  return new StjTrgSoftwareTriggerSimuMaker(_simuTrig);
}

