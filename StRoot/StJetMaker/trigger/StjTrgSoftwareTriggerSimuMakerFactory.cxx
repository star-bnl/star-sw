// $Id: StjTrgSoftwareTriggerSimuMakerFactory.cxx,v 1.1 2008/08/02 22:21:34 tai Exp $
#include "StjTrgSoftwareTriggerSimuMakerFactory.h"

#include "StjTrgSoftwareTriggerSimuMaker.h"

StjTrgSoftware* StjTrgSoftwareTriggerSimuMakerFactory::create()
{
  return new StjTrgSoftwareTriggerSimuMaker(_simuTrig);
}

