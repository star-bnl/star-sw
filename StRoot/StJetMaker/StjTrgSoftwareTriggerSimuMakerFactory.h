// -*- mode: c++;-*-
// $Id: StjTrgSoftwareTriggerSimuMakerFactory.h,v 1.1 2008/08/02 04:08:21 tai Exp $
#ifndef STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H

#include "StjTrgSoftwareFactory.h"

class StTriggerSimuMaker;

class StJetTrgSoftwareTriggerSimuMakerFactory : public StJetTrgSoftwareFactory {

public:
  StJetTrgSoftwareTriggerSimuMakerFactory(StTriggerSimuMaker* simuTrig)
    : _simuTrig(simuTrig) { }
  virtual ~StJetTrgSoftwareTriggerSimuMakerFactory() { }

  StJetTrgSoftware* create();

private:
  StTriggerSimuMaker* _simuTrig;
};


#endif // STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
