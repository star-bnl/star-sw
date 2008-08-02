// -*- mode: c++;-*-
// $Id: StjTrgSoftwareTriggerSimuMakerFactory.h,v 1.2 2008/08/02 22:43:44 tai Exp $
#ifndef STJTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H

#include "StjTrgSoftwareFactory.h"

class StTriggerSimuMaker;

class StjTrgSoftwareTriggerSimuMakerFactory : public StjTrgSoftwareFactory {

public:
  StjTrgSoftwareTriggerSimuMakerFactory(StTriggerSimuMaker* simuTrig)
    : _simuTrig(simuTrig) { }
  virtual ~StjTrgSoftwareTriggerSimuMakerFactory() { }

  StjTrgSoftware* create();

private:
  StTriggerSimuMaker* _simuTrig;
};


#endif // STJTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
