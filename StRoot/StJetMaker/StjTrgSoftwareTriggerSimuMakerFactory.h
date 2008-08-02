// -*- mode: c++;-*-
// $Id: StjTrgSoftwareTriggerSimuMakerFactory.h,v 1.2 2008/08/02 19:22:31 tai Exp $
#ifndef STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H

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


#endif // STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
