// -*- mode: c++;-*-
// $Id: StjTrgSoftwareTriggerSimuMakerFactory.h,v 1.1 2008/08/02 22:21:35 tai Exp $
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
