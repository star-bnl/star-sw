// -*- mode: c++;-*-
// $Id: StJetTrgSoftwareTriggerSimuMakerFactory.h,v 1.1 2008/07/24 02:14:50 tai Exp $
#ifndef STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJETTRGSOFTWARETRIGGERSIMUMAKERFACTORY_H

#include "StJetTrgSoftwareFactory.h"

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
