// -*- mode: c++;-*-
// $Id: StjTrgSoftwareEmcTriggerMakerFactory.h,v 1.2 2008/08/02 19:22:30 tai Exp $
#ifndef STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H

#include "StjTrgSoftwareFactory.h"

class StEmcTriggerMaker;

class StjTrgSoftwareEmcTriggerMakerFactory : public StjTrgSoftwareFactory {

public:
  StjTrgSoftwareEmcTriggerMakerFactory(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  virtual ~StjTrgSoftwareEmcTriggerMakerFactory() { }

  StjTrgSoftware* create();

private:
  StEmcTriggerMaker* _emcTrigMaker;
};


#endif // STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
