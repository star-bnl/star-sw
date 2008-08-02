// -*- mode: c++;-*-
// $Id: StjTrgSoftwareEmcTriggerMakerFactory.h,v 1.2 2008/08/02 22:43:44 tai Exp $
#ifndef STJTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H

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


#endif // STJTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
