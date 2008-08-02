// -*- mode: c++;-*-
// $Id: StjTrgSoftwareEmcTriggerMakerFactory.h,v 1.1 2008/08/02 04:07:46 tai Exp $
#ifndef STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H

#include "StjTrgSoftwareFactory.h"

class StEmcTriggerMaker;

class StJetTrgSoftwareEmcTriggerMakerFactory : public StJetTrgSoftwareFactory {

public:
  StJetTrgSoftwareEmcTriggerMakerFactory(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  virtual ~StJetTrgSoftwareEmcTriggerMakerFactory() { }

  StJetTrgSoftware* create();

private:
  StEmcTriggerMaker* _emcTrigMaker;
};


#endif // STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
