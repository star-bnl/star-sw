// -*- mode: c++;-*-
// $Id: StJetTrgSoftwareEmcTriggerMakerFactory.h,v 1.1 2008/07/24 02:14:49 tai Exp $
#ifndef STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJETTRGSOFTWAREEMCTRIGGERMAKERFACTORY_H

#include "StJetTrgSoftwareFactory.h"

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
