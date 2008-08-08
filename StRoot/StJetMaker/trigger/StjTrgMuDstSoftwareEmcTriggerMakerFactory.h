// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareEmcTriggerMakerFactory.h,v 1.1 2008/08/08 23:12:26 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H
#define STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H

#include "StjTrgMuDstSoftwareFactory.h"

class StEmcTriggerMaker;

class StjTrgMuDstSoftwareEmcTriggerMakerFactory : public StjTrgMuDstSoftwareFactory {

public:
  StjTrgMuDstSoftwareEmcTriggerMakerFactory(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  virtual ~StjTrgMuDstSoftwareEmcTriggerMakerFactory() { }

  StjTrgMuDstSoftware* create();

private:
  StEmcTriggerMaker* _emcTrigMaker;
};


#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKERFACTORY_H
