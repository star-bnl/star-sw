// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareTriggerSimuMakerFactory.h,v 1.1 2008/08/08 23:18:51 tai Exp $
#ifndef STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H
#define STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H

#include "StjTrgMuDstSoftwareFactory.h"

class StTriggerSimuMaker;

class StjTrgMuDstSoftwareTriggerSimuMakerFactory : public StjTrgMuDstSoftwareFactory {

public:
  StjTrgMuDstSoftwareTriggerSimuMakerFactory(StTriggerSimuMaker* simuTrig)
    : _simuTrig(simuTrig) { }
  virtual ~StjTrgMuDstSoftwareTriggerSimuMakerFactory() { }

  StjTrgMuDstSoftware* create();

private:
  StTriggerSimuMaker* _simuTrig;
};


#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKERFACTORY_H
