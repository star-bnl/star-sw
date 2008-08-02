// -*- mode: c++;-*-
// $Id: StjTrgSoftwareTriggerSimuMaker.h,v 1.2 2008/08/02 22:43:44 tai Exp $
#ifndef STJTRGSOFTWARETRIGGERSIMUMAKER_H
#define STJTRGSOFTWARETRIGGERSIMUMAKER_H

#include "StjTrgSoftware.h"

#include <map>
#include <algorithm>

class StTriggerSimuMaker;

class StjTrgSoftwareTriggerSimuMaker : public StjTrgSoftware {

public:
  StjTrgSoftwareTriggerSimuMaker(StTriggerSimuMaker* simuTrig)
    : _simuTrig(simuTrig) { }
  virtual ~StjTrgSoftwareTriggerSimuMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);

  std::vector<int> jetPatches(int trgId);

private:

  StTriggerSimuMaker* _simuTrig;

};

#endif // STJTRGSOFTWARETRIGGERSIMUMAKER_H
