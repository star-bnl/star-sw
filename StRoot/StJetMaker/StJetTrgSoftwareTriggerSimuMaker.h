// -*- mode: c++;-*-
// $Id: StJetTrgSoftwareTriggerSimuMaker.h,v 1.1 2008/07/23 20:25:43 tai Exp $
#ifndef STJETTRGSOFTWARETRIGGERSIMUMAKER_H
#define STJETTRGSOFTWARETRIGGERSIMUMAKER_H

#include "StJetTrgSoftware.h"

#include <map>
#include <algorithm>

class StTriggerSimuMaker;

class StJetTrgSoftwareTriggerSimuMaker : public StJetTrgSoftware {

public:
  StJetTrgSoftwareTriggerSimuMaker(StTriggerSimuMaker* simuTrig)
    : _simuTrig(simuTrig) { }
  virtual ~StJetTrgSoftwareTriggerSimuMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);

  std::vector<int> jetPatches(int trgId);

private:

  StTriggerSimuMaker* _simuTrig;

};

#endif // STJETTRGSOFTWARETRIGGERSIMUMAKER_H
