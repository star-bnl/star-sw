// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareTriggerSimuMaker.h,v 1.1 2008/08/08 23:12:26 tai Exp $
#ifndef STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
#define STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H

#include "StjTrgMuDstSoftware.h"

#include <map>
#include <algorithm>

class StTriggerSimuMaker;

class StjTrgMuDstSoftwareTriggerSimuMaker : public StjTrgMuDstSoftware {

public:
  StjTrgMuDstSoftwareTriggerSimuMaker(StTriggerSimuMaker* simuTrig)
    : _simuTrig(simuTrig) { }
  virtual ~StjTrgMuDstSoftwareTriggerSimuMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);

  std::vector<int> jetPatches(int trgId);

private:

  StTriggerSimuMaker* _simuTrig;

};

#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
