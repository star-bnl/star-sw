// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareTriggerSimuMaker.h,v 1.2 2008/08/17 11:29:15 tai Exp $
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
  std::vector<int> towerDsmAdc(int trgId);

  std::vector<int> jetPatches(int trgId);
  std::vector<int> jetPatchDsmAdc(int trgId);

private:

  StTriggerSimuMaker* _simuTrig;

  ClassDef(StjTrgMuDstSoftwareTriggerSimuMaker, 1)

};

#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
