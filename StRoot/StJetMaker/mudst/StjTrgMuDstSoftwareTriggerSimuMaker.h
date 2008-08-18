// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareTriggerSimuMaker.h,v 1.3 2008/08/18 06:20:47 tai Exp $
#ifndef STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
#define STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H

#include "StjTrgMuDstSoftware.h"

#include <map>
#include <algorithm>

class StTriggerSimuMaker;

class StjTrgMuDstSoftwareGetAdcEt;

class StjTrgMuDstSoftwareTriggerSimuMaker : public StjTrgMuDstSoftware {

public:

  StjTrgMuDstSoftwareTriggerSimuMaker(StTriggerSimuMaker* simuTrig, StjTrgMuDstSoftwareGetAdcEt* adcEt);
  virtual ~StjTrgMuDstSoftwareTriggerSimuMaker() { }

  bool soft();

  std::vector<int> towers();
  std::vector<int> towerDsmAdc();
  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

  std::vector<int> jetPatches();
  std::vector<int> jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();
  std::vector<double> jetPatchEt();

  void setTrg(StjTrg* trg);

private:

  StTriggerSimuMaker* _simuTrig;

  StjTrgMuDstSoftwareGetAdcEt* _adcEt;

  ClassDef(StjTrgMuDstSoftwareTriggerSimuMaker, 1)

};

#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
