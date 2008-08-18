// -*- mode: c++;-*-
// $Id: StjTrgSoftMuDstTriggerSimuMaker.h,v 1.1 2008/08/18 06:37:26 tai Exp $
#ifndef STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
#define STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H

#include "StjTrgSoft.h"

#include <map>
#include <algorithm>

class StTriggerSimuMaker;

class StjTrgSoftGetAdcEt;

class StjTrgSoftMuDstTriggerSimuMaker : public StjTrgSoft {

public:

  StjTrgSoftMuDstTriggerSimuMaker(StTriggerSimuMaker* simuTrig, StjTrgSoftGetAdcEt* adcEt);
  virtual ~StjTrgSoftMuDstTriggerSimuMaker() { }

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

  StjTrgSoftGetAdcEt* _adcEt;

  ClassDef(StjTrgSoftMuDstTriggerSimuMaker, 1)

};

#endif // STJTRGMUDSTSOFTWARETRIGGERSIMUMAKER_H
