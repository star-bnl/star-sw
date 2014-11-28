// -*- mode: c++;-*-
// $Id: StjTrgSoftMuDstEmcTriggerMaker.h,v 1.1 2008/08/18 06:37:25 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
#define STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H

#include "StjTrgSoft.h"

#include <map>
#include <algorithm>

class StEmcTriggerMaker;

class StjTrgSoftGetAdcEt;

class StjTrgSoftMuDstEmcTriggerMaker : public StjTrgSoft {

public:
  StjTrgSoftMuDstEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker, StjTrgSoftGetAdcEt* adcEt);

  virtual ~StjTrgSoftMuDstEmcTriggerMaker() { }

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

  StEmcTriggerMaker* _emcTrigMaker;

  StjTrgSoftGetAdcEt* _adcEt;

  ClassDef(StjTrgSoftMuDstEmcTriggerMaker, 1)

};

#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
