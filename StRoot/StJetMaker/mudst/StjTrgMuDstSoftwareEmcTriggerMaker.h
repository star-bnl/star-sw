// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareEmcTriggerMaker.h,v 1.3 2008/08/18 06:20:45 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
#define STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H

#include "StjTrgMuDstSoftware.h"

#include <map>
#include <algorithm>

class StEmcTriggerMaker;

class StjTrgMuDstSoftwareGetAdcEt;

class StjTrgMuDstSoftwareEmcTriggerMaker : public StjTrgMuDstSoftware {

public:
  StjTrgMuDstSoftwareEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker, StjTrgMuDstSoftwareGetAdcEt* adcEt);

  virtual ~StjTrgMuDstSoftwareEmcTriggerMaker() { }

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

  StjTrgMuDstSoftwareGetAdcEt* _adcEt;

  ClassDef(StjTrgMuDstSoftwareEmcTriggerMaker, 1)

};

#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
