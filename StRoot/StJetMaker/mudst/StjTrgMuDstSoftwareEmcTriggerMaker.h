// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareEmcTriggerMaker.h,v 1.2 2008/08/17 11:29:15 tai Exp $
#ifndef STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
#define STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H

#include "StjTrgMuDstSoftware.h"

#include <map>
#include <algorithm>

class StEmcTriggerMaker;

class StjTrgMuDstSoftwareEmcTriggerMaker : public StjTrgMuDstSoftware {

public:
  StjTrgMuDstSoftwareEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  StjTrgMuDstSoftwareEmcTriggerMaker(int trgId, StEmcTriggerMaker* emcTrigMaker)
    : _trgId(trgId), _emcTrigMaker(emcTrigMaker) { }
  virtual ~StjTrgMuDstSoftwareEmcTriggerMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);
  std::vector<int> towerDsmAdc(int trgId);

  std::vector<int> jetPatches(int trgId);
  std::vector<int> jetPatchDsmAdc(int trgId);

private:

  int _trgId;

  StEmcTriggerMaker* _emcTrigMaker;

  ClassDef(StjTrgMuDstSoftwareEmcTriggerMaker, 1)

};

#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
