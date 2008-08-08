// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareEmcTriggerMaker.h,v 1.1 2008/08/08 23:12:25 tai Exp $
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

  std::vector<int> jetPatches(int trgId);

private:

  int _trgId;

  StEmcTriggerMaker* _emcTrigMaker;

};

#endif // STJTRGMUDSTSOFTWAREEMCTRIGGERMAKER_H
