// -*- mode: c++;-*-
// $Id: StjTrgSoftwareEmcTriggerMaker.h,v 1.2 2008/08/02 19:22:30 tai Exp $
#ifndef STJETTRGSOFTWAREEMCTRIGGERMAKER_H
#define STJETTRGSOFTWAREEMCTRIGGERMAKER_H

#include "StjTrgSoftware.h"

#include <map>
#include <algorithm>

class StEmcTriggerMaker;

class StjTrgSoftwareEmcTriggerMaker : public StjTrgSoftware {

public:
  StjTrgSoftwareEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  StjTrgSoftwareEmcTriggerMaker(int trgId, StEmcTriggerMaker* emcTrigMaker)
    : _trgId(trgId), _emcTrigMaker(emcTrigMaker) { }
  virtual ~StjTrgSoftwareEmcTriggerMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);

  std::vector<int> jetPatches(int trgId);

private:

  int _trgId;

  StEmcTriggerMaker* _emcTrigMaker;

};

#endif // STJETTRGSOFTWAREEMCTRIGGERMAKER_H
