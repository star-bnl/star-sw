// -*- mode: c++;-*-
// $Id: StjTrgSoftwareEmcTriggerMaker.h,v 1.1 2008/08/02 04:07:33 tai Exp $
#ifndef STJETTRGSOFTWAREEMCTRIGGERMAKER_H
#define STJETTRGSOFTWAREEMCTRIGGERMAKER_H

#include "StjTrgSoftware.h"

#include <map>
#include <algorithm>

class StEmcTriggerMaker;

class StJetTrgSoftwareEmcTriggerMaker : public StJetTrgSoftware {

public:
  StJetTrgSoftwareEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  StJetTrgSoftwareEmcTriggerMaker(int trgId, StEmcTriggerMaker* emcTrigMaker)
    : _trgId(trgId), _emcTrigMaker(emcTrigMaker) { }
  virtual ~StJetTrgSoftwareEmcTriggerMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);

  std::vector<int> jetPatches(int trgId);

private:

  int _trgId;

  StEmcTriggerMaker* _emcTrigMaker;

};

#endif // STJETTRGSOFTWAREEMCTRIGGERMAKER_H
