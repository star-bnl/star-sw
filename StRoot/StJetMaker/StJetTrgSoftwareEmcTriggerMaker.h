// -*- mode: c++;-*-
// $Id: StJetTrgSoftwareEmcTriggerMaker.h,v 1.1 2008/07/23 20:25:43 tai Exp $
#ifndef STJETTRGSOFTWAREEMCTRIGGERMAKER_H
#define STJETTRGSOFTWAREEMCTRIGGERMAKER_H

#include "StJetTrgSoftware.h"

#include <map>
#include <algorithm>

class StEmcTriggerMaker;

class StJetTrgSoftwareEmcTriggerMaker : public StJetTrgSoftware {

public:
  StJetTrgSoftwareEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker)
    : _emcTrigMaker(emcTrigMaker) { }
  virtual ~StJetTrgSoftwareEmcTriggerMaker() { }

  bool soft(int trgId);

  std::vector<int> towers(int trgId);

  std::vector<int> jetPatches(int trgId);

private:

  StEmcTriggerMaker* _emcTrigMaker;

};

#endif // STJETTRGSOFTWAREEMCTRIGGERMAKER_H
