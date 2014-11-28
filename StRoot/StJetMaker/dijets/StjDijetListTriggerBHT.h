// -*- mode: c++;-*-
// $Id: StjDijetListTriggerBHT.h,v 1.2 2008/09/12 22:32:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTTRIGGERBHT2_H
#define STJDIJETLISTTRIGGERBHT2_H

#include "StjDijetListTrigger.h"

class StjTrg;
class StjJet;

class StjDijetListTriggerBHT : public StjDijetListTrigger {

public:
  StjDijetListTriggerBHT(StjTrg* trg) : _trg(trg) { }
  virtual ~StjDijetListTriggerBHT() { }

private:

  StjDijetList processOneItem(const StjDijetList::value_type& item);

  double getMaxEtForBEMCTowersThatPassedBHT(const StjJet& jet);

  double getTowerEtIfFiredBHT(const StjFourVec& p4);

  StjTrg* _trg;

  ClassDef(StjDijetListTriggerBHT, 1)

};

#endif // STJDIJETLISTTRIGGERBHT2_H
