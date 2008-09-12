// -*- mode: c++;-*-
// $Id: StjDijetListTriggerBJP.h,v 1.2 2008/09/12 22:32:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTTRIGGERBJP2_H
#define STJDIJETLISTTRIGGERBJP2_H

#include "StjDijetListTrigger.h"

class StjTrg;
class StjTrgBEMCJetPatchTowerIdMap;

class StjDijetListTriggerBJP : public StjDijetListTrigger {

public:
  StjDijetListTriggerBJP(StjTrg* trg, StjTrgBEMCJetPatchTowerIdMap* jetPatchTowerMap)
    : _trg(trg), _jetPatchTowerMap(jetPatchTowerMap) { }
  virtual ~StjDijetListTriggerBJP() { }

private:

  StjDijetList processOneItem(const StjDijetList::value_type& item);

  double getMaxEtForBEMCJetPatchThatPassedBJP(const StjJet& jet);

  double getJetPatchEtIfFiredBJP(const StjFourVec& p4);

  StjTrg* _trg;

  StjTrgBEMCJetPatchTowerIdMap* _jetPatchTowerMap;

  ClassDef(StjDijetListTriggerBJP, 1)

};

#endif // STJDIJETLISTTRIGGERBJP2_H
