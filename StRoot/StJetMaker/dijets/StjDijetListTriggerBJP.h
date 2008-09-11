// -*- mode: c++;-*-
// $Id: StjDijetListTriggerBJP.h,v 1.1 2008/09/11 23:34:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETLISTTRIGGERBJP2_H
#define STJDIJETLISTTRIGGERBJP2_H

#include "StjDijetListTrigger.h"

class StjTrg;
class StjTrgBEMCJetPatchTowerIdMap;

class StjDijetListTriggerBJP : public StjDijetListTrigger {

public:
  StjDijetListTriggerBJP(StjTrg* trg, StjTrgBEMCJetPatchTowerIdMap* jetPatchTowerMap) { }
  virtual ~StjDijetListTriggerBJP() { }

private:

  StjDijetList processOneItem(const StjDijetList::value_type& item);

  ClassDef(StjDijetListTriggerBJP, 1)

};

#endif // STJDIJETLISTTRIGGERBJP2_H
