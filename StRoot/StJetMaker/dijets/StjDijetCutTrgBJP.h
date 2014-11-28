// -*- mode: c++;-*-
// $Id: StjDijetCutTrgBJP.h,v 1.1 2008/09/11 23:34:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTTRGBJP_H
#define STJDIJETCUTTRGBJP_H

#include "StjDijetCut.h"

#include "StjJetCutTrgBJP.h"

class StjTrg;
class StjTrgBEMCJetPatchTowerIdMap;

class StjDijetCutTrgBJP : public StjDijetCut {

public:
  StjDijetCutTrgBJP(StjTrg* trg, StjTrgBEMCJetPatchTowerIdMap* jetPatchTowerMap)
    : _jetCut(trg, jetPatchTowerMap) { }
  virtual ~StjDijetCutTrgBJP() { }

  bool operator()(const StjDijet& dijet)
  {
    if(_jetCut(dijet.jet3) && _jetCut(dijet.jet4)) return true;

    return false;
  }

private:

  StjJetCutTrgBJP _jetCut;

  ClassDef(StjDijetCutTrgBJP, 1)

};

#endif // STJDIJETCUTTRGBJP_H
