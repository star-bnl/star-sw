// -*- mode: c++;-*-
// $Id: StjFourVecCutTrgBJP.h,v 1.1 2008/11/27 07:29:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUTTRGBJP_H
#define STJFOURVECCUTTRGBJP_H

#include "StjFourVecCut.h"

#include <vector>

class StjTrg;
class StjTrgBEMCJetPatchTowerIdMap;

class StjFourVecCutTrgBJP : public StjFourVecCut {

public:
  StjFourVecCutTrgBJP(StjTrg* trg, StjTrgBEMCJetPatchTowerIdMap* jetPatchTowerMap)
    : _trg(trg), _jetPatchTowerMap(jetPatchTowerMap) { }
  virtual ~StjFourVecCutTrgBJP() { }

  bool operator()(const StjFourVec& p4);

private:

  std::vector<int> getTowersFor(const std::vector<int>& jetPatches);

  StjTrg* _trg;

  StjTrgBEMCJetPatchTowerIdMap* _jetPatchTowerMap;

  ClassDef(StjFourVecCutTrgBJP, 1)

};

#endif // STJFOURVECCUTTRGBJP_H
