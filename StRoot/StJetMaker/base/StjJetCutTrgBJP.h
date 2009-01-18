// -*- mode: c++;-*-
// $Id: StjJetCutTrgBJP.h,v 1.2 2008/08/12 04:06:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTTRGBJP_H
#define STJJETCUTTRGBJP_H

#include "StjJetCut.h"

#include "StjFourVecListCut.h"

class StjTrg;
class StjTrgBEMCJetPatchTowerIdMap;

class StjJetCutTrgBJP : public StjJetCut {

public:
  StjJetCutTrgBJP(StjTrg* trg, StjTrgBEMCJetPatchTowerIdMap* jetPatchTowerMap);
  virtual ~StjJetCutTrgBJP() { }

  bool operator()(const StjJet& jet);

private:

  StjTrg* _trg;
  StjFourVecListCut _fourVecListCut;

  ClassDef(StjJetCutTrgBJP, 1)

};

#endif // STJJETCUTTRGBJP_H
