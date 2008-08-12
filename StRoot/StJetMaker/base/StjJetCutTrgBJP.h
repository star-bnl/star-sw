// -*- mode: c++;-*-
// $Id: StjJetCutTrgBJP.h,v 1.1 2008/08/12 04:01:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTTRGBJP_H
#define STJJETCUTTRGBJP_H

#include "StjJetCut.h"

#include "StjFourVecListCut.h"

class StjTrg;
class StjTrgJetPatchTowerIdMap;

class StjJetCutTrgBJP : public StjJetCut {

public:
  StjJetCutTrgBJP(StjTrg* trg, StjTrgJetPatchTowerIdMap* jetPatchTowerMap);
  virtual ~StjJetCutTrgBJP() { }

  bool operator()(const StjJet& jet);

private:

  StjTrg* _trg;
  StjFourVecListCut _fourVecListCut;

  ClassDef(StjJetCutTrgBJP, 1)

};

#endif // STJJETCUTTRGBJP_H
