// -*- mode: c++;-*-
// $Id: StjJetCutTrgBHT.h,v 1.1 2008/08/12 04:01:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTTRGBHT_H
#define STJJETCUTTRGBHT_H

#include "StjJetCut.h"

#include "StjFourVecListCut.h"

class StjTrg;

class StjJetCutTrgBHT : public StjJetCut {

public:
  StjJetCutTrgBHT(StjTrg* trg);
  virtual ~StjJetCutTrgBHT() { }

  bool operator()(const StjJet& jet);

private:

  StjTrg* _trg;
  StjFourVecListCut _fourVecListCut;

  ClassDef(StjJetCutTrgBHT, 1)

};

#endif // STJJETCUTTRGBHT_H
