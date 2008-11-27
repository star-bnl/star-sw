// -*- mode: c++;-*-
// $Id: StjFourVecCutTrgBHT.h,v 1.1 2008/11/27 07:29:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUTTRGBHT_H
#define STJFOURVECCUTTRGBHT_H

#include "StjFourVecCut.h"

class StjTrg;

class StjFourVecCutTrgBHT : public StjFourVecCut {

public:
  StjFourVecCutTrgBHT(StjTrg* trg) : _trg(trg) { }
  virtual ~StjFourVecCutTrgBHT() { }

  bool operator()(const StjFourVec& p4);

private:

  StjTrg* _trg;

  ClassDef(StjFourVecCutTrgBHT, 1)

};

#endif // STJFOURVECCUTTRGBHT_H
