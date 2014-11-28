// -*- mode: c++;-*-
// $Id: StjDijetCutTrgBHT.h,v 1.1 2008/09/11 23:34:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTTRGBHT_H
#define STJDIJETCUTTRGBHT_H

#include "StjDijetCut.h"

#include "StjJetCutTrgBHT.h"

class StjTrg;

class StjDijetCutTrgBHT : public StjDijetCut {

public:
  StjDijetCutTrgBHT(StjTrg* trg) : _jetCut(trg) { }
  virtual ~StjDijetCutTrgBHT() { }

  bool operator()(const StjDijet& dijet)
  {
    if(_jetCut(dijet.jet3) && _jetCut(dijet.jet4)) return true;

    return false;
  }

private:

  StjJetCutTrgBHT _jetCut;

  ClassDef(StjDijetCutTrgBHT, 1)

};

#endif // STJDIJETCUTTRGBHT_H
