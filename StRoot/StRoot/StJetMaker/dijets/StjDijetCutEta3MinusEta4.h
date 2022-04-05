// -*- mode: c++;-*-
// $Id: StjDijetCutEta3MinusEta4.h,v 1.1 2008/09/11 23:34:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTETA3MINUSETA4_H
#define STJDIJETCUTETA3MINUSETA4_H

#include "StjDijetCut.h"

class StjDijetCutEta3MinusEta4 : public StjDijetCut {

public:
  StjDijetCutEta3MinusEta4(double max = 0.5, double min = 0.0)
    : _min(min), _max(max) { }
  virtual ~StjDijetCutEta3MinusEta4() { }

  bool operator()(const StjDijet& dijet)
  {
    if(dijet.jet3.eta - dijet.jet4.eta < _min) return true;

    if(dijet.jet3.eta - dijet.jet4.eta > _max) return true;

    return false;
  }

private:

  double _min;

  double _max;

  ClassDef(StjDijetCutEta3MinusEta4, 1)

};

#endif // STJDIJETCUTETA3MINUSETA4_H
