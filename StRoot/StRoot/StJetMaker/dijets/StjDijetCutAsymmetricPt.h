// -*- mode: c++;-*-
// $Id: StjDijetCutAsymmetricPt.h,v 1.1 2008/09/11 23:34:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTASYMMETRICPT_H
#define STJDIJETCUTASYMMETRICPT_H

#include "StjDijetCut.h"

class StjDijetCutAsymmetricPt : public StjDijetCut {

public:
  StjDijetCutAsymmetricPt(double minMin, double maxMin)
    : _minMin(minMin), _maxMin(maxMin) { }
  virtual ~StjDijetCutAsymmetricPt() { }

  bool operator()(const StjDijet& dijet)
  {
    if(dijet.jet3.pt >= dijet.jet4.pt) {
      if(dijet.jet3.pt <= _maxMin) return true;
      if(dijet.jet4.pt <= _minMin) return true;
    } else {
      if(dijet.jet4.pt <= _maxMin) return true;
      if(dijet.jet3.pt <= _minMin) return true;
    }
    return false;
  }

private:

  double _minMin;
  double _maxMin;

  ClassDef(StjDijetCutAsymmetricPt, 1)

};

#endif // STJDIJETCUTASYMMETRICPT_H
