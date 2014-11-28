// -*- mode: c++;-*-
// $Id: StjDijetCutEta.h,v 1.1 2008/09/11 23:34:47 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTETA_H
#define STJDIJETCUTETA_H

#include "StjDijetCut.h"

#include "StjJetCutEta.h"

class StjDijetCutEta : public StjDijetCut {

public:
  StjDijetCutEta(double min = -10.0, double max = 10.0)
    : _jetcut(min, max) { }
  virtual ~StjDijetCutEta() { }

  bool operator()(const StjDijet& dijet)
  {
    if(_jetcut(dijet.jet3)) return true;

    if(_jetcut(dijet.jet4)) return true;

    return false;
  }

private:

  StjJetCutEta _jetcut;

  ClassDef(StjDijetCutEta, 1)

};

#endif // STJDIJETCUTETA_H
