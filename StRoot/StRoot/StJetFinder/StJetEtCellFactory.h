// -*- mode: c++;-*-
// $Id: StJetEtCellFactory.h,v 1.2 2008/05/05 00:32:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETETCELLFACTORY_H
#define STJETETCELLFACTORY_H


class StEtaPhiCell;

class StJetEtCellFactory {

public:
  virtual StEtaPhiCell* create(double etaMin, double etaMax, double phiMin, double phiMax);
};


#endif // STJETETCELLFACTORY_H
