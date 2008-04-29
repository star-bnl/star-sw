// -*- mode: c++;-*-
// $Id: StJetEtCellFactory.h,v 1.1 2008/04/29 00:11:10 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETETCELLFACTORY_H
#define STJETETCELLFACTORY_H


class StJetEtCell;

class StJetEtCellFactory {

public:
  virtual StJetEtCell* create(double etaMin, double etaMax, double phiMin, double phiMax);
};


#endif // STJETETCELLFACTORY_H
