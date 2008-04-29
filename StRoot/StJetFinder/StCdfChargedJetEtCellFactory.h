// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCellFactory.h,v 1.1 2008/04/29 00:11:09 tai Exp $
#ifndef STCDFCHARGEDJETETCELLFACTORY_H
#define STCDFCHARGEDJETETCELLFACTORY_H

#include "StJetEtCellFactory.h"

class StCdfChargedJetEtCellFactory : public StJetEtCellFactory {

public:
  StJetEtCell* create(double etaMin, double etaMax, double phiMin, double phiMax);

};

#endif  // STCDFCHARGEDJETETCELLFACTORY_H
