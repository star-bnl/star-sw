// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCellFactory.h,v 1.2 2008/05/05 00:32:48 tai Exp $
#ifndef STCDFCHARGEDJETETCELLFACTORY_H
#define STCDFCHARGEDJETETCELLFACTORY_H

#include "StJetEtCellFactory.h"

class StCdfChargedJetEtCellFactory : public StJetEtCellFactory {

public:
  StEtaPhiCell* create(double etaMin, double etaMax, double phiMin, double phiMax);

};

#endif  // STCDFCHARGEDJETETCELLFACTORY_H
