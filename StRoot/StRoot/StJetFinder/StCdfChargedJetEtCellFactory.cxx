// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCellFactory.cxx,v 1.2 2008/05/05 00:32:47 tai Exp $
#include "StCdfChargedJetEtCellFactory.h"

#include "StCdfChargedJetEtCell.h"

StEtaPhiCell* StCdfChargedJetEtCellFactory::create(double etaMin, double etaMax, double phiMin, double phiMax)
{ 
  return new StCdfChargedJetEtCell(etaMin, etaMax, phiMin, phiMax);
}
