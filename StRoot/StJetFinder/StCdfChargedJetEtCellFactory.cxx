// -*- mode: c++;-*-
// $Id: StCdfChargedJetEtCellFactory.cxx,v 1.1 2008/04/29 00:11:09 tai Exp $
#include "StCdfChargedJetEtCellFactory.h"

#include "StCdfChargedJetEtCell.h"

StJetEtCell* StCdfChargedJetEtCellFactory::create(double etaMin, double etaMax, double phiMin, double phiMax)
{ 
  return new StCdfChargedJetEtCell(etaMin, etaMax, phiMin, phiMax);
}
