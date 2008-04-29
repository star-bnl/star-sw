// -*- mode: c++;-*-
// $Id: StJetEtCellFactory.cxx,v 1.1 2008/04/29 00:11:10 tai Exp $
#include "StJetEtCellFactory.h"

#include "StJetEtCell.h"

StJetEtCell* StJetEtCellFactory::create(double etaMin, double etaMax, double phiMin, double phiMax)
{ 
  return new StJetEtCell(etaMin, etaMax, phiMin, phiMax);
}
