// $Id: StJetEtCellFactory.cxx,v 1.2 2008/04/30 01:43:09 tai Exp $
#include "StJetEtCellFactory.h"

#include "StJetEtCell.h"

StJetEtCell* StJetEtCellFactory::create(double etaMin, double etaMax, double phiMin, double phiMax)
{ 
  return new StJetEtCell(etaMin, etaMax, phiMin, phiMax);
}
