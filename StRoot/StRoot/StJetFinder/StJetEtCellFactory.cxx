// $Id: StJetEtCellFactory.cxx,v 1.3 2008/05/05 00:32:49 tai Exp $
#include "StJetEtCellFactory.h"

#include "StJetEtCell.h"

StEtaPhiCell* StJetEtCellFactory::create(double etaMin, double etaMax, double phiMin, double phiMax)
{ 
  return new StJetEtCell(etaMin, etaMax, phiMin, phiMax);
}
