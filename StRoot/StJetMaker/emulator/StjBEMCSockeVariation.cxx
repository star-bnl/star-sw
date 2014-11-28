// $Id: StjBEMCSockeVariation.cxx,v 1.1 2009/09/03 23:10:39 pibero Exp $
#include "StjBEMCSockeVariation.h"

#include "StjTowerEnergyListVariation.h"

ClassImp(StjBEMCSockeVariation)

StjTowerEnergyList StjBEMCSockeVariation::getEnergyList()
{
  return (*_variation)(_src->getEnergyList());
}
