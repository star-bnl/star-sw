// $Id: TowerEnergyToTLorentzVectorWithId.cxx,v 1.1 2008/07/15 06:23:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "TowerEnergyToTLorentzVectorWithId.h"

#include <TowerEnergyList.h>

namespace StSpinJet {

TLorentzVectorWithId TowerEnergyToTLorentzVectorWithId::operator()(const TowerEnergy& deposit)
{
  TLorentzVectorWithId p4(_towerenergy2tlorentzvector(deposit));
  p4.runNumber   = deposit.runNumber;
  p4.eventId     = deposit.eventId;
  p4.type        = 2;     
  p4.detectorId  = deposit.detectorId;
  p4.trackId     = 0;
  p4.towerId     = deposit.towerId;
  return p4;
}

}
