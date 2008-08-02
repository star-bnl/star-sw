// $Id: StjTowerEnergyToFourVec.cxx,v 1.2 2008/08/02 19:22:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyToFourVec.h"

#include "StjTowerEnergyList.h"
#include "StjFourVecList.h"

namespace StSpinJet {

StjFourVec StjTowerEnergyToFourVec::operator()(const StjTowerEnergy& towerEnergy)
{
  StjFourVec ret;
  ret.runNumber   = towerEnergy.runNumber;
  ret.eventId     = towerEnergy.eventId;
  ret.type        = 2;     
  ret.detectorId  = towerEnergy.detectorId;
  ret.trackId     = 0;
  ret.towerId     = towerEnergy.towerId;

  TLorentzVector p4(_towerenergy2tlorentzvector(towerEnergy));
  ret.pt  = p4.Pt();
  ret.eta = p4.Eta();
  ret.phi = p4.Phi();
  ret.m   = p4.M();
  return ret;
}

}
