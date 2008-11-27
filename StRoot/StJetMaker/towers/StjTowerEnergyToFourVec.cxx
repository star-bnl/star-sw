// $Id: StjTowerEnergyToFourVec.cxx,v 1.1 2008/11/27 07:35:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyToFourVec.h"

#include "StjTowerEnergyList.h"
#include "StjFourVecList.h"

ClassImp(StjTowerEnergyToFourVec)

StjFourVec StjTowerEnergyToFourVec::operator()(const StjTowerEnergy& towerEnergy)
{
  StjFourVec ret;
  ret.runNumber   = towerEnergy.runNumber;
  ret.eventId     = towerEnergy.eventId;
  ret.type        = 2;     
  ret.detectorId  = towerEnergy.detectorId;
  ret.trackId     = 0;
  ret.towerId     = towerEnergy.towerId;
  ret.mcparticleId = 0;
  ret.vertexZ     = towerEnergy.vertexZ;

  TLorentzVector p4(_towerenergy2tlorentzvector(towerEnergy));
  ret.pt  = p4.Pt();
  ret.eta = p4.Eta();
  ret.phi = p4.Phi();
  ret.m   = p4.M();
  return ret;
}
