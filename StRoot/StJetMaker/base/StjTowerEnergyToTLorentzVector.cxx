// $Id: StjTowerEnergyToTLorentzVector.cxx,v 1.2 2008/08/02 19:22:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyToTLorentzVector.h"

#include "StjTowerEnergyList.h"

namespace StSpinJet {

TLorentzVector StjTowerEnergyToTLorentzVector::operator()(const StjTowerEnergy& deposit)
{
  TVector3 towerLocation;
  towerLocation.SetPtEtaPhi(deposit.towerR, deposit.towerEta, deposit.towerPhi); 
  TVector3 vertex(deposit.vertexX, deposit.vertexY, deposit.vertexZ);

  TVector3 momentum = towerLocation - vertex;

  double pMag = (deposit.energy > _mass) ? sqrt(deposit.energy*deposit.energy - _mass*_mass) : deposit.energy;

  momentum.SetMag(pMag);
  TLorentzVector ret(momentum.x(), momentum.y(), momentum.z(), deposit.energy);
  return ret;
}

}
