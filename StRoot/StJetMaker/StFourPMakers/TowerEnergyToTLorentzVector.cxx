// $Id: TowerEnergyToTLorentzVector.cxx,v 1.3 2008/07/15 06:23:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "TowerEnergyToTLorentzVector.h"

#include <TowerEnergyList.h>

namespace StSpinJet {

TLorentzVector TowerEnergyToTLorentzVector::operator()(const TowerEnergy& deposit)
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
