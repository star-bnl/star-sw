// $Id: EnergyListToFourList.cxx,v 1.4 2008/07/13 10:02:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "EnergyListToFourList.h"

#include "../StMuTrackFourVec.h"

namespace StSpinJet {

FourList EnergyListToFourList::operator()(const TowerEnergyList& energyDepositList)
{
  FourList ret;

  for(TowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TLorentzVector p4 = constructFourMomentum((*it));
	    
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}

TLorentzVector EnergyListToFourList::constructFourMomentum(const TowerEnergy& deposit)
{
  TVector3 towerLocation;
  towerLocation.SetPtEtaPhi(deposit.towerR, deposit.towerEta, deposit.towerPhi); 
  TVector3 vertex(deposit.vertexX, deposit.vertexY, deposit.vertexZ);

  TVector3 momentum = towerLocation - vertex;

  double mass(0); // assume photon mass

  double pMag = (deposit.energy > mass) ? sqrt(deposit.energy*deposit.energy - mass*mass) : deposit.energy;

  momentum.SetMag(pMag);

  return TLorentzVector(momentum.x(), momentum.y(), momentum.z(), deposit.energy);
}


}
