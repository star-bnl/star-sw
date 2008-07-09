// $Id: EnergyListToFourList.cxx,v 1.1 2008/07/09 10:58:09 tai Exp $
#include "EnergyListToFourList.h"

#include "../StMuTrackFourVec.h"

namespace StSpinJet {

FourList EnergyListToFourList::operator()(const TowerEnergyDepositList& energyDepositList)
{
  FourList ret;

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TLorentzVector p4 = constructFourMomentum((*it));
	    
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}

TLorentzVector EnergyListToFourList::constructFourMomentum(const TowerEnergyDeposit& deposit)
{
  TVector3 towerLocation(deposit.towerX, deposit.towerY, deposit.towerZ); 
  TVector3 vertex(deposit.vertexX, deposit.vertexY, deposit.vertexZ);

  TVector3 momentum = towerLocation - vertex;

  double mass(0); // assume photon mass

  double pMag = (deposit.energy > mass) ? sqrt(deposit.energy*deposit.energy - mass*mass) : deposit.energy;

  momentum.SetMag(pMag);

  return TLorentzVector(momentum.x(), momentum.y(), momentum.z(), deposit.energy);
}


}
