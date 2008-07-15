// $Id: EnergyListToFourList.cxx,v 1.5 2008/07/15 03:42:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "EnergyListToFourList.h"

#include "TowerEnergyToTLorentzVector.h"

#include "../StMuTrackFourVec.h"

namespace StSpinJet {

EnergyListToFourList::EnergyListToFourList()
  : _energyTo4p(*(new TowerEnergyToTLorentzVector)) { }

FourList EnergyListToFourList::operator()(const TowerEnergyList& energyDepositList)
{
  FourList ret;

  for(TowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TLorentzVector p4 = _energyTo4p((*it));	    

    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}


}
