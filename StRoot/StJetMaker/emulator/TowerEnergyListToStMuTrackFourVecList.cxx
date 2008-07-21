// $Id: TowerEnergyListToStMuTrackFourVecList.cxx,v 1.1 2008/07/21 22:15:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "TowerEnergyListToStMuTrackFourVecList.h"

#include "TowerEnergyToTLorentzVector.h"

#include "../emulator/StMuTrackFourVec.h"

namespace StSpinJet {

TowerEnergyListToStMuTrackFourVecList::TowerEnergyListToStMuTrackFourVecList()
  : _energyTo4p(*(new TowerEnergyToTLorentzVector)) { }

FourList TowerEnergyListToStMuTrackFourVecList::operator()(const TowerEnergyList& energyDepositList)
{
  FourList ret;

  for(TowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TLorentzVector p4 = _energyTo4p(*it);	    

    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}


}
