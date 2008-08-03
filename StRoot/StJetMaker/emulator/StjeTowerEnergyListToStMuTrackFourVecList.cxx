// $Id: StjeTowerEnergyListToStMuTrackFourVecList.cxx,v 1.2 2008/08/03 00:26:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeTowerEnergyListToStMuTrackFourVecList.h"

#include "StjTowerEnergyToTLorentzVector.h"

#include "../emulator/StMuTrackFourVec.h"

StjeTowerEnergyListToStMuTrackFourVecList::StjeTowerEnergyListToStMuTrackFourVecList()
  : _energyTo4p(*(new StjTowerEnergyToTLorentzVector)) { }

FourList StjeTowerEnergyListToStMuTrackFourVecList::operator()(const StjTowerEnergyList& energyDepositList)
{
  FourList ret;

  for(StjTowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TLorentzVector p4 = _energyTo4p(*it);	    

    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}
