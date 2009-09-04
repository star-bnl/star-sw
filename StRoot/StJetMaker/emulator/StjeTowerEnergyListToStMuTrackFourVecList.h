// -*- mode: c++;-*-
// $Id: StjeTowerEnergyListToStMuTrackFourVecList.h,v 1.3 2009/09/04 17:30:20 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

#include <StjTowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

class StjTowerEnergyToTLorentzVector;

class StMuTowerEmu;

class StjeTowerEnergyListToStMuTrackFourVecList {

public:
  StjeTowerEnergyListToStMuTrackFourVecList();
  virtual ~StjeTowerEnergyListToStMuTrackFourVecList() { }

  FourList operator()(const StjTowerEnergyList& energyDepositList);

private:

  StMuTowerEmu* createTowerEmu(const StjTowerEnergy& tower);

  StjTowerEnergyToTLorentzVector& _energyTo4p;

};

#endif // STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
