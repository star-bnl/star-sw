// -*- mode: c++;-*-
// $Id: StjeTowerEnergyListToStMuTrackFourVecList.h,v 1.2 2008/08/03 00:26:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

#include <StjTowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

class StjTowerEnergyToTLorentzVector;

class StjeTowerEnergyListToStMuTrackFourVecList {

public:
  StjeTowerEnergyListToStMuTrackFourVecList();
  virtual ~StjeTowerEnergyListToStMuTrackFourVecList() { }

  FourList operator()(const StjTowerEnergyList& energyDepositList);

private:

  StjTowerEnergyToTLorentzVector& _energyTo4p;

};

#endif // STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
