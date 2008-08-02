// -*- mode: c++;-*-
// $Id: StjTowerEnergyListToStMuTrackFourVecList.h,v 1.2 2008/08/02 19:23:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

#include <StjTowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class StjTowerEnergyToTLorentzVector;

class StjTowerEnergyListToStMuTrackFourVecList {

public:
  StjTowerEnergyListToStMuTrackFourVecList();
  virtual ~StjTowerEnergyListToStMuTrackFourVecList() { }

  FourList operator()(const StjTowerEnergyList& energyDepositList);

private:

  StjTowerEnergyToTLorentzVector& _energyTo4p;

};

}


#endif // TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
