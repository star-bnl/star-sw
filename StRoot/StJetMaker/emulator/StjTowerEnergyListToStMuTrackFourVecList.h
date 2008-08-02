// -*- mode: c++;-*-
// $Id: StjTowerEnergyListToStMuTrackFourVecList.h,v 1.1 2008/08/02 04:18:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

#include <StjTowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class TowerEnergyToTLorentzVector;

class TowerEnergyListToStMuTrackFourVecList {

public:
  TowerEnergyListToStMuTrackFourVecList();
  virtual ~TowerEnergyListToStMuTrackFourVecList() { }

  FourList operator()(const TowerEnergyList& energyDepositList);

private:

  TowerEnergyToTLorentzVector& _energyTo4p;

};

}


#endif // TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
