// -*- mode: c++;-*-
// $Id: TowerEnergyListToStMuTrackFourVecList.h,v 1.1 2008/07/21 22:15:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define TOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

#include <TowerEnergyList.h>

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
