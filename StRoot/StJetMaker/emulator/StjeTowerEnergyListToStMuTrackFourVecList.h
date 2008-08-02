// -*- mode: c++;-*-
// $Id: StjeTowerEnergyListToStMuTrackFourVecList.h,v 1.1 2008/08/02 23:10:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

#include <StjTowerEnergyList.h>

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class StjTowerEnergyToTLorentzVector;

class StjeTowerEnergyListToStMuTrackFourVecList {

public:
  StjeTowerEnergyListToStMuTrackFourVecList();
  virtual ~StjeTowerEnergyListToStMuTrackFourVecList() { }

  FourList operator()(const StjTowerEnergyList& energyDepositList);

private:

  StjTowerEnergyToTLorentzVector& _energyTo4p;

};

}


#endif // STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
