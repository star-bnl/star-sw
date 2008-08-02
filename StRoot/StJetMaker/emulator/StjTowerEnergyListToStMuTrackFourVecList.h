// -*- mode: c++;-*-
// $Id: StjTowerEnergyListToStMuTrackFourVecList.h,v 1.3 2008/08/02 22:43:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
#define STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H

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


#endif // STJTOWERENERGYLISTTOSTMUTRACKFOURVECLIST_H
