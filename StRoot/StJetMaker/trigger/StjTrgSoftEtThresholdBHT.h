// -*- mode: c++;-*-
// $Id: StjTrgSoftEtThresholdBHT.h,v 1.2 2008/08/21 22:23:05 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGSOFTETTHRESHOLDBHT_H
#define STJTRGSOFTETTHRESHOLDBHT_H

#include <StjTrgSoft.h>

#include <StjTowerEnergyListCut.h>

#include <vector>

class StjBEMC;

class StjTrgSoftEtThresholdBHT : public StjTrgSoft {

public:
  StjTrgSoftEtThresholdBHT(StjBEMC* bemc, double minEt);
  virtual ~StjTrgSoftEtThresholdBHT() { }

  bool soft();

  std::vector<int> towers();
  std::vector<int> towerDsmAdc();
  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

private:

  bool isNewEvent();
  void read();

  StjBEMC* _bemc;
  double _minEt;

  int _runNumber;
  int _eventId;

  bool                      _passed;
  std::vector<int>          _towers;
  std::vector<int>          _towerDsmAdc;
  std::vector<unsigned int> _towerAdc;
  std::vector<double>       _towerEnergy;
  std::vector<double>       _towerEt;

  StjTowerEnergyListCut _cut;

  ClassDef(StjTrgSoftEtThresholdBHT, 1)

};

#endif // STJTRGSOFTETTHRESHOLDBHT_H
