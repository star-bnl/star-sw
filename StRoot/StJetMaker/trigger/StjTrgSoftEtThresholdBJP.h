// -*- mode: c++;-*-
// $Id: StjTrgSoftEtThresholdBJP.h,v 1.2 2008/08/21 22:23:05 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGSOFTETTHRESHOLDBJP_H
#define STJTRGSOFTETTHRESHOLDBJP_H

#include <StjTrgSoft.h>

#include <StjTowerEnergyList.h>
#include <StjTowerEnergyListCut.h>

#include <vector>

class StjBEMC;
class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgSoftEtThresholdBJP : public StjTrgSoft {

public:
  StjTrgSoftEtThresholdBJP(StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* jpTowerMap, double minEt);
  virtual ~StjTrgSoftEtThresholdBJP() { }

  bool soft();

  std::vector<int> jetPatches();
  std::vector<int> jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();
  std::vector<double> jetPatchEt();

private:

  double computeEtSum(const StjTowerEnergyList& energyList);

  bool isNewEvent();
  void read();

  StjBEMC* _bemc;
  StjTrgBEMCJetPatchTowerIdMap* _jpTowerMap;
  double _minEt;

  int _runNumber;
  int _eventId;

  bool                      _passed;
  std::vector<int>          _jetPatches;
  std::vector<int>          _jetPatchDsmAdc;
  std::vector<unsigned int> _jetPatchAdc;
  std::vector<double>       _jetPatchEnergy;
  std::vector<double>       _jetPatchEt;

  StjTowerEnergyListCut _cut;

  ClassDef(StjTrgSoftEtThresholdBJP, 1)

};

#endif // STJTRGSOFTETTHRESHOLDBJP_H
