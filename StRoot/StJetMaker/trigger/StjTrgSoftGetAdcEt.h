// -*- mode: c++;-*-
// $Id: StjTrgSoftGetAdcEt.h,v 1.2 2008/08/18 08:50:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGMUDSTSOFTWAREGETADCET_H
#define STJTRGMUDSTSOFTWAREGETADCET_H

#include <TObject.h>

#include <StjTowerEnergyList.h>

#include <StjBEMC.h>

#include <StjTowerEnergyListCut.h>

#include <vector>

class StjTrg;

class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgSoftGetAdcEt : public TObject {

public:
  StjTrgSoftGetAdcEt(StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap);
  virtual ~StjTrgSoftGetAdcEt() { }

  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();
  std::vector<double> jetPatchEt();

  void setTrg(StjTrg* trg) { _trg = trg; }

private:

  StjBEMC* _bemc;

  StjTrgBEMCJetPatchTowerIdMap* _bemcJpTowerMap;

  StjTrg* _trg;

  std::vector<unsigned int> _towerAdc;
  std::vector<double> _towerEnergy;
  std::vector<double> _towerEt;

  std::vector<unsigned int> _jetPatchAdc;
  std::vector<double> _jetPatchEnergy;
  std::vector<double> _jetPatchEt;

  int _runNumber;
  int _eventId;

  bool isNewEvent();
  void read();

  StjTowerEnergyListCut _cut;

  ClassDef(StjTrgSoftGetAdcEt, 1)

};

#endif // STJTRGMUDSTSOFTWAREGETADCET_H
