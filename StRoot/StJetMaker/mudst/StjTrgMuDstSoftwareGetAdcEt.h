// -*- mode: c++;-*-
// $Id: StjTrgMuDstSoftwareGetAdcEt.h,v 1.1 2008/08/18 06:20:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGMUDSTSOFTWAREGETADCET_H
#define STJTRGMUDSTSOFTWAREGETADCET_H

#include <TObject.h>

#include <StjTowerEnergyList.h>

#include "StjBEMCMuDst.h"

#include "StjTowerEnergyListCut.h"

#include <vector>

class StjTrg;

class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgMuDstSoftwareGetAdcEt : public TObject {

public:
  StjTrgMuDstSoftwareGetAdcEt(StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap);
  virtual ~StjTrgMuDstSoftwareGetAdcEt() { }

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

  ClassDef(StjTrgMuDstSoftwareGetAdcEt, 1)

};

#endif // STJTRGMUDSTSOFTWAREGETADCET_H
