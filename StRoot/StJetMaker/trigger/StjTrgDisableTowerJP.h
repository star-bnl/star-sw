// -*- mode: c++;-*-
// $Id: StjTrgDisableTowerJP.h,v 1.2 2009/04/28 02:37:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGDISABLETOWERJP_H
#define STJTRGDISABLETOWERJP_H

#include "StjTrgDisableTower.h"

class StjBEMC;
class StjTrgBEMCJetPatchTowerIdMap;

class StjTrgDisableTowerJP : public StjTrgDisableTower {

public:
  StjTrgDisableTowerJP(StjTrg* src, int badTowerId, StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* jpTowerMap)
    : StjTrgDisableTower(src, badTowerId)
    , _bemc(bemc), _jpTowerMap(jpTowerMap) { }
  StjTrgDisableTowerJP(StjTrg* src, int nbadTowerIds, int* badTowerIds, StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* jpTowerMap)
    : StjTrgDisableTower(src, nbadTowerIds, badTowerIds)
    , _bemc(bemc), _jpTowerMap(jpTowerMap) { }
  virtual ~StjTrgDisableTowerJP() { }

  bool soft() const;

  std::vector<int>          jetPatches();
  std::vector<int>          jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double>       jetPatchEnergy();
  std::vector<double>       jetPatchEt();     

private:

  StjBEMC* _bemc;
  StjTrgBEMCJetPatchTowerIdMap* _jpTowerMap;

  void read() const;

  mutable bool                      _passed;
  mutable std::vector<int>          _jetPatches;
  mutable std::vector<int>          _jetPatchDsmAdc;
  mutable std::vector<unsigned int> _jetPatchAdc;
  mutable std::vector<double>       _jetPatchEnergy;
  mutable std::vector<double>       _jetPatchEt;

  ClassDef(StjTrgDisableTowerJP, 1)

};

#endif // STJTRGDISABLETOWERJP_H
