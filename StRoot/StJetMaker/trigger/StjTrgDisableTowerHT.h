// -*- mode: c++;-*-
// $Id: StjTrgDisableTowerHT.h,v 1.1 2008/09/21 19:11:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGDISABLETOWERHT_H
#define STJTRGDISABLETOWERHT_H

#include "StjTrgDisableTower.h"

class StjTrgDisableTowerHT : public StjTrgDisableTower {

public:
  StjTrgDisableTowerHT(StjTrg* src, int badTowerId)
    : StjTrgDisableTower(src, badTowerId) { }
  virtual ~StjTrgDisableTowerHT() { }

  bool soft() const;

  std::vector<int> towers();
  std::vector<int> towerDsmAdc();
  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

private:

  void read() const;

  mutable bool                      _passed;
  mutable std::vector<int>          _towers;
  mutable std::vector<int>          _towerDsmAdc;
  mutable std::vector<unsigned int> _towerAdc;
  mutable std::vector<double>       _towerEnergy;
  mutable std::vector<double>       _towerEt;

  ClassDef(StjTrgDisableTowerHT, 1)

};

#endif // STJTRGDISABLETOWERHT_H
