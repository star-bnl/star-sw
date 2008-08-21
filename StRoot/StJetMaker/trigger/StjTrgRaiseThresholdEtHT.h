// -*- mode: c++;-*-
// $Id: StjTrgRaiseThresholdEtHT.h,v 1.2 2008/08/21 22:23:04 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGRAISETHRESHOLDETHT_H
#define STJTRGRAISETHRESHOLDETHT_H

#include "StjTrgRaiseThreshold.h"

class StjTrgRaiseThresholdEtHT : public StjTrgRaiseThreshold {

public:
  StjTrgRaiseThresholdEtHT(StjTrg* src, StjTrgPassCondition* passCondition, double minEt)
    : StjTrgRaiseThreshold(src, passCondition), _minEt(minEt) { }
  virtual ~StjTrgRaiseThresholdEtHT() { }

  bool soft() const;

  std::vector<int> towers();
  std::vector<int> towerDsmAdc();
  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

private:

  void read() const;

  double _minEt;

  mutable bool                      _passed;
  mutable std::vector<int>          _towers;
  mutable std::vector<int>          _towerDsmAdc;
  mutable std::vector<unsigned int> _towerAdc;
  mutable std::vector<double>       _towerEnergy;
  mutable std::vector<double>       _towerEt;

  ClassDef(StjTrgRaiseThresholdEtHT, 1)

};

#endif // STJTRGRAISETHRESHOLDETHT_H
