// -*- mode: c++;-*-
// $Id: StjTrgRaiseThresholdEtJP.h,v 1.3 2008/09/20 01:02:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGRAISETHRESHOLDETJP_H
#define STJTRGRAISETHRESHOLDETJP_H

#include "StjTrgRaiseThreshold.h"

class StjTrgRaiseThresholdEtJP : public StjTrgRaiseThreshold {

public:
  StjTrgRaiseThresholdEtJP(StjTrg* src, double minEt)
    : StjTrgRaiseThreshold(src), _minEt(minEt) { }
  virtual ~StjTrgRaiseThresholdEtJP() { }

  bool soft() const;

  std::vector<int>          jetPatches();
  std::vector<int>          jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double>       jetPatchEnergy();
  std::vector<double>       jetPatchEt();     

private:

  void read() const;

  double _minEt;

  mutable bool                      _passed;
  mutable std::vector<int>          _jetPatches;
  mutable std::vector<int>          _jetPatchDsmAdc;
  mutable std::vector<unsigned int> _jetPatchAdc;
  mutable std::vector<double>       _jetPatchEnergy;
  mutable std::vector<double>       _jetPatchEt;

  ClassDef(StjTrgRaiseThresholdEtJP, 1)

};

#endif // STJTRGRAISETHRESHOLDETJP_H
