// -*- mode: c++;-*-
// $Id: StjTrg.h,v 1.9 2008/09/21 19:11:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRG_H
#define STJTRG_H

#include <TObject.h>

#include <vector>

class StjTrg : public TObject {

public:
  StjTrg() { }
  virtual ~StjTrg() { }

  virtual int id() = 0;

  virtual int runNumber() = 0;
  virtual int eventId() = 0;
  virtual bool hard() const = 0;
  virtual bool soft() const = 0;
  virtual bool passed() const = 0;
  virtual double prescale() = 0;
  virtual double vertexZ() = 0;

  virtual std::vector<int> towers() = 0;
  virtual std::vector<int> towerDsmAdc() = 0;
  virtual std::vector<unsigned int> towerAdc() = 0;
  virtual std::vector<double> towerEnergy() = 0;
  virtual std::vector<double> towerEt() = 0;

  virtual std::vector<int> jetPatches() = 0;
  virtual std::vector<int> jetPatchDsmAdc() = 0;
  virtual std::vector<unsigned int> jetPatchAdc() = 0;
  virtual std::vector<double> jetPatchEnergy() = 0;
  virtual std::vector<double> jetPatchEt() = 0;

private:

  ClassDef(StjTrg, 1)

};

#endif // STJTRG_H
