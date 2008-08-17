// -*- mode: c++;-*-
// $Id: StjTrg.h,v 1.6 2008/08/17 11:29:22 tai Exp $
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
  virtual bool pass() = 0;
  virtual double prescale() = 0;
  virtual double vertexZ() = 0;

  virtual std::vector<int> towers() = 0;
  virtual std::vector<int> towerDsmAdc() = 0;
  virtual std::vector<unsigned int> towerAdc() = 0;
  virtual std::vector<double> towerEnergy() = 0;

  virtual std::vector<int> jetPatches() = 0;
  virtual std::vector<int> jetPatchDsmAdc() = 0;
  virtual std::vector<unsigned int> jetPatchAdc() = 0;
  virtual std::vector<double> jetPatchEnergy() = 0;

private:

  ClassDef(StjTrg, 1)

};

#endif // STJTRG_H
