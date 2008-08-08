// -*- mode: c++;-*-
// $Id: StjTrg.h,v 1.4 2008/08/08 21:17:10 tai Exp $
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
  virtual bool hard() = 0;
  virtual bool soft() = 0;
  virtual double prescale() = 0;
  virtual double vertexZ() = 0;
  virtual std::vector<int> towers() = 0;
  virtual std::vector<int> jetPatches() = 0;

private:

  ClassDef(StjTrg, 1)

};

#endif // STJTRG_H
