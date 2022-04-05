// -*- mode: c++;-*-
// $Id: StjMCKin.h,v 1.1 2008/08/22 22:10:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCKIN_H
#define STJMCKIN_H

#include <TObject.h>

class StjMCKin : public TObject {

public:
  StjMCKin() { }
  virtual ~StjMCKin() { }

  virtual int runNumber() = 0;
  virtual int eventId() = 0;
  virtual double vertexZ() = 0;
  virtual double s() = 0;
  virtual double t() = 0;
  virtual double u() = 0;
  virtual double pt() = 0;
  virtual double costh() = 0;
  virtual double x1() = 0;
  virtual double x2() = 0;
  virtual int pid() = 0;

private:

  ClassDef(StjMCKin, 1)

};

#endif // STJMCKIN_H
