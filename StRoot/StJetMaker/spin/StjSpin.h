// -*- mode: c++;-*-
// $Id: StjSpin.h,v 1.1 2008/11/05 05:48:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJSPIN_H
#define STJSPIN_H

#include <TObject.h>

class StjSpin : public TObject {

public:
  StjSpin() { }
  virtual ~StjSpin() { }

  virtual int runNumber() = 0;
  virtual int eventId() = 0;
  virtual int bx7() = 0;
  virtual int bx48() = 0;
  virtual int spin4() = 0;
  virtual int bbcTimebin() = 0;
  virtual double vertexZ() = 0;

private:

  ClassDef(StjSpin, 1)

};

#endif // STJSPIN_H
