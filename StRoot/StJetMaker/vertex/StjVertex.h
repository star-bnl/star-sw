// -*- mode: c++;-*-
// $Id: StjVertex.h,v 1.1 2008/08/13 19:37:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJVERTEX_H
#define STJVERTEX_H

#include <TObject.h>

class StjVertex : public TObject {

public:
  StjVertex() { }
  virtual ~StjVertex() { }

  virtual int runNumber() = 0;
  virtual int eventId() = 0;
  virtual double vertexZ() = 0;
  virtual double vertexY() = 0;
  virtual double vertexX() = 0;

private:

  ClassDef(StjVertex, 1)

};

#endif // STJVERTEX_H
