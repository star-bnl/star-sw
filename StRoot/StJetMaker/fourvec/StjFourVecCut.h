// -*- mode: c++;-*-
// $Id: StjFourVecCut.h,v 1.1 2008/11/27 07:29:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUT_H
#define STJFOURVECCUT_H

#include <TObject.h>

#include "StjFourVecList.h"

class StjFourVecCut : public TObject {

public:
  StjFourVecCut() { }
  virtual ~StjFourVecCut() { }

  virtual bool operator()(const StjFourVec& p4) = 0;

private:

  ClassDef(StjFourVecCut, 1)

};

#endif // STJFOURVECCUT_H
