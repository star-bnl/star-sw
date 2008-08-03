// -*- mode: c++;-*-
// $Id: StjFourVecCut.h,v 1.4 2008/08/03 00:26:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUT_H
#define STJFOURVECCUT_H

#include "StjFourVecList.h"

class StjFourVecCut {

public:
  StjFourVecCut() { }
  virtual ~StjFourVecCut() { }

  virtual bool operator()(const StjFourVec& p4) = 0;

private:

};

#endif // STJFOURVECCUT_H
