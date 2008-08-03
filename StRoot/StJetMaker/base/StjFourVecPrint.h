// -*- mode: c++;-*-
// $Id: StjFourVecPrint.h,v 1.4 2008/08/03 00:26:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECPRINT_H
#define STJFOURVECPRINT_H

#include "StjFourVecList.h"

#include <fstream>
#include <string>

class StjFourVecPrint {

public:

  StjFourVecPrint() { }
  virtual ~StjFourVecPrint() { }

  void operator()(const StjFourVecList& fourList);

private:

  void print(const StjFourVec& four);

};

#endif // STJFOURVECPRINT_H
