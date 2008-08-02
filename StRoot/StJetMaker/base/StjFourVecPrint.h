// -*- mode: c++;-*-
// $Id: StjFourVecPrint.h,v 1.3 2008/08/02 22:43:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECPRINT_H
#define STJFOURVECPRINT_H

#include "StjFourVecList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StjFourVecPrint {

public:

  StjFourVecPrint() { }
  virtual ~StjFourVecPrint() { }

  void operator()(const StjFourVecList& fourList);

private:

  void print(const StjFourVec& four);

};

}

#endif // STJFOURVECPRINT_H
