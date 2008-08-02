// -*- mode: c++;-*-
// $Id: StjFourVecPrint.h,v 1.2 2008/08/02 19:22:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECPRINT_H
#define STJETFOURVECPRINT_H

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

#endif // STJETFOURVECPRINT_H
