// -*- mode: c++;-*-
// $Id: StjFourVecPrint.h,v 1.1 2008/08/02 04:15:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECPRINT_H
#define STJETFOURVECPRINT_H

#include "StjFourVecList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetFourVecPrint {

public:

  StJetFourVecPrint() { }
  virtual ~StJetFourVecPrint() { }

  void operator()(const FourVecList& fourList);

private:

  void print(const FourVec& four);

};

}

#endif // STJETFOURVECPRINT_H
