// -*- mode: c++;-*-
// $Id: StJetFourVecPrint.h,v 1.1 2008/07/18 19:20:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFOURVECPRINT_H
#define STJETFOURVECPRINT_H

#include "FourVecList.h"

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
