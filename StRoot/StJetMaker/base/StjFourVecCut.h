// -*- mode: c++;-*-
// $Id: StjFourVecCut.h,v 1.3 2008/08/02 22:43:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUT_H
#define STJFOURVECCUT_H

#include "StjFourVecList.h"

namespace StJetFourVecCut {

class StjFourVecCut {

public:
  StjFourVecCut() { }
  virtual ~StjFourVecCut() { }

  virtual bool operator()(const StSpinJet::StjFourVec& p4) = 0;

private:

};

}

#endif // STJFOURVECCUT_H
