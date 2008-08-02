// -*- mode: c++;-*-
// $Id: StjFourVecCut.h,v 1.2 2008/08/02 19:22:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURVECCUT_H
#define FOURVECCUT_H

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

#endif // FOURVECCUT_H
