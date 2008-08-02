// -*- mode: c++;-*-
// $Id: StjFourVecCut.h,v 1.1 2008/08/02 04:15:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURVECCUT_H
#define FOURVECCUT_H

#include "StjFourVecList.h"

namespace StJetFourVecCut {

class FourVecCut {

public:
  FourVecCut() { }
  virtual ~FourVecCut() { }

  virtual bool operator()(const StSpinJet::FourVec& p4) = 0;

private:

};

}

#endif // FOURVECCUT_H
