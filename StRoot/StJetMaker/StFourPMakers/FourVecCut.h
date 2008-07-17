// -*- mode: c++;-*-
// $Id: FourVecCut.h,v 1.1 2008/07/17 19:06:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURVECCUT_H
#define FOURVECCUT_H

#include "FourVecList.h"

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
