// -*- mode: c++;-*-
// $Id: FourCut.h,v 1.1 2008/07/21 17:24:40 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURCUT_H
#define FOURCUT_H

#include "TLorentzVectorWithId.h"

namespace StJetFourCut {

class FourCut {

public:
  FourCut() { }
  virtual ~FourCut() { }

  virtual bool operator()(const TLorentzVectorWithId& p4) = 0;

private:

};

}

#endif // FOURCUT_H
