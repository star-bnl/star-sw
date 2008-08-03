// -*- mode: c++;-*-
// $Id: StjJetCut.h,v 1.4 2008/08/03 00:26:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUT_H
#define STJJETCUT_H

#include "StjJetList.h"

class StjJetCut {

public:
  StjJetCut() { }
  virtual ~StjJetCut() { }

  virtual bool operator()(const StjJet& p4) = 0;

private:

};

#endif // STJJETCUT_H
