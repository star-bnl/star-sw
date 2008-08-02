// -*- mode: c++;-*-
// $Id: StjJetCut.h,v 1.3 2008/08/02 22:43:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUT_H
#define STJJETCUT_H

#include "StjJetList.h"

namespace StJetJetCut {

class StjJetCut {

public:
  StjJetCut() { }
  virtual ~StjJetCut() { }

  virtual bool operator()(const StSpinJet::StjJet& p4) = 0;

private:

};

}

#endif // STJJETCUT_H
