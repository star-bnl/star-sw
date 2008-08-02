// -*- mode: c++;-*-
// $Id: StjJetCut.h,v 1.2 2008/08/02 19:22:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUT_H
#define JETCUT_H

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

#endif // JETCUT_H
