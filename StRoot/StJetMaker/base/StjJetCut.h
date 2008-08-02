// -*- mode: c++;-*-
// $Id: StjJetCut.h,v 1.1 2008/08/02 04:15:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUT_H
#define JETCUT_H

#include "StjJetList.h"

namespace StJetJetCut {

class JetCut {

public:
  JetCut() { }
  virtual ~JetCut() { }

  virtual bool operator()(const StSpinJet::Jet& p4) = 0;

private:

};

}

#endif // JETCUT_H
