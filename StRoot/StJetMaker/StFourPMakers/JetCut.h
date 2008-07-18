// -*- mode: c++;-*-
// $Id: JetCut.h,v 1.1 2008/07/18 01:39:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUT_H
#define JETCUT_H

#include "JetList.h"

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
