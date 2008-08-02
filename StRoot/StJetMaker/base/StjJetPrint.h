// -*- mode: c++;-*-
// $Id: StjJetPrint.h,v 1.1 2008/08/02 04:15:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETJETPRINT_H
#define STJETJETPRINT_H

#include "StjJetList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetJetPrint {

public:

  StJetJetPrint() { }
  virtual ~StJetJetPrint() { }

  void operator()(const JetList& jetList);

private:

  void print(const Jet& jet);

};

}

#endif // STJETJETPRINT_H
