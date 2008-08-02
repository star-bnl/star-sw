// -*- mode: c++;-*-
// $Id: StjJetPrint.h,v 1.3 2008/08/02 22:43:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETPRINT_H
#define STJJETPRINT_H

#include "StjJetList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StjJetPrint {

public:

  StjJetPrint() { }
  virtual ~StjJetPrint() { }

  void operator()(const StjJetList& jetList);

private:

  void print(const StjJet& jet);

};

}

#endif // STJJETPRINT_H
