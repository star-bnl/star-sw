// -*- mode: c++;-*-
// $Id: StJetMakerBackwordCompatibility.h,v 1.2 2008/08/02 19:23:06 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMAKERBACKWORDCOMPATIBILITY_H
#define STJETMAKERBACKWORDCOMPATIBILITY_H

#include "StppJetAnalyzer.h"

#include <string>
#include <map>

class StjTreeWriter;

namespace StSpinJet {

class StJetMakerBackwordCompatibility {

public:

  StJetMakerBackwordCompatibility() { }
  virtual ~StJetMakerBackwordCompatibility() { }

  typedef std::map<std::string, StppJetAnalyzer*> jetBranchesMap;

  void addAnalyzer(StppJetAnalyzer* analyzer, StjTreeWriter* treeWriter, const char* name);

  jetBranchesMap& getJets() { return _jetBranches; }

private:

  jetBranchesMap  _jetBranches;

};

}

#endif // STJETMAKERBACKWORDCOMPATIBILITY_H


