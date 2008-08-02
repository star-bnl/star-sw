// -*- mode: c++;-*-
// $Id: StJetMakerBackwordCompatibility.h,v 1.3 2008/08/02 23:10:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMAKERBACKWORDCOMPATIBILITY_H
#define STJETMAKERBACKWORDCOMPATIBILITY_H

#include "StppJetAnalyzer.h"

#include <string>
#include <map>

class StjeTreeWriter;

namespace StSpinJet {

class StJetMakerBackwordCompatibility {

public:

  StJetMakerBackwordCompatibility() { }
  virtual ~StJetMakerBackwordCompatibility() { }

  typedef std::map<std::string, StppJetAnalyzer*> jetBranchesMap;

  void addAnalyzer(StppJetAnalyzer* analyzer, StjeTreeWriter* treeWriter, const char* name);

  jetBranchesMap& getJets() { return _jetBranches; }

private:

  jetBranchesMap  _jetBranches;

};

}

#endif // STJETMAKERBACKWORDCOMPATIBILITY_H


