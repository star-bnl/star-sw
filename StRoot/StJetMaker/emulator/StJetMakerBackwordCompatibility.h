// -*- mode: c++;-*-
// $Id: StJetMakerBackwordCompatibility.h,v 1.4 2008/08/03 00:26:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMAKERBACKWORDCOMPATIBILITY_H
#define STJETMAKERBACKWORDCOMPATIBILITY_H

#include "StppJetAnalyzer.h"

#include <string>
#include <map>

class StjeTreeWriter;

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

#endif // STJETMAKERBACKWORDCOMPATIBILITY_H


