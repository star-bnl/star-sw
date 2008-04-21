// -*- mode: c++;-*-
// $Id: StJetMakerBackwordCompatibility.h,v 1.1 2008/04/21 01:53:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETMAKERBACKWORDCOMPATIBILITY_H
#define STJETMAKERBACKWORDCOMPATIBILITY_H

#include "StppJetAnalyzer.h"

#include <string>
#include <map>

namespace StSpinJet {

class StJetMakerBackwordCompatibility {

public:

  StJetMakerBackwordCompatibility() { }
  virtual ~StJetMakerBackwordCompatibility() { }

  typedef std::map<std::string, StppJetAnalyzer*> jetBranchesMap;

  void addAnalyzer(StppJetAnalyzer* analyzer, StJets *stJets, const char* name)
  {
    _jetBranches[name] = analyzer;
    analyzer->setmuDstJets(stJets);
  }

  jetBranchesMap& getJets() const { return _jetBranches; }

private:

  jetBranchesMap  _jetBranches;

};

}

#endif // STJETMAKERBACKWORDCOMPATIBILITY_H


