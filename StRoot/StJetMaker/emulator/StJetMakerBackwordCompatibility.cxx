// $Id: StJetMakerBackwordCompatibility.cxx,v 1.1 2008/07/21 02:00:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetMakerBackwordCompatibility.h"

#include "StDefaultJetTreeWriter.h"

namespace StSpinJet {

void StJetMakerBackwordCompatibility::addAnalyzer(StppJetAnalyzer* analyzer, StJetTreeWriter* treeWriter, const char* name)
{
  _jetBranches[name] = analyzer;

  StDefaultJetTreeWriter* defaultWriter = dynamic_cast<StDefaultJetTreeWriter*>(treeWriter);
  if (!defaultWriter) return;

  analyzer->setmuDstJets(defaultWriter->getLastStJets());
}


}
