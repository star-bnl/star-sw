// $Id: StJetMakerBackwordCompatibility.cxx,v 1.2 2008/08/02 19:23:06 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetMakerBackwordCompatibility.h"

#include "StDefaultJetTreeWriter.h"

namespace StSpinJet {

void StJetMakerBackwordCompatibility::addAnalyzer(StppJetAnalyzer* analyzer, StjTreeWriter* treeWriter, const char* name)
{
  _jetBranches[name] = analyzer;

  StDefaultJetTreeWriter* defaultWriter = dynamic_cast<StDefaultJetTreeWriter*>(treeWriter);
  if (!defaultWriter) return;

  analyzer->setmuDstJets(defaultWriter->getLastStJets());
}


}
