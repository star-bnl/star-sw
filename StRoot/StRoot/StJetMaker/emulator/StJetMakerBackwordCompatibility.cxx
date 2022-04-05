// $Id: StJetMakerBackwordCompatibility.cxx,v 1.6 2008/08/03 00:26:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetMakerBackwordCompatibility.h"

#include "StjeDefaultJetTreeWriter.h"

void StJetMakerBackwordCompatibility::addAnalyzer(StppJetAnalyzer* analyzer, StjeTreeWriter* treeWriter, const char* name)
{
  _jetBranches[name] = analyzer;

  StjeDefaultJetTreeWriter* defaultWriter = dynamic_cast<StjeDefaultJetTreeWriter*>(treeWriter);
  if (!defaultWriter) return;

  analyzer->setmuDstJets(defaultWriter->getLastStJets());
}
