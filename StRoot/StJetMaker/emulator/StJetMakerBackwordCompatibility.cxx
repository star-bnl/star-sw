// $Id: StJetMakerBackwordCompatibility.cxx,v 1.4 2008/08/02 21:37:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetMakerBackwordCompatibility.h"

#include "StjeDefaultJetTreeWriter.h"

namespace StSpinJet {

void StJetMakerBackwordCompatibility::addAnalyzer(StppJetAnalyzer* analyzer, StjTreeWriter* treeWriter, const char* name)
{
  _jetBranches[name] = analyzer;

  StjeDefaultJetTreeWriter* defaultWriter = dynamic_cast<StjeDefaultJetTreeWriter*>(treeWriter);
  if (!defaultWriter) return;

  analyzer->setmuDstJets(defaultWriter->getLastStJets());
}


}
