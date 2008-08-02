// $Id: StJetMakerBackwordCompatibility.cxx,v 1.5 2008/08/02 23:10:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetMakerBackwordCompatibility.h"

#include "StjeDefaultJetTreeWriter.h"

namespace StSpinJet {

void StJetMakerBackwordCompatibility::addAnalyzer(StppJetAnalyzer* analyzer, StjeTreeWriter* treeWriter, const char* name)
{
  _jetBranches[name] = analyzer;

  StjeDefaultJetTreeWriter* defaultWriter = dynamic_cast<StjeDefaultJetTreeWriter*>(treeWriter);
  if (!defaultWriter) return;

  analyzer->setmuDstJets(defaultWriter->getLastStJets());
}


}
