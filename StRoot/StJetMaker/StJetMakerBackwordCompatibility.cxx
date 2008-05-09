// $Id: StJetMakerBackwordCompatibility.cxx,v 1.2 2008/05/09 02:14:52 tai Exp $
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
