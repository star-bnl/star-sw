// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.1 2008/04/20 20:57:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

#include "AnalyzerCtl.h"

class StMuDstMaker;
class StJets;
class StProtoJet;
class StppJetAnalyzer;

class TTree;

#include <vector>

namespace StSpinJet {

class StJetTreeWriter {

public:
  StJetTreeWriter(StMuDstMaker& uDstMaker);
  virtual ~StJetTreeWriter();

  void fillJetTree(std::vector<AnalyzerCtl> &mAnalyzerCtl, TTree *mJetTree);
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, StppJetAnalyzer* analyzer);
  void fillJet(StJets &jets, StProtoJet& pj);

private:

  StMuDstMaker& _uDstMaker;

};


}

#endif // STJETTREEWRITER_H

