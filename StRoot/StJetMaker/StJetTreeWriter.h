// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.2 2008/04/20 21:38:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

#include "AnalyzerCtl.h"

class StMuDstMaker;
class StJets;
class StProtoJet;
class StppJetAnalyzer;

class TTree;
class TFile;

#include <string>
#include <vector>

namespace StSpinJet {

class StJetTreeWriter {

public:
  StJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName);
  virtual ~StJetTreeWriter();

  void Init();
  void Finish();

  void fillJetTree(std::vector<AnalyzerCtl> &mAnalyzerCtl, TTree *mJetTree);
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, StppJetAnalyzer* analyzer);
  void fillJet(StJets &jets, StProtoJet& pj);

private:

  StMuDstMaker& _uDstMaker;
  std::string _OutFileName;
  TFile *_outFile;

};


}

#endif // STJETTREEWRITER_H

