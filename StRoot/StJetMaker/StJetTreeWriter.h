// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.3 2008/04/20 23:34:26 tai Exp $
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

  TTree* jetTree() const { return _jetTree; }

  void fillJetTree();
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, StppJetAnalyzer* analyzer);
  void fillJet(StJets &jets, StProtoJet& pj);

  void push_back(AnalyzerCtl anaCtl);

private:

  StMuDstMaker& _uDstMaker;
  std::string _OutFileName;
  TTree *_jetTree;
  TFile *_outFile;
  std::vector<StSpinJet::AnalyzerCtl> _analyzerCtlList;

};

}

#endif // STJETTREEWRITER_H

