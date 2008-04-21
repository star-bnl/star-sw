// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.4 2008/04/21 00:24:57 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

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

  void addAnalyzer(StppJetAnalyzer* analyzer, StJets *stJets, const char* name);

  TTree* jetTree() const { return _jetTree; }

  void fillJetTree();
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, StppJetAnalyzer* analyzer);
  void fillJet(StJets &jets, StProtoJet& pj);


private:

  struct AnalyzerCtl {
    std::string mBranchName;
    StppJetAnalyzer* mAnalyzer;
    StJets *mJets;
  };

  StMuDstMaker& _uDstMaker;
  std::string _OutFileName;
  TTree *_jetTree;
  TFile *_outFile;

  std::vector<AnalyzerCtl> _analyzerCtlList;
};

}

#endif // STJETTREEWRITER_H

