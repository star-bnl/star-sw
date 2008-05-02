// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.7 2008/05/02 19:02:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

class StMuDstMaker;
class StJets;
class StProtoJet;
class StFourPMaker;

class TTree;
class TFile;

#include <string>
#include <vector>
#include <list>

namespace StSpinJet {

class StJetTreeWriter {

public:
  StJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName);
  virtual ~StJetTreeWriter();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, std::list<StProtoJet>* protoJetList, const char* name);

  TTree* jetTree() const { return _jetTree; }

  void fillJetTree();
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, std::list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker);
  void fillJet(StJets &jets, StProtoJet& pj);

  StJets *getLastStJets() { return _analyzerCtlList[_analyzerCtlList.size()]._jets; }

private:
  
  struct AnalyzerCtl {
    std::string _branchName;
    StFourPMaker* _fourPMaker;
    std::list<StProtoJet>* _protoJetList;
    StJets *_jets;
  };

  StMuDstMaker& _uDstMaker;
  std::string _OutFileName;
  TTree *_jetTree;
  TFile *_outFile;

  std::vector<AnalyzerCtl> _analyzerCtlList;
};

}

#endif // STJETTREEWRITER_H

