// -*- mode: c++;-*-
// $Id: StDefaultJetTreeWriter.h,v 1.4 2008/05/09 02:14:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STDEFAULTJETTREEWRITER_H
#define STDEFAULTJETTREEWRITER_H

#include "StJetTreeWriter.h"

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

class StDefaultJetTreeWriter : public StJetTreeWriter {

public:
  StDefaultJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName);
  virtual ~StDefaultJetTreeWriter();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name);

  TTree* jetTree() const { return _jetTree; }

  void fillJetTree();
  StJets *getLastStJets() { return _analyzerCtlList[_analyzerCtlList.size()]._jets; }

private:
  
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, std::list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker);
  void fillJet(StJets &jets, StProtoJet& pj);

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

#endif // STDEFAULTJETTREEWRITER_H


