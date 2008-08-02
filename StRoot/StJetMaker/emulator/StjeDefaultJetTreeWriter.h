// -*- mode: c++;-*-
// $Id: StjeDefaultJetTreeWriter.h,v 1.3 2008/08/02 22:43:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEDEFAULTJETTREEWRITER_H
#define STJEDEFAULTJETTREEWRITER_H

#include "StjTreeWriter.h"

class StMuDstMaker;
class StJets;
class StProtoJet;
class StFourPMaker;

class TTree;
class TFile;

#include <string>
#include <vector>
#include <list>

class StjeDefaultJetTreeWriter : public StjTreeWriter {

public:
  StjeDefaultJetTreeWriter(StMuDstMaker& uDstMaker, std::string outFileName);
  virtual ~StjeDefaultJetTreeWriter();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name, StJets* stjets = 0);

  TTree* jetTree() const { return _jetTree; }

  void fillJetTree();
  StJets *getLastStJets() { return _analyzerCtlList[_analyzerCtlList.size() - 1]._jets; }

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

#endif // STJEDEFAULTJETTREEWRITER_H


