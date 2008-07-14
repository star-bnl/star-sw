// -*- mode: c++;-*-
// $Id: StJetTreeWriterScratch.h,v 1.1 2008/07/14 06:44:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEWRITERSCRATCH_H
#define STJETTREEWRITERSCRATCH_H

#include "StJetTreeWriter.h"

class StJets;
class StProtoJet;
class StFourPMaker;

class TTree;
class TFile;

#include <string>
#include <vector>
#include <list>

class StJetTreeWriterScratch : public StJetTreeWriter {

public:
  StJetTreeWriterScratch();
  virtual ~StJetTreeWriterScratch();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name);

  void fillJetTree();

  TTree* jetTree() const { return (TTree*)0; }

private:
  
  void fillJetTreeForOneJetFindingAlgorithm(StJets& jets, std::list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker);
  void fillJet(StJets &jets, StProtoJet& pj);

  struct AnalyzerCtl {
    std::string _branchName;
    StFourPMaker* _fourPMaker;
    std::list<StProtoJet>* _protoJetList;
    StJets *_jets;
  };

  std::vector<AnalyzerCtl> _analyzerCtlList;

  ClassDef(StJetTreeWriterScratch, 0)
};

#endif // STJETTREEWRITERSCRATCH_H


