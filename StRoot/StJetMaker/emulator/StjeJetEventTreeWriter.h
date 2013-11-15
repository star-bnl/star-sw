// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 1 September 2009
//

#ifndef ST_JE_JET_EVENT_TREE_WRITER_H
#define ST_JE_JET_EVENT_TREE_WRITER_H

#include "StjeTreeWriter.h"

class StJets;
class StProtoJet;
class StFourPMaker;
class StJetEvent;

class TTree;
class TFile;

#include <string>
#include <vector>
#include <list>

using namespace std;

class StjeJetEventTreeWriter : public StjeTreeWriter {

public:
  StjeJetEventTreeWriter(const string& outFileName);
  virtual ~StjeJetEventTreeWriter() {}

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name, StJets* stjets = 0);

  TTree* jetTree() const { return _jetTree; }

  void fillJetTree();

private:
  
  void fillJetTreeForOneJetFindingAlgorithm(StJetEvent& jetEvent, list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker);
  void fillJet(StJetEvent& jetEvent, StProtoJet& pj);

  struct AnalyzerCtl {
    string            _branchName;
    StFourPMaker*     _fourPMaker;
    list<StProtoJet>* _protoJetList;
    StJetEvent*       _jetEvent;
  };

  vector<AnalyzerCtl> _analyzerCtlList;

  string        _OutFileName;
  TTree*        _jetTree;
  TFile*        _outFile;

  ClassDef(StjeJetEventTreeWriter, 1);
};

#endif // ST_JE_JET_EVENT_TREE_WRITER_H
