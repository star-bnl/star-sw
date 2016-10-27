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
class StJetVertex;
class StJetCandidate;
class StMuPrimaryVertex;

class TTree;
class TFile;

#include <string>
#include <vector>
#include <list>

using namespace std;

class StjeJetEventTreeWriter : public StjeTreeWriter {

public:

  StjeJetEventTreeWriter(const char* outFileName);
  virtual ~StjeJetEventTreeWriter() {}

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const vector<const AbstractFourVec*>* particleList, list<StProtoJet>* protoJetList, const char* name, StJets* = 0);

  TTree* jetTree() const { return _jetTree; }

  void fillJetTreeHeader(int iAnalyzer);
  void fillJetTree(int iAnalyzer, int iVertex);

private:

  void fillJetTreeForOneVertex(StJetEvent* jetEvent, list<StProtoJet>* protoJetList, StFourPMaker* fourPMaker, int iVertex);
  StJetCandidate* fillJet(StJetEvent* jetEvent, StJetVertex* jetVertex, StProtoJet& protojet);
  void copyVertex(const StMuPrimaryVertex* muVertex, StJetVertex* jetVertex);

  struct AnalyzerCtl {
    string            _branchName;
    StFourPMaker*     _fourPMaker;
    list<StProtoJet>* _protoJetList;
    StJetEvent*       _jetEvent;
  };

  vector<AnalyzerCtl> _analyzerCtlList;

  TString       _OutFileName;
  TTree*        _jetTree;
  TFile*        _outFile;

  ClassDef(StjeJetEventTreeWriter, 1);
};

#endif // ST_JE_JET_EVENT_TREE_WRITER_H
