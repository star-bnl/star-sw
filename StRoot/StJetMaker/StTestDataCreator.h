// -*- mode: c++;-*-
// $Id: StTestDataCreator.h,v 1.6 2008/07/14 06:44:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STTESTDATACREATOR_H
#define STTESTDATACREATOR_H

#include "StJetTreeWriter.h"

#include "StTestData.h"

class StProtoJet;
class StFourPMaker;

class TTree;
class TFile;

#include <string>
#include <list>

class StTestDataCreator : public StJetTreeWriter {

public:

  StTestDataCreator(std::string outFileName);
  virtual ~StTestDataCreator();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name);

  void fillJetTree();

  TTree* jetTree() const { return _particleTree; }

private:

  struct DataCtl {
    std::string _name;
    StFourPMaker* _fourPMaker;
    const std::vector<const AbstractFourVec*>* _particleList;
    std::list<StProtoJet>* _protoJetList;
  };

  std::string _OutFileName;
  TTree *_particleTree;
  TFile *_outFile;

  std::vector<DataCtl> _dataCtlList;

  TestParticle_t _particleListBranch;

  ClassDef(StTestDataCreator, 0)

};

#endif // STTESTDATACREATOR_H
