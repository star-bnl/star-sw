// -*- mode: c++;-*-
// $Id: StTestDataCreator.h,v 1.2 2008/05/03 01:06:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STTESTDATACREATOR_H
#define STTESTDATACREATOR_H

#include "StJetTreeWriter.h"

class StProtoJet;
class StFourPMaker;

class TTree;
class TFile;

#include <string>
#include <list>

class StTestDataCreator : public StSpinJet::StJetTreeWriter {

public:

  StTestDataCreator(std::string outFileName);
  virtual ~StTestDataCreator();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name);

  void fillJetTree();

  TTree* jetTree() const { return _jetTree; }

private:

  std::string _OutFileName;
  TTree *_jetTree;
  TFile *_outFile;

  ClassDef(StTestDataCreator, 0)

};

#endif // STTESTDATACREATOR_H
