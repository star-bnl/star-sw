// -*- mode: c++;-*-
// $Id: StTestDataCreator.h,v 1.1 2008/05/02 22:13:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STTESTDATACREATOR_H
#define STTESTDATACREATOR_H

#include "StJetTreeWriter.h"

class StMuDstMaker;
class StJets;
class StProtoJet;
class StFourPMaker;

class TTree;
class TFile;

#include <string>
#include <list>

namespace StSpinJet {

class StTestDataCreator : public StJetTreeWriter {

public:

  StTestDataCreator(std::string outFileName);
  virtual ~StTestDataCreator();

  void Init();
  void Finish();

  void addJetFinder(StFourPMaker* fourPMaker, std::list<StProtoJet>* protoJetList, const char* name);

  void fillJetTree();

  TTree* jetTree() const { return _jetTree; }

private:

  std::string _OutFileName;
  TTree *_jetTree;
  TFile *_outFile;

};

}

#endif // STTESTDATACREATOR_H
