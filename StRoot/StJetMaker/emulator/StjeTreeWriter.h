// -*- mode: c++;-*-
// $Id: StjeTreeWriter.h,v 1.2 2010/04/24 04:15:35 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEWRITER_H
#define STJTREEWRITER_H

#include <TObject.h>

class StJets;
class StProtoJet;
class StFourPMaker;
class AbstractFourVec;

class TTree;

#include <list>
#include <vector>

class StjeTreeWriter : public TObject {

public:

  StjeTreeWriter() { }
  virtual ~StjeTreeWriter() { }

  virtual void Init() { }
  virtual void Finish() { }

  virtual void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name, StJets* stjets = 0) = 0;

  virtual void fillJetTreeHeader(int iAnalyzer) = 0;
  virtual void fillJetTree(int iAnalyzer, int iVertex) = 0;
  virtual TTree* jetTree() const = 0;
};

#endif // STJTREEWRITER_H

