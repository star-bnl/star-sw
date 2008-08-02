// -*- mode: c++;-*-
// $Id: StjTreeWriter.h,v 1.3 2008/08/02 22:43:32 tai Exp $
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

class StjTreeWriter : public TObject {

public:

  StjTreeWriter() { }
  virtual ~StjTreeWriter() { }

  virtual void Init() { }
  virtual void Finish() { }

  virtual void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name, StJets* stjets = 0) { }

  virtual void fillJetTree() = 0;

  virtual TTree* jetTree() const = 0;

private:

};

#endif // STJTREEWRITER_H

