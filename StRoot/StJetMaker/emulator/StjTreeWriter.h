// -*- mode: c++;-*-
// $Id: StjTreeWriter.h,v 1.2 2008/08/02 19:23:08 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

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

#endif // STJETTREEWRITER_H

