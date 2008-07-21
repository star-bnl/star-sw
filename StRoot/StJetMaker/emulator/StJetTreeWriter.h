// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.1 2008/07/21 02:00:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

#include <TObject.h>

class StProtoJet;
class StFourPMaker;
class AbstractFourVec;

class TTree;

#include <list>
#include <vector>

class StJetTreeWriter : public TObject {

public:

  StJetTreeWriter() { }
  virtual ~StJetTreeWriter() { }

  virtual void Init() { }
  virtual void Finish() { }

  virtual void addJetFinder(StFourPMaker* fourPMaker, const std::vector<const AbstractFourVec*>* particleList, std::list<StProtoJet>* protoJetList, const char* name) = 0;

  virtual void fillJetTree() = 0;

  virtual TTree* jetTree() const = 0;

private:

};

#endif // STJETTREEWRITER_H

