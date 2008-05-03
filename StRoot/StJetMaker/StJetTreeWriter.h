// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.9 2008/05/03 01:06:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

#include <TObject.h>

class StProtoJet;
class StFourPMaker;
class AbstractFourVec;

class TTree;

#include <list>
#include <vector>

namespace StSpinJet {

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

}

#endif // STJETTREEWRITER_H

