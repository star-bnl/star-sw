// -*- mode: c++;-*-
// $Id: StJetTreeWriter.h,v 1.8 2008/05/02 21:47:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STJETTREEWRITER_H
#define STJETTREEWRITER_H

class StProtoJet;
class StFourPMaker;

class TTree;

#include <list>

namespace StSpinJet {

class StJetTreeWriter {

public:

  StJetTreeWriter() { }
  virtual ~StJetTreeWriter() { }

  virtual void Init() { }
  virtual void Finish() { }

  virtual void addJetFinder(StFourPMaker* fourPMaker, std::list<StProtoJet>* protoJetList, const char* name) = 0;

  virtual void fillJetTree() = 0;

  virtual TTree* jetTree() const = 0;

private:

};

}

#endif // STJETTREEWRITER_H

