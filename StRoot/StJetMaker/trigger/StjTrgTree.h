// -*- mode: c++;-*-
// $Id: StjTrgTree.h,v 1.2 2008/08/11 06:07:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGTREE_H
#define STJTRGTREE_H

#include "StjTrg.h"

class StjTrgReader;

class StjTrgTree : public StjTrg {

public:
  StjTrgTree(StjTrgReader* reader)
    : _reader(reader) { }
  virtual ~StjTrgTree() { }

  int id();

  int runNumber();
  int eventId();
  bool hard() const;
  bool soft() const;
  bool pass();
  double prescale();
  double vertexZ();
  std::vector<int> towers();
  std::vector<int> jetPatches();

private:

  StjTrgReader* _reader;

  ClassDef(StjTrgTree, 1)

};

#endif // STJTRGTREE_H
