// -*- mode: c++;-*-
// $Id: StjSpinTree.h,v 1.1 2008/11/05 05:48:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJSPINTREE_H
#define STJSPINTREE_H

#include <StjSpin.h>

class StjSpinReader;

class StjSpinTree : public StjSpin {

public:
  StjSpinTree(StjSpinReader* reader)
    : _reader(reader) { }
  virtual ~StjSpinTree() { }

private:

  int runNumber();
  int eventId();
  int bx7();
  int bx48();
  int spin4();
  int bbcTimebin();
  double vertexZ();

  StjSpinReader* _reader;

  ClassDef(StjSpinTree, 1)

};

#endif // STJSPINTREE_H
