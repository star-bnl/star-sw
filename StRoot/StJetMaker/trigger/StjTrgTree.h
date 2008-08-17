// -*- mode: c++;-*-
// $Id: StjTrgTree.h,v 1.3 2008/08/17 11:29:22 tai Exp $
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
  std::vector<int> towerDsmAdc();

  std::vector<int> jetPatches();
  std::vector<int> jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();

private:

  StjTrgReader* _reader;

  ClassDef(StjTrgTree, 1)

};

#endif // STJTRGTREE_H
