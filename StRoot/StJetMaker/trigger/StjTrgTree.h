// -*- mode: c++;-*-
// $Id: StjTrgTree.h,v 1.4 2008/08/18 06:20:53 tai Exp $
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
  std::vector<unsigned int> towerAdc();
  std::vector<double> towerEnergy();
  std::vector<double> towerEt();

  std::vector<int> jetPatches();
  std::vector<int> jetPatchDsmAdc();
  std::vector<unsigned int> jetPatchAdc();
  std::vector<double> jetPatchEnergy();
  std::vector<double> jetPatchEt();

private:

  StjTrgReader* _reader;

  ClassDef(StjTrgTree, 1)

};

#endif // STJTRGTREE_H
