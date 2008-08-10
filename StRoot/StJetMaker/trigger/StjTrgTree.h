// -*- mode: c++;-*-
// $Id: StjTrgTree.h,v 1.1 2008/08/10 23:04:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRGTREE_H
#define STJTRGTREE_H

#include "StjTrg.h"

class TTree;

class StjTrgTree : public StjTrg {

public:
  StjTrgTree(TTree *tree,
	     const Int_t& indexMajor, const Int_t& indexMinor,
	     const char* indexMajorName = "runNumber",
	     const char* indexMinorName = "eventId"
	     );
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

  TTree* _tree;

  const Int_t& _indexMajor;
  const Int_t& _indexMinor;

  ClassDef(StjTrgTree, 1)

};

#endif // STJTRGTREE_H
