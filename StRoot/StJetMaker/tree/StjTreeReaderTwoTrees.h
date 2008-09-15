// -*- mode: c++;-*-
// $Id: StjTreeReaderTwoTrees.h,v 1.1 2008/09/15 05:50:08 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEREADERTWOTREES_H
#define STJTREEREADERTWOTREES_H

#include "StjTreeReader.h"

class StjTreeReaderTwoTrees : public StjTreeReader {

public:
  StjTreeReaderTwoTrees(TTree *tree1, TTree *tree2) 
    : _tree1(tree1), _tree2(tree2) { }
  virtual ~StjTreeReaderTwoTrees() { }

  virtual void Init();

  virtual Long64_t GetEntryWithIndex(Int_t major, Int_t minor);

private:

  virtual void SetBranchAddress(TTree *tree1, TTree *tree2) { }

  virtual void clearEntry() = 0;
  virtual void readEntry() = 0;

  TTree* _tree1;
  TTree* _tree2;

  ClassDef(StjTreeReaderTwoTrees, 1)

};

#endif // STJTREEREADERTWOTREES_H
