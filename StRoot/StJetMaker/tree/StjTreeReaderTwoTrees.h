// -*- mode: c++;-*-
// $Id: StjTreeReaderTwoTrees.h,v 1.2 2009/12/03 09:57:37 pibero Exp $
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

  virtual Long64_t GetEntryWithIndex(const StjTreeIndex& idx);
  virtual Long64_t GetEntryWithIndex(Int_t major, Int_t minor);

protected:

  virtual void SetBranchAddress(TTree *tree) { StjTreeReader::SetBranchAddress(tree); }
  virtual void SetBranchAddress(TTree *tree1, TTree *tree2) { }

  virtual void clearEntry() = 0;
  virtual void readEntry() = 0;

  TTree* _tree1;
  TTree* _tree2;

  ClassDef(StjTreeReaderTwoTrees, 1)

};

#endif // STJTREEREADERTWOTREES_H
