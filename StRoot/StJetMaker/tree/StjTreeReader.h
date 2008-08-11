// -*- mode: c++;-*-
// $Id: StjTreeReader.h,v 1.1 2008/08/11 04:48:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEREADER_H
#define STJTREEREADER_H

#include <TObject.h>

class TTree;

class StjTreeIndex;

class StjTreeReader : public TObject {

public:
  StjTreeReader(TTree* tree) : _tree(tree) { }
  virtual ~StjTreeReader() { }

  void Init();

  Long64_t GetEntryWithIndex(const StjTreeIndex& idx);
  Long64_t GetEntryWithIndex(Int_t major, Int_t minor);

private:

  virtual void SetBranchAddress(TTree *tree) = 0;

  virtual void clearEntry() = 0;
  virtual void readEntry() = 0;

  TTree* _tree;

  ClassDef(StjTreeReader, 1)

};

#endif // STJTREEREADER_H
