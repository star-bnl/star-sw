// -*- mode: c++;-*-
// $Id: StjTreeReader.h,v 1.3 2009/12/03 09:57:36 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEREADER_H
#define STJTREEREADER_H

#include <TObject.h>

class TTree;

class StjTreeIndex;

class StjTreeReader : public TObject {

public:
  StjTreeReader(TTree* tree) : _tree(tree) { }
  StjTreeReader() : _tree(0) { }
  virtual ~StjTreeReader() { }

  virtual void Init();

  virtual Long64_t GetEntryWithIndex(const StjTreeIndex& idx);
  virtual Long64_t GetEntryWithIndex(Int_t major, Int_t minor);

protected:

  virtual void SetBranchAddress(TTree *tree) { }

  virtual void clearEntry() = 0;
  virtual void readEntry() = 0;

  TTree* _tree;

  ClassDef(StjTreeReader, 1)

};

#endif // STJTREEREADER_H
