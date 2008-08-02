// -*- mode: c++;-*-
// $Id: StjTPCTree.h,v 1.2 2008/08/02 19:22:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTREE_H
#define STJETTPCTREE_H

#include "StjTPC.h"
#include <Rtypes.h>

class TTree;

class StjTrackListReader;

namespace StSpinJet {

class StjTPCTree : public StjTPC {

public:
  StjTPCTree(TTree *tree,
	       const Int_t& indexMajor, const Int_t& indexMinor,
	       const char* indexMajorName = "runNumber",
	       const char* indexMinorName = "eventId"
	       );
  virtual ~StjTPCTree() { }

  StjTrackList getTrackList();

private:

  TTree* _tree;

  const Int_t& _indexMajor;
  const Int_t& _indexMinor;

  StjTrackListReader* _reader;

};

}

#endif // STJETTPCTREE_H
