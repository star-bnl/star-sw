// -*- mode: c++;-*-
// $Id: StjTPCTree.h,v 1.1 2008/08/02 04:15:44 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTREE_H
#define STJETTPCTREE_H

#include "StjTPC.h"
#include <Rtypes.h>

class TTree;

class StJetTrackListReader;

namespace StSpinJet {

class StJetTPCTree : public StJetTPC {

public:
  StJetTPCTree(TTree *tree,
	       const Int_t& indexMajor, const Int_t& indexMinor,
	       const char* indexMajorName = "runNumber",
	       const char* indexMinorName = "eventId"
	       );
  virtual ~StJetTPCTree() { }

  TrackList getTrackList();

private:

  TTree* _tree;

  const Int_t& _indexMajor;
  const Int_t& _indexMinor;

  StJetTrackListReader* _reader;

};

}

#endif // STJETTPCTREE_H
