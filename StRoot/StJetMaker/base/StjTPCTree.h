// -*- mode: c++;-*-
// $Id: StjTPCTree.h,v 1.6 2008/08/11 03:50:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPCTREE_H
#define STJTPCTREE_H

#include "StjTPC.h"

class StjTrackListReader;

class StjTPCTree : public StjTPC {

public:
  StjTPCTree(StjTrackListReader* reader)
    : _reader(reader) { }
  virtual ~StjTPCTree() { }

  StjTrackList getTrackList();

private:

  StjTrackListReader* _reader;

  ClassDef(StjTPCTree, 1)

};

#endif // STJTPCTREE_H
