// $Id: StjTPCTree.cxx,v 1.1 2008/11/27 07:09:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTPCTree.h"

#include "StjTrackListReader.h"

ClassImp(StjTPCTree)

StjTrackList StjTPCTree::getTrackList()
{
  return _reader->getTrackList();
}
