// $Id: StjTPCTree.cxx,v 1.5 2008/08/11 03:50:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTPCTree.h"

#include "StjTrackListReader.h"

ClassImp(StjTPCTree)

StjTrackList StjTPCTree::getTrackList()
{
  return _reader->getTrackList();
}
