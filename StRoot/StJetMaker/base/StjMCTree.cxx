// $Id: StjMCTree.cxx,v 1.1 2008/08/22 17:25:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjMCTree.h"

#include "StjMCParticleListReader.h"

ClassImp(StjMCTree)

StjMCParticleList StjMCTree::getMCParticleList()
{
  return _reader->getMCParticleList();
}
