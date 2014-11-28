// -*- mode: c++;-*-
// $Id: StjMCTree.h,v 1.1 2008/11/27 07:40:08 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCTREE_H
#define STJMCTREE_H

#include "StjMC.h"

class StjMCParticleListReader;

class StjMCTree : public StjMC {

public:
  StjMCTree(StjMCParticleListReader* reader)
    : _reader(reader) { }
  virtual ~StjMCTree() { }

  StjMCParticleList getMCParticleList();

private:

  StjMCParticleListReader* _reader;

  ClassDef(StjMCTree, 1)

};

#endif // STJMCTREE_H
