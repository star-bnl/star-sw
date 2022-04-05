// -*- mode: c++;-*-
// $Id: StjVertexTree.h,v 1.1 2008/08/13 19:37:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJVERTEXTREE_H
#define STJVERTEXTREE_H

#include <StjVertex.h>

class StjVertexReader;

class StjVertexTree : public StjVertex {

public:
  StjVertexTree(StjVertexReader* reader)
    : _reader(reader) { }
  virtual ~StjVertexTree() { }

private:

  int runNumber();
  int eventId();
  double vertexZ();
  double vertexY();
  double vertexX();

  StjVertexReader* _reader;

  ClassDef(StjVertexTree, 1)

};

#endif // STJVERTEXTREE_H
