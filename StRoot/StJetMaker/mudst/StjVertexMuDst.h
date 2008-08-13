// -*- mode: c++;-*-
// $Id: StjVertexMuDst.h,v 1.1 2008/08/13 19:37:21 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJVERTEXMUDST_H
#define STJVERTEXMUDST_H

#include <StjVertex.h>

class StMuDstMaker;

class StjVertexMuDst : public StjVertex {

public:
  StjVertexMuDst(StMuDstMaker* uDstMaker)
    : _uDstMaker(uDstMaker) { }
  virtual ~StjVertexMuDst() { }

  int runNumber();
  int eventId();
  double vertexZ();
  double vertexY();
  double vertexX();

private:

  StMuDstMaker* _uDstMaker;

  ClassDef(StjVertexMuDst, 1)

};

#endif // STJVERTEXMUDST_H
