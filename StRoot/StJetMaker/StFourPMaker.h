// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.9 2010/05/22 13:43:20 pibero Exp $
#ifndef STFOURPMAKER_H
#define STFOURPMAKER_H

#include <StMaker.h>
#include <StThreeVectorF.hh>

#include "StJetFinder/AbstractFourVec.h"

class StMuPrimaryVertex;

typedef std::vector<AbstractFourVec*> FourList;

struct VertexNode {
  StMuPrimaryVertex* vertex;
  FourList tracks;
};

class StFourPMaker : public StMaker {

public:

  StFourPMaker(const char *name) : StMaker(name) { }

  virtual const vector<VertexNode>& getVertexNodes() const { return _vertexNodes; }
  virtual bool bemcCorrupt() const { return false; }

protected:
  vector<VertexNode> _vertexNodes;

  ClassDef(StFourPMaker,0)
};

#endif // STFOURPMAKER_H
