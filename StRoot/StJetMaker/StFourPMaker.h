// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.8 2010/04/24 04:15:27 pibero Exp $
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

  virtual const vector<VertexNode>& getVertexNodes() const = 0;
  virtual bool bemcCorrupt() const { return false; }

private:

  ClassDef(StFourPMaker,1)
};

#endif // STFOURPMAKER_H
