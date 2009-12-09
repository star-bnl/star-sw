// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.7 2009/12/09 05:12:02 pibero Exp $
#ifndef STFOURPMAKER_H
#define STFOURPMAKER_H

#include <StMaker.h>
#include <StThreeVectorF.hh>

#include "StJetFinder/AbstractFourVec.h"


class StFourPMaker : public StMaker {

public:

  StFourPMaker(const char *name)
    : StMaker(name) { }

  typedef std::vector<AbstractFourVec*> FourList;
  virtual const StThreeVectorF& getVertex() const = 0;
  virtual FourList& getTracks() = 0;
  virtual bool bemcCorrupt() const { return false; }

private:

  StThreeVectorF mVertex;

  ClassDef(StFourPMaker,1)
};

#endif // STFOURPMAKER_H
