// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.6 2008/08/02 22:43:04 tai Exp $
#ifndef STFOURPMAKER_H
#define STFOURPMAKER_H

#include <StMaker.h>

#include "StJetFinder/AbstractFourVec.h"


class StFourPMaker : public StMaker {

public:

  StFourPMaker(const char *name)
    : StMaker(name) { }

  typedef std::vector<AbstractFourVec*> FourList;
  virtual FourList &getTracks() = 0;

  virtual bool bemcCorrupt() const { return false; }


  ClassDef(StFourPMaker,0)
};
#endif // STFOURPMAKER_H



