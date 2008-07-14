// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.12 2008/07/14 21:18:42 tai Exp $
#ifndef StFourPMaker_h
#define StFourPMaker_h

#include <StMaker.h>

#include "StJetFinder/AbstractFourVec.h"

typedef std::vector<AbstractFourVec*> FourList;

class StFourPMaker : public StMaker {

public:

  StFourPMaker(const char *name)
    : StMaker(name) { }

  virtual FourList &getTracks() = 0;

  virtual bool bemcCorrupt() const { return false; }


  ClassDef(StFourPMaker,0)
};
#endif



