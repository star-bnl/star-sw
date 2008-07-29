// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.5 2008/07/29 00:16:35 tai Exp $
#ifndef StFourPMaker_h
#define StFourPMaker_h

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
#endif



