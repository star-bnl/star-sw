// -*- mode: c++;-*-
// $Id: StJetPars.h,v 1.2 2008/04/21 17:29:25 tai Exp $
#ifndef STJETPARS_H
#define STJETPARS_H

#include "TObject.h"

class StJetFinder;

class StJetPars {

public:

  virtual StJetFinder* constructJetFinder() = 0;

private:
    ClassDef(StJetPars,1)
};

#endif // STJETPARS_H
