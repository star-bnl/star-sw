// -*- mode: c++;-*-
// $Id: StJetPars.h,v 1.3 2008/04/22 00:14:59 tai Exp $
#ifndef STJETPARS_H
#define STJETPARS_H

#include "TObject.h"

class StJetFinder;

class StJetPars : public TObject {

public:

  virtual StJetFinder* constructJetFinder() = 0;

private:

    ClassDef(StJetPars,1)

};

#endif // STJETPARS_H
