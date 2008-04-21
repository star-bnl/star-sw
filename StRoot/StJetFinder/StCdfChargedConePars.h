// -*- mode: c++;-*-
// $Id: StCdfChargedConePars.h,v 1.2 2008/04/21 17:29:24 tai Exp $
#ifndef STCDFCHARGEDCONEPARS_H
#define STCDFCHARGEDCONEPARS_H

#include "StConePars.h"

class StCdfChargedConePars : public StConePars {

public:

  virtual StJetFinder* constructJetFinder();

private:    
  ClassDef(StCdfChargedConePars,1)
};

#endif // STCDFCHARGEDCONEPARS_H
