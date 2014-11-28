// -*- mode: c++;-*-
// $Id: StjJetVariation.h,v 1.1 2008/09/12 22:33:01 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETVARIATION_H
#define STJJETVARIATION_H

#include <TObject.h>

#include "StjJetList.h"

class StjJetVariation : public TObject {

public:
  StjJetVariation() { }
  virtual ~StjJetVariation() { }

  virtual StjJet operator()(const StjJet& item) = 0;

private:

  ClassDef(StjJetVariation, 1)

};

#endif // STJJETVARIATION_H
