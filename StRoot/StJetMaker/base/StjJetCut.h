// -*- mode: c++;-*-
// $Id: StjJetCut.h,v 1.6 2008/08/13 04:55:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUT_H
#define STJJETCUT_H

#include <TObject.h>

#include "StjJetList.h"

class StjJetCut : public TObject {

public:
  StjJetCut() { }
  virtual ~StjJetCut() { }

  virtual bool operator()(const StjJet& jet) = 0;

private:

  ClassDef(StjJetCut, 1)

};

#endif // STJJETCUT_H
