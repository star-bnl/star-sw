// -*- mode: c++;-*-
// $Id: StjJetCut.h,v 1.5 2008/08/04 06:10:21 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUT_H
#define STJJETCUT_H

#include <TObject.h>

#include "StjJetList.h"

class StjJetCut : public TObject {

public:
  StjJetCut() { }
  virtual ~StjJetCut() { }

  virtual bool operator()(const StjJet& p4) = 0;

private:

  ClassDef(StjJetCut, 1)

};

#endif // STJJETCUT_H
