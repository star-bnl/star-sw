// -*- mode: c++;-*-
// $Id: StjRunJetFinder.h,v 1.2 2008/08/04 06:10:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDER_H
#define RUNJETFINDER_H

#include <TObject.h>

#include <StJetFinder/StProtoJet.h>

#include "StjFourVecList.h"
#include "StjJetList.h"

class StJetPars;
class StJetFinder;

class StjRunJetFinder : public TObject {

public:

  StjRunJetFinder() { }
  virtual ~StjRunJetFinder() { }

  void Init(StJetPars* pars);

  StjJetList operator()(const StjFourVecList& fourList);

private:

  StJetFinder* _jetFinder;

  ClassDef(StjRunJetFinder, 1)

};

#endif // RUNJETFINDER_H

