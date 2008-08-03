// -*- mode: c++;-*-
// $Id: StjRunJetFinder.h,v 1.1 2008/08/03 00:28:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDER_H
#define RUNJETFINDER_H

#include <StJetFinder/StProtoJet.h>

#include "StjFourVecList.h"
#include "StjJetList.h"

class StJetPars;
class StJetFinder;

class StjRunJetFinder {

public:

  StjRunJetFinder(StJetPars* jp); 
  virtual ~StjRunJetFinder() { }

  void Init();

  StjJetList operator()(const StjFourVecList& fourList);

private:

  StJetFinder* _jetFinder;

};

#endif // RUNJETFINDER_H

