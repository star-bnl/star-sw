// -*- mode: c++;-*-
// $Id: RunJetFinder.h,v 1.3 2008/08/02 19:23:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDER_HH
#define RUNJETFINDER_HH

#include <StJetFinder/StProtoJet.h>

#include "StjFourVecList.h"
#include "StjJetList.h"

class StJetPars;
class StJetFinder;

namespace StSpinJet {

class RunJetFinder {

public:

  RunJetFinder(StJetPars* jp); 
  virtual ~RunJetFinder() { }

  void Init();

  StjJetList operator()(const StjFourVecList& fourList);

private:

  StJetFinder* _jetFinder;

};

}

#endif // RUNJETFINDER_HH

