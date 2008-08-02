// -*- mode: c++;-*-
// $Id: RunJetFinder.h,v 1.2 2008/08/02 04:18:57 tai Exp $
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

  JetList operator()(const FourVecList& fourList);

private:

  StJetFinder* _jetFinder;

};

}

#endif // RUNJETFINDER_HH

