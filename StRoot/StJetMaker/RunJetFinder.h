// -*- mode: c++;-*-
// $Id: RunJetFinder.h,v 1.4 2008/07/21 22:15:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDER_HH
#define RUNJETFINDER_HH

#include <StJetFinder/StProtoJet.h>

#include "base/FourVecList.h"
#include "base/JetList.h"

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

