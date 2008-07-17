// -*- mode: c++;-*-
// $Id: RunJetFinder.h,v 1.3 2008/07/17 17:49:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDER_HH
#define RUNJETFINDER_HH

#include <StJetFinder/StProtoJet.h>

#include "FourVecList.h"
#include "JetList.h"

#include <list>
#include <vector>

class StJetPars;
class StJetFinder;

class TObjArray;

namespace StSpinJet {

class RunJetFinder {

public:

  RunJetFinder(StJetPars* jp); 
  virtual ~RunJetFinder() { }

  void Init();

  TObjArray operator()(const TObjArray& fourList);
  JetList operator()(const FourVecList& fourList);

private:

  StJetFinder* _jetFinder;

};

}

#endif // RUNJETFINDER_HH

