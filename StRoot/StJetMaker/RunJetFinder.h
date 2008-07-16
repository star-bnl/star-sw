// -*- mode: c++;-*-
// $Id: RunJetFinder.h,v 1.1 2008/07/16 03:54:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef RUNJETFINDER_HH
#define RUNJETFINDER_HH

#include <StJetFinder/StProtoJet.h>

#include <list>
#include <vector>

class StJetPars;
class StJetFinder;

class TClonesArray;

namespace StSpinJet {

class RunJetFinder {

public:

  RunJetFinder(StJetPars* jp); 
  virtual ~RunJetFinder() { }

  void Init();

  TClonesArray operator()(const TClonesArray& fourList);

private:

  StJetFinder* _jetFinder;

};

}

#endif // RUNJETFINDER_HH

