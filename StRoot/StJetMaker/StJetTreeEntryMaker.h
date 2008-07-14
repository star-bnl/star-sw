// -*- mode: c++;-*-
// $Id: StJetTreeEntryMaker.h,v 1.2 2008/07/14 07:12:03 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEENTRYMAKER_H
#define STJETTREEENTRYMAKER_H

#include "StMaker.h"

class TFile;

class StJetTreeEntryCoordinator;

class StJetTreeEntryMaker : public StMaker {

public:
  StJetTreeEntryMaker(const Char_t *name, const char* inputFileName);
  virtual ~StJetTreeEntryMaker() { }

  StJetTreeEntryCoordinator* coordinator() { return _coord; }

  Int_t Init();
  Int_t Make();

  void AddTrgTreeName(const char* treeName);

private:

  TFile* _file;
  StJetTreeEntryCoordinator* _coord;

  ClassDef(StJetTreeEntryMaker, 0)

};

#endif // #define STJETTREEENTRYMAKER_H
