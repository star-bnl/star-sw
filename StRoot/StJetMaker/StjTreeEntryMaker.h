// -*- mode: c++;-*-
// $Id: StjTreeEntryMaker.h,v 1.1 2008/08/02 04:06:05 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEENTRYMAKER_H
#define STJETTREEENTRYMAKER_H

#include "StMaker.h"

class TDirectory;

class StJetTreeEntryCoordinator;

class StJetTreeEntryMaker : public StMaker {

public:
  StJetTreeEntryMaker(const Char_t *name, TDirectory* file);
  StJetTreeEntryMaker(const Char_t *name, const char* inputFileName);
  virtual ~StJetTreeEntryMaker() { }

  StJetTreeEntryCoordinator* coordinator() { return _coord; }

  Int_t Init();
  Int_t Make();

  void AddTrgTreeName(const char* treeName);

private:

  TDirectory* _file;
  StJetTreeEntryCoordinator* _coord;

  ClassDef(StJetTreeEntryMaker, 0)

};

#endif // #define STJETTREEENTRYMAKER_H
