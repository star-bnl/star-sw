// -*- mode: c++;-*-
// $Id: StJetTreeEntryMaker.h,v 1.1 2008/07/14 06:44:57 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEENTRYMAKER_H
#define STJETTREEENTRYMAKER_H

#include "StMaker.h"

class StJetTreeEntryCoordinator;

class StJetTreeEntryMaker : public StMaker {

public:
  StJetTreeEntryMaker(const Char_t *name, const char* inputFileName) { }
  virtual ~StJetTreeEntryMaker() { }

  StJetTreeEntryCoordinator* coordinator() { return _coord; }

private:

  StJetTreeEntryCoordinator* _coord;

  ClassDef(StJetTreeEntryMaker, 0)

};

#endif // #define STJETTREEENTRYMAKER_H
