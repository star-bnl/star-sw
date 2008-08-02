// -*- mode: c++;-*-
// $Id: StjTreeEntryMaker.h,v 1.2 2008/08/02 19:22:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTREEENTRYMAKER_H
#define STJETTREEENTRYMAKER_H

#include "StMaker.h"

class TDirectory;

class StjTreeEntryCoordinator;

class StjTreeEntryMaker : public StMaker {

public:
  StjTreeEntryMaker(const Char_t *name, TDirectory* file);
  StjTreeEntryMaker(const Char_t *name, const char* inputFileName);
  virtual ~StjTreeEntryMaker() { }

  StjTreeEntryCoordinator* coordinator() { return _coord; }

  Int_t Init();
  Int_t Make();

  void AddTrgTreeName(const char* treeName);

private:

  TDirectory* _file;
  StjTreeEntryCoordinator* _coord;

  ClassDef(StjTreeEntryMaker, 0)

};

#endif // #define STJETTREEENTRYMAKER_H
