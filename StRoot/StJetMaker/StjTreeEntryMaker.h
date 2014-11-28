// -*- mode: c++;-*-
// $Id: StjTreeEntryMaker.h,v 1.4 2008/08/10 23:04:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEENTRYMAKER_H
#define STJTREEENTRYMAKER_H

#include "StMaker.h"

class TDirectory;

class StjTreeEntryCoordinator;

class StjTreeEntryMaker : public StMaker {

public:
  StjTreeEntryMaker(const Char_t *name, StjTreeEntryCoordinator* coord)
  : StMaker(name), _coord(coord) { }
  virtual ~StjTreeEntryMaker() { }

  StjTreeEntryCoordinator* coordinator() { return _coord; }

  Int_t Init();
  Int_t Make();

private:

  StjTreeEntryCoordinator* _coord;

  ClassDef(StjTreeEntryMaker, 0)

};

#endif // STJTREEENTRYMAKER_H
