// -*- mode: c++;-*-
// $Id: StjDijetCut.h,v 1.1 2008/09/11 23:34:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUT_H
#define STJDIJETCUT_H

#include <TObject.h>

#include "StjDijetList.h"

class StjDijetCut : public TObject {

public:
  StjDijetCut() { }
  virtual ~StjDijetCut() { }

  virtual bool operator()(const StjDijet& dijet) = 0;

private:

  ClassDef(StjDijetCut, 1)

};

#endif // STJDIJETCUT_H
