// -*- mode: c++;-*-
// $Id: StjDijetPrint.h,v 1.1 2008/09/11 23:34:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETPRINT_H
#define STJDIJETPRINT_H

#include <TObject.h>

#include "StjDijetList.h"

#include <fstream>
#include <string>

class StjDijetPrint : public TObject {

public:

  StjDijetPrint() { }
  virtual ~StjDijetPrint() { }

  void operator()(const StjDijetList& dijetList);

private:

  void print(const StjDijet& dijet);

  ClassDef(StjDijetPrint, 1)

};

#endif // STJDIJETPRINT_H
