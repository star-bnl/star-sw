// -*- mode: c++;-*-
// $Id: StjFourVecPrint.h,v 1.1 2008/11/27 07:29:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECPRINT_H
#define STJFOURVECPRINT_H

#include <TObject.h>

#include "StjFourVecList.h"

#include <fstream>
#include <string>

class StjFourVecPrint : public TObject {

public:

  StjFourVecPrint() { }
  virtual ~StjFourVecPrint() { }

  void operator()(const StjFourVecList& fourList);

private:

  void print(const StjFourVec& four);

  ClassDef(StjFourVecPrint, 1)

};

#endif // STJFOURVECPRINT_H
