// -*- mode: c++;-*-
// $Id: StjTPCTxt.h,v 1.1 2008/11/27 07:09:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPCTXT_H
#define STJTPCTXT_H

#include "StjTPC.h"

#include <fstream>
#include <string>

class StjTPCTxt : public StjTPC {

public:
  StjTPCTxt(const char* path);
  virtual ~StjTPCTxt() { }

  StjTrackList getTrackList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

  ClassDef(StjTPCTxt, 1)

};

#endif // STJTPCTXT_H
