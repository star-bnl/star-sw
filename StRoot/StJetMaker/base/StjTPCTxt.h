// -*- mode: c++;-*-
// $Id: StjTPCTxt.h,v 1.2 2008/08/02 19:22:49 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTXT_H
#define STJETTPCTXT_H

#include "StjTPC.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StjTPCTxt : public StjTPC {

public:
  StjTPCTxt(const char* path);
  virtual ~StjTPCTxt() { }

  StjTrackList getTrackList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

};

}

#endif // STJETTPCTXT_H
