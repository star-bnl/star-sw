// -*- mode: c++;-*-
// $Id: StjTPCTxt.h,v 1.3 2008/08/02 22:43:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPCTXT_H
#define STJTPCTXT_H

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

#endif // STJTPCTXT_H
