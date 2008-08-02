// -*- mode: c++;-*-
// $Id: StjTPCTxt.h,v 1.1 2008/08/02 04:15:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTXT_H
#define STJETTPCTXT_H

#include "StjTPC.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetTPCTxt : public StJetTPC {

public:
  StJetTPCTxt(const char* path);
  virtual ~StJetTPCTxt() { }

  TrackList getTrackList();

private:

  std::ifstream _dataFile;
  long _currentEvent;
  std::string _oldLine;

};

}

#endif // STJETTPCTXT_H
