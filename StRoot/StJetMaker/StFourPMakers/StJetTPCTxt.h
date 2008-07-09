// -*- mode: c++;-*-
// $Id: StJetTPCTxt.h,v 1.3 2008/07/09 08:16:05 tai Exp $
#ifndef STJETTPCTXT_H
#define STJETTPCTXT_H

#include "StJetTPC.h"

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
