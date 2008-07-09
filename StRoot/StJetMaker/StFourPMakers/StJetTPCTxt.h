// -*- mode: c++;-*-
// $Id: StJetTPCTxt.h,v 1.2 2008/07/09 04:26:39 tai Exp $
#ifndef STJETTPCTXT_H
#define STJETTPCTXT_H

#include "StJetTPC.h"

#include <fstream>

namespace StSpinJet {

class StJetTPCTxt : public StJetTPC {

public:
  StJetTPCTxt(const char* path);
  virtual ~StJetTPCTxt() { }

  TrackList getTrackList();

private:

  std::ifstream _dataFile;

};

}

#endif // STJETTPCTXT_H
