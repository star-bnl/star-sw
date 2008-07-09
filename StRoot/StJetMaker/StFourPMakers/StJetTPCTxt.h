// -*- mode: c++;-*-
// $Id: StJetTPCTxt.h,v 1.1 2008/07/09 02:40:04 tai Exp $
#ifndef STJETTPCTXT_H
#define STJETTPCTXT_H

#include "StJetTPC.h"

namespace StSpinJet {

class StJetTPCTxt : public StJetTPC {

public:
  StJetTPCTxt(const char* path);
  virtual ~StJetTPCTxt() { }

  TrackList getTrackList();

private:


};

}

#endif // STJETTPCTXT_H
