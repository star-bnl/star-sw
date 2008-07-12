// -*- mode: c++;-*-
// $Id: StJetTPC.h,v 1.5 2008/07/12 01:32:07 tai Exp $
#ifndef STJETTPC_H
#define STJETTPC_H

#include "TrackList.h"

namespace StSpinJet {

class StMuTrackEmu;

class StJetTPC {

public:
  StJetTPC() { }
  virtual ~StJetTPC() { }

  virtual void Init() { }

  virtual TrackList getTrackList() = 0;
};


class StJetTPCNull : public StJetTPC {

public:
  StJetTPCNull() { }
  virtual ~StJetTPCNull() { }

  void Init() { }

  TrackList getTrackList() { return TrackList(); }
};

}

#endif // STJETTPC_H
