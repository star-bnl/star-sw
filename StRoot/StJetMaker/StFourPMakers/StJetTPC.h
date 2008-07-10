// -*- mode: c++;-*-
// $Id: StJetTPC.h,v 1.2 2008/07/10 01:56:09 tai Exp $
#ifndef STJETTPC_H
#define STJETTPC_H

#include <vector>

namespace StSpinJet {

class StMuTrackEmu;

class StJetTPC {

public:
  StJetTPC() { }
  virtual ~StJetTPC() { }

  virtual void Init() { }

  typedef std::vector<StMuTrackEmu*> TrackList;

  virtual TrackList getTrackList() = 0;
};

}

#endif // STJETTPC_H
