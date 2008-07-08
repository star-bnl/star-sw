// -*- mode: c++;-*-
// $Id: StJetTPC.h,v 1.1 2008/07/08 04:16:04 tai Exp $
#ifndef STJETTPC_H
#define STJETTPC_H

#include <vector>

namespace StSpinJet {

class StMuTrackEmu;

class StJetTPC {

public:
  StJetTPC() { }
  virtual ~StJetTPC() { }

  typedef std::vector<StMuTrackEmu*> TrackList;

  virtual TrackList getTrackList() = 0;
};

}

#endif // STJETTPC_H
