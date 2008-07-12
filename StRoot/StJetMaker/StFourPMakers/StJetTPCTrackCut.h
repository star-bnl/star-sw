// -*- mode: c++;-*-
// $Id: StJetTPCTrackCut.h,v 1.3 2008/07/12 01:32:07 tai Exp $
#ifndef STJETTPCTRACKCUT_H
#define STJETTPCTRACKCUT_H

#include "TrackList.h"

#include <vector>

namespace StSpinJet {

class StJetTPCTrackCut {

public:

  StJetTPCTrackCut() : _use2006Cuts(false) { }
  virtual ~StJetTPCTrackCut() { }

  void setUse2006Cuts(bool v) { _use2006Cuts = v; }

  TrackList operator()(const TrackList& trackList);

  bool Use2006Cuts() const { return _use2006Cuts; }

private:

  bool shoudNotPass(const Track& track) const;

  bool _use2006Cuts;
};

}

#endif // STJETTPCTRACKCUT_H
