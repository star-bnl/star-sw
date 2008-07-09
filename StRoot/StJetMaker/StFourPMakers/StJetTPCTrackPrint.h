// -*- mode: c++;-*-
// $Id: StJetTPCTrackPrint.h,v 1.2 2008/07/09 02:40:04 tai Exp $
#ifndef STJETTPCTRACKPRINT_H
#define STJETTPCTRACKPRINT_H

#include <vector>

namespace StSpinJet {

class StMuTrackEmu;

class StJetTPCTrackPrint {

public:

  StJetTPCTrackPrint() { }
  virtual ~StJetTPCTrackPrint() { }

  typedef std::vector<StMuTrackEmu*> TrackList;

  void operator()(const TrackList& trackList);

private:

  void print(const StMuTrackEmu& track, long i) const;

};

}

#endif // STJETTPCTRACKPRINT_H
