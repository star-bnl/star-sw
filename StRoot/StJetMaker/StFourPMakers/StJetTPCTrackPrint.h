// -*- mode: c++;-*-
// $Id: StJetTPCTrackPrint.h,v 1.1 2008/07/09 01:01:11 tai Exp $
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

  void print(const StMuTrackEmu& track) const;

};

}

#endif // STJETTPCTRACKPRINT_H
