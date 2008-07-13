// -*- mode: c++;-*-
// $Id: StJetTPCTrackPrint.h,v 1.3 2008/07/13 10:02:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
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
