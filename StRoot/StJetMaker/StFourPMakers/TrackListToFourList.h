// -*- mode: c++;-*-
// $Id: TrackListToFourList.h,v 1.2 2008/07/12 01:32:08 tai Exp $
#ifndef TRACKLISTTOFOURLIST_H
#define TRACKLISTTOFOURLIST_H

#include <StJetFinder/AbstractFourVec.h>

#include "TrackList.h"

#include <vector>

typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class StMuTrackEmu;

class TrackListToFourList {

public:
  TrackListToFourList() { }
  virtual ~TrackListToFourList() { }

  FourList operator()(const TrackList& trackList);

private:

  StMuTrackEmu* createTrackEmu(const Track& track);

};

}


#endif // TRACKLISTTOFOURLIST_H
