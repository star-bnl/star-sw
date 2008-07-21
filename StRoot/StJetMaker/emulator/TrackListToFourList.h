// -*- mode: c++;-*-
// $Id: TrackListToFourList.h,v 1.1 2008/07/21 02:00:27 tai Exp $
#ifndef TRACKLISTTOFOURLIST_H
#define TRACKLISTTOFOURLIST_H

#include "TrackList.h"

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class TrackToTLorentzVector;

class StMuTrackEmu;

class TrackListToFourList {

public:
  TrackListToFourList();
  virtual ~TrackListToFourList() { }

  FourList operator()(const TrackList& trackList);

private:

  StMuTrackEmu* createTrackEmu(const Track& track);

  TrackToTLorentzVector& _trackTo4p;

};

}

#endif // TRACKLISTTOFOURLIST_H
