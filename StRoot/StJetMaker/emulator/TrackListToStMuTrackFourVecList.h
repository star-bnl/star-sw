// -*- mode: c++;-*-
// $Id: TrackListToStMuTrackFourVecList.h,v 1.1 2008/07/21 22:15:50 tai Exp $
#ifndef TRACKLISTTOSTMUTRACKFOURVECLIST_H
#define TRACKLISTTOSTMUTRACKFOURVECLIST_H

#include "TrackList.h"

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class TrackToTLorentzVector;

class StMuTrackEmu;

class TrackListToStMuTrackFourVecList {

public:
  TrackListToStMuTrackFourVecList();
  virtual ~TrackListToStMuTrackFourVecList() { }

  FourList operator()(const TrackList& trackList);

private:

  StMuTrackEmu* createTrackEmu(const Track& track);

  TrackToTLorentzVector& _trackTo4p;

};

}

#endif // TRACKLISTTOSTMUTRACKFOURVECLIST_H
