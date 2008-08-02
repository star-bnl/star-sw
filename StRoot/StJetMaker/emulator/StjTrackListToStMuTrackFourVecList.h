// -*- mode: c++;-*-
// $Id: StjTrackListToStMuTrackFourVecList.h,v 1.2 2008/08/02 19:23:07 tai Exp $
#ifndef TRACKLISTTOSTMUTRACKFOURVECLIST_H
#define TRACKLISTTOSTMUTRACKFOURVECLIST_H

#include "StjTrackList.h"

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

namespace StSpinJet {

class StjTrackToTLorentzVector;

class StMuTrackEmu;

class StjTrackListToStMuTrackFourVecList {

public:
  StjTrackListToStMuTrackFourVecList();
  virtual ~StjTrackListToStMuTrackFourVecList() { }

  FourList operator()(const StjTrackList& trackList);

private:

  StMuTrackEmu* createTrackEmu(const StjTrack& track);

  StjTrackToTLorentzVector& _trackTo4p;

};

}

#endif // TRACKLISTTOSTMUTRACKFOURVECLIST_H
