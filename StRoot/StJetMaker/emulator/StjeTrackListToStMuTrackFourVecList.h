// -*- mode: c++;-*-
// $Id: StjeTrackListToStMuTrackFourVecList.h,v 1.2 2008/08/03 00:26:53 tai Exp $
#ifndef STJTRACKLISTTOSTMUTRACKFOURVECLIST_H
#define STJTRACKLISTTOSTMUTRACKFOURVECLIST_H

#include "StjTrackList.h"

#include <StJetFinder/AbstractFourVec.h>
typedef std::vector<AbstractFourVec*> FourList;

class StjTrackToTLorentzVector;

class StMuTrackEmu;

class StjeTrackListToStMuTrackFourVecList {

public:
  StjeTrackListToStMuTrackFourVecList();
  virtual ~StjeTrackListToStMuTrackFourVecList() { }

  FourList operator()(const StjTrackList& trackList);

private:

  StMuTrackEmu* createTrackEmu(const StjTrack& track);

  StjTrackToTLorentzVector& _trackTo4p;

};

#endif // STJTRACKLISTTOSTMUTRACKFOURVECLIST_H
