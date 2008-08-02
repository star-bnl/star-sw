// -*- mode: c++;-*-
// $Id: StjTrackListToStMuTrackFourVecList.h,v 1.3 2008/08/02 22:43:32 tai Exp $
#ifndef STJTRACKLISTTOSTMUTRACKFOURVECLIST_H
#define STJTRACKLISTTOSTMUTRACKFOURVECLIST_H

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

#endif // STJTRACKLISTTOSTMUTRACKFOURVECLIST_H
