// $Id: StJetTPCTrackPrint.cxx,v 1.1 2008/07/21 17:24:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetTPCTrackPrint.h"

#include <iostream>
#include <fstream>

using namespace std;

namespace StSpinJet {

void StJetTPCTrackPrint::operator()(const TrackList& trackList)
{
  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    print(*it);
  }
}

void StJetTPCTrackPrint::print(const Track& track) const
{
  static ofstream ofs("./tpctracks.txt");

  ofs 
    << track.runNumber	      << " "
    << track.eventId	      << " "
    << track.detectorId       << " "
    << track.pt               << " "
    << track.eta              << " "
    << track.phi              << " "
    << track.flag	      << " "
    << track.nHits	      << " "
    << track.charge	      << " "
    << track.nHitsPoss	      << " "
    << track.nHitsDedx	      << " "
    << track.nHitsFit	      << " "
    << track.nSigmaPion       << " "
    << track.Tdca             << " "
    << track.dcaZ             << " "
    << track.dcaD             << " "
    << track.BField	      << " "
    << track.bemcRadius	      << " "
    << track.vertexZ	      << " "
    << track.exitDetectorId   << " "
    << track.exitTowerId      << " "
    << track.exitEta         << " "
    << track.exitPhi        << " "
    << track.dEdx	   << " "
    << track.trackIndex   << " "
    << track.id          << " "
    << endl;

}



}
