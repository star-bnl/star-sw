// $Id: StjTrackPrint.cxx,v 1.1 2008/08/02 04:16:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetTrackPrint::operator()(const TrackList &trackList)
{
  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    print(*it);
  }
}

void StJetTrackPrint::print(const Track& track)
{
  cout 
    << track.runNumber      << " "
    << track.eventId        << " "
    << track.detectorId     << " "
    << track.pt             << " "
    << track.eta            << " "
    << track.phi            << " "
    << track.flag           << " "
    << track.nHits          << " "
    << track.charge         << " "
    << track.nHitsPoss      << " "
    << track.nHitsDedx      << " "
    << track.nHitsFit       << " "
    << track.nSigmaPion     << " "
    << track.Tdca           << " "
    << track.dcaZ           << " "
    << track.dcaD           << " "
    << track.BField         << " "
    << track.bemcRadius     << " "
    << track.vertexZ        << " "
    << track.exitDetectorId << " "
    << track.exitTowerId    << " "
    << track.exitEta        << " "
    << track.exitPhi        << " "
    << track.dEdx           << " "
    << track.trackIndex     << " "
    << track.id             << " "
    << endl;

}



}
