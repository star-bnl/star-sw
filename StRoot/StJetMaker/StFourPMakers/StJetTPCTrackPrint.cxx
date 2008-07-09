// $Id: StJetTPCTrackPrint.cxx,v 1.1 2008/07/09 01:01:10 tai Exp $
#include "StJetTPCTrackPrint.h"

#include "../StMuTrackEmu.h"

#include <TVector3.h>

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetTPCTrackPrint::operator()(const TrackList& trackList)
{
  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    StMuTrackEmu* track = *it;

    if (print(*track)) continue;

  }

}

void StJetTPCTrackPrint::print(const StMuTrackEmu& track) const
{

}



}
