// $Id: StJetTPCTrackPrint.cxx,v 1.2 2008/07/09 01:04:56 tai Exp $
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

    print(*track);

  }

}

void StJetTPCTrackPrint::print(const StMuTrackEmu& track) const
{

}



}
