// $Id: StJetTPCTrackPrint.cxx,v 1.3 2008/07/09 02:40:04 tai Exp $
#include "StJetTPCTrackPrint.h"

#include "../StMuTrackEmu.h"

#include <TVector3.h>

#include <iostream>
#include <fstream>

using namespace std;

namespace StSpinJet {

void StJetTPCTrackPrint::operator()(const TrackList& trackList)
{
  static long i(0);
  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    StMuTrackEmu* track = *it;

    print(*track, i);

  }
  ++i;
}

void StJetTPCTrackPrint::print(const StMuTrackEmu& track, long i) const
{
  static ofstream ofs("./tpctracks.txt");

  ofs 
    << i << " "
    << track.px() << " "
    << track.py() << " "
    << track.pz() << " "
    << track.flag() << " "
    << track.nHits() << " "
    << track.charge() << " "
    << track.nHitsPoss() << " "
    << track.nHitsDedx() << " "
    << track.nHitsFit() << " "
    << track.nSigmaPion() << " "
    << track.Tdca() << " "
    << track.dcaZ() << " "
    << track.dcaD() << " "
    << track.BField() << " "
    << track.bemcRadius() << " "
    << track.etaext() << " "
    << track.phiext() << " "
    << track.dEdx() << " "
    << track.trackIndex() << " "
    << track.id() 
    << endl;

}



}
