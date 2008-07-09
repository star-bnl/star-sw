// $Id: TrackListToFourList.cxx,v 1.1 2008/07/09 10:41:27 tai Exp $
#include "TrackListToFourList.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackFourVec.h"

namespace StSpinJet {

FourList TrackListToFourList::operator()(const TrackList& trackList)
{
  FourList ret;

  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {

    TVector3 momentum((*track)->px(), (*track)->py(), (*track)->pz());
    double pionMass = 0.1395700;
    float energy = sqrt(pionMass*pionMass + momentum.Mag()*momentum.Mag());

    TLorentzVector p4(momentum, energy);

    StMuTrackFourVec* pmu = new StMuTrackFourVec((*track), p4, (*track)->charge(), (*track)->trackIndex(), kTpcId);
    ret.push_back(pmu);
  }
  return ret;
}

}
