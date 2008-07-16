// $Id: TrackTowerEnergyListToFourList.cxx,v 1.2 2008/07/16 03:54:32 tai Exp $
#include "TrackTowerEnergyListToFourList.h"

#include "TLorentzVectorWithId.h"
#include <TrackToTLorentzVectorWithId.h>
#include <TowerEnergyToTLorentzVectorWithId.h>

namespace StSpinJet {

TClonesArray TrackTowerEnergyListToFourList::operator()(const std::pair<TrackList, TowerEnergyList>& inList)
{
  const TrackList& trackList = inList.first;
  const TowerEnergyList& energyList = inList.second;

  TrackToTLorentzVectorWithId track2p4;
  TowerEnergyToTLorentzVectorWithId tower2p4;

  TClonesArray ret("TLorentzVectorWithId", 10000);
  Int_t iPar(0);
  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {
    new(ret[iPar]) TLorentzVectorWithId(track2p4(*track));
    ((TLorentzVectorWithId*)ret[iPar])->particleId = iPar + 1;
    ++iPar;
  }

  for(TowerEnergyList::const_iterator tower = energyList.begin(); tower != energyList.end(); ++tower) {
    new(ret[iPar]) TLorentzVectorWithId(tower2p4(*tower));
    ((TLorentzVectorWithId*)ret[iPar])->particleId = iPar + 1;
    ++iPar;
  }

  return ret;
}


}
