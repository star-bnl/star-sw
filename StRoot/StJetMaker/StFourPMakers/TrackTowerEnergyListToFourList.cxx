// $Id: TrackTowerEnergyListToFourList.cxx,v 1.4 2008/07/17 02:39:53 tai Exp $
#include "TrackTowerEnergyListToFourList.h"

#include "TLorentzVectorWithId.h"
#include <TrackToTLorentzVectorWithId.h>
#include <TowerEnergyToTLorentzVectorWithId.h>

namespace StSpinJet {

TObjArray TrackTowerEnergyListToFourList::operator()(const std::pair<TrackList, TowerEnergyList>& inList)
{
  //  const TrackList& trackList = inList.first;
  //  const TowerEnergyList& energyList = inList.second;
  return operator()(inList.first, inList.second);
}

TObjArray TrackTowerEnergyListToFourList::operator()(const TrackList& trackList, const TowerEnergyList& energyList)
{
  TrackToTLorentzVectorWithId track2p4;
  TowerEnergyToTLorentzVectorWithId tower2p4;

  TObjArray ret(40000);
  Int_t iPar(0);
  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {
    ret[iPar] = new TLorentzVectorWithId(track2p4(*track));
    ((TLorentzVectorWithId*)ret[iPar])->particleId = iPar + 1;
    ++iPar;
  }

  for(TowerEnergyList::const_iterator tower = energyList.begin(); tower != energyList.end(); ++tower) {
    ret[iPar] = new TLorentzVectorWithId(tower2p4(*tower));
    ((TLorentzVectorWithId*)ret[iPar])->particleId = iPar + 1;
    ++iPar;
  }

  return ret;
}


}
