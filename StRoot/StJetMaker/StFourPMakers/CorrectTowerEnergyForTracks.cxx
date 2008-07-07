// $Id: CorrectTowerEnergyForTracks.cxx,v 1.2 2008/07/07 22:12:29 tai Exp $
#include "CorrectTowerEnergyForTracks.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StMuEmcPosition.h"
#include "../StMuTrackEmu.h"

namespace StSpinJet {

const int CorrectTowerEnergyForTracks::mNOfBemcTowers;

CorrectTowerEnergyForTracks::CorrectTowerEnergyForTracks(StMuDstMaker* uDstMaker)
  : mMuDstMaker(uDstMaker)
{

}

TowerEnergyDepositList CorrectTowerEnergyForTracks::Do(const TowerEnergyDepositList &energyDepositList, const TrackList& trackList)
{
  for (int i = 1; i <= mNOfBemcTowers; ++i) {
    mNtracksOnTower[i] = 0;
  }

  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    const StMuTrackEmu* track = *it;
    countTracksOnBemcTower(*track);
  }

  TowerEnergyDepositList ret;

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TowerEnergyDeposit energyDeposit(*it);

    energyDeposit.energy = correctBemcTowerEnergyForTracks_(energyDeposit.energy, energyDeposit.towerId);

    if(energyDeposit.energy <= 0) continue;

    ret.push_back(energyDeposit);
  }

  return ret;
}

void CorrectTowerEnergyForTracks::countTracksOnBemcTower(const StMuTrackEmu& track)
{
  //  int m, e, s, id = 0;
  int id = 0;

  //  StEmcGeom::instance("bemc")->getBin(track.phiext(), track.etaext(), m, e, s);

  //  int bad = StEmcGeom::instance("bemc")->getId(m,e,s,id);

  //  int bad = StEmcGeom::instance("bemc")->getId(track.phiext(), track.etaext(), id);

  //  if(bad == 0)  mNtracksOnTower[id]++;

  if(StEmcGeom::instance("bemc")->getId(track.phiext(), track.etaext(), id) == 0)
    mNtracksOnTower[id]++;
}

double CorrectTowerEnergyForTracks::correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId)
{

    //Get eta, phi
    float eta, phi;
    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    geom->getEtaPhi(bemcTowerId,eta,phi); // to convert software bemcTowerId into eta/phi

    //construct four momentum
	    
    float theta=2.*atan(exp(-eta));

    //do a quick correction for hadronic MIP eneryg deposition:
    double MipE = 0.261*(1.+0.056*eta*eta)/sin(theta); //GeV

    return energy - mNtracksOnTower[bemcTowerId]*MipE;
}


}
