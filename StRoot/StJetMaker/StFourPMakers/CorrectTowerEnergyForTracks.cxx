// $Id: CorrectTowerEnergyForTracks.cxx,v 1.1 2008/06/10 09:17:57 tai Exp $
#include "CorrectTowerEnergyForTracks.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StMuEmcPosition.h"

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
    const StMuTrack* track = (*it).first;
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

void CorrectTowerEnergyForTracks::countTracksOnBemcTower(const StMuTrack& track)
{
  StMuDst* uDst = mMuDstMaker->muDst();

  StThreeVectorD momentumAt, positionAt;
	
  double magneticField = uDst->event()->magneticField()/10.0; //to put it in Tesla
  StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
  StMuEmcPosition muEmcPosition;
  bool tok = muEmcPosition.trackOnEmc(&positionAt, &momentumAt, &track, magneticField, geom->Radius());
  if(tok) {
    int m,e,s,id=0;
    geom->getBin(positionAt.phi(), positionAt.pseudoRapidity(), m, e, s);
    int bad = geom->getId(m,e,s,id);
    if(bad == 0) {
      mNtracksOnTower[id]++;
    }
  }
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
