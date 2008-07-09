// $Id: StBET4pMakerImp.cxx,v 1.75 2008/07/09 10:44:07 tai Exp $

#include "StBET4pMakerImp.h"

#include "CollectEnergyDepositsFromBEMC.h"
#include "StJetEEMC.h"
#include "TrackListToFourList.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

StBET4pMakerImp::StBET4pMakerImp(CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
				 CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC,
				 CorrectTowerEnergyForTracks* correctTowerEnergyForTracks,
				 StJetEEMC* eemc)
  : mUseEndcap(false)
  , mUseBEMC(true)
  , _collectChargedTracksFromTPC(collectChargedTracksFromTPC)
  , _collectEnergyDepositsFromBEMC(collectEnergyDepositsFromBEMC)
  , _eemc(eemc)
  , _correctTowerEnergyForTracks(correctTowerEnergyForTracks)
  , _track2four(*(new TrackListToFourList))
{

}

void StBET4pMakerImp::Clear(Option_t* opt)
{
    
  for (FourList::iterator it = _tracks.begin(); it != _tracks.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  _tracks.clear();

}

void StBET4pMakerImp::Make()
{
  TrackList trackmuList = _collectChargedTracksFromTPC->Do();

  FourList tpcFourMomentumList = _track2four(trackmuList);

  _tracks.insert(_tracks.end(), tpcFourMomentumList.begin(), tpcFourMomentumList.end());

  if(mUseBEMC) {
    TowerEnergyDepositList bemcEnergyDepositList = _collectEnergyDepositsFromBEMC->Do();

    TowerEnergyDepositList bemcCorrectedEnergyDepositList = _correctTowerEnergyForTracks->Do(bemcEnergyDepositList, trackmuList);

    FourList bemcFourMomentumList = constructFourMomentumListFrom(bemcCorrectedEnergyDepositList);

    _tracks.insert(_tracks.end(), bemcFourMomentumList.begin(), bemcFourMomentumList.end());
  }


  if(mUseEndcap) {

    TowerEnergyDepositList eemcEnergyList = _eemc->getEnergyList();

    FourList eemcFourMomentumList = constructFourMomentumListFrom(eemcEnergyList);

    _tracks.insert(_tracks.end(), eemcFourMomentumList.begin(), eemcFourMomentumList.end());
  }

}


FourList StBET4pMakerImp::constructFourMomentumListFrom(const TowerEnergyDepositList& energyDepositList)
{
  FourList ret;

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TLorentzVector p4 = constructFourMomentum((*it));
	    
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}

TLorentzVector StBET4pMakerImp::constructFourMomentum(const TowerEnergyDeposit& deposit)
{
  TVector3 towerLocation(deposit.towerX, deposit.towerY, deposit.towerZ); 
  TVector3 vertex(deposit.vertexX, deposit.vertexY, deposit.vertexZ);

  TVector3 momentum = towerLocation - vertex;

  double mass(0); // assume photon mass

  double pMag = (deposit.energy > mass) ? sqrt(deposit.energy*deposit.energy - mass*mass) : deposit.energy;

  momentum.SetMag(pMag);

  return TLorentzVector(momentum.x(), momentum.y(), momentum.z(), deposit.energy);
}


