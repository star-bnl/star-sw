// $Id: StBET4pMakerImp.cxx,v 1.72 2008/07/08 11:21:56 tai Exp $

#include "StBET4pMakerImp.h"

#include "CollectEnergyDepositsFromBEMC.h"


//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

StBET4pMakerImp::StBET4pMakerImp(
				 CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
				 CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC,
				 CollectEnergyDepositsFromEEMC *collectEnergyDepositsFromEEMC,
				 CorrectTowerEnergyForTracks* correctTowerEnergyForTracks
				 )
  : mUseEndcap(false)
  , mUseBEMC(true)
  , _collectChargedTracksFromTPC(collectChargedTracksFromTPC)
  , _collectEnergyDepositsFromBEMC(collectEnergyDepositsFromBEMC)
  , _collectEnergyDepositsFromEEMC(collectEnergyDepositsFromEEMC)
  , _correctTowerEnergyForTracks(correctTowerEnergyForTracks)
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

  FourList tpcFourMomentumList = constructFourMomentumListFrom(trackmuList);

  _tracks.insert(_tracks.end(), tpcFourMomentumList.begin(), tpcFourMomentumList.end());

  if(mUseBEMC) {
    TowerEnergyDepositList bemcEnergyDepositList = _collectEnergyDepositsFromBEMC->Do();

    TowerEnergyDepositList bemcCorrectedEnergyDepositList = _correctTowerEnergyForTracks->Do(bemcEnergyDepositList, trackmuList);

    FourList bemcFourMomentumList = constructFourMomentumListFrom(bemcCorrectedEnergyDepositList);

    _tracks.insert(_tracks.end(), bemcFourMomentumList.begin(), bemcFourMomentumList.end());
  }


  if(mUseEndcap) {

    TowerEnergyDepositList eemcCorrectedEnergyDepositList = _collectEnergyDepositsFromEEMC->Do();

    FourList eemcFourMomentumList = constructFourMomentumListFrom(eemcCorrectedEnergyDepositList);

    _tracks.insert(_tracks.end(), eemcFourMomentumList.begin(), eemcFourMomentumList.end());
  }

}

FourList StBET4pMakerImp::constructFourMomentumListFrom(const TrackList& trackList)
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


