// $Id: StBET4pMakerImp.cxx,v 1.70 2008/07/08 02:14:50 tai Exp $

#include "StBET4pMakerImp.h"

#include "CollectEnergyDepositsFromBEMC.h"


//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//StEmc
#include "StEmcUtil/geometry/StEmcGeom.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"

#include "StMuEmcPosition.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

StBET4pMakerImp::StBET4pMakerImp(StMuDstMaker* uDstMaker,
				 CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
				 CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC,
				 CollectEnergyDepositsFromEEMC *collectEnergyDepositsFromEEMC,
				 CorrectTowerEnergyForTracks* correctTowerEnergyForTracks
				 )
  : mUseEndcap(false)
  , mUseBEMC(true)
  , mMuDstMaker(uDstMaker)
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

    TLorentzVector p4 = constructFourMomentum((*it).towerLocation, (*it).energy);
	    
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, (*it).towerId, (*it).detectorId);

    ret.push_back(pmu);
  }
  return ret;
}

TLorentzVector StBET4pMakerImp::constructFourMomentum(const TVector3& towerLocation, double energy)
{
  TVector3 momentum = towerLocation - getVertex();

  double mass(0); // assume photon mass

  double pMag = (energy > mass) ? sqrt(energy*energy - mass*mass) : energy;

  momentum.SetMag(pMag);

  return TLorentzVector(momentum.x(), momentum.y(), momentum.z(), energy);
}

TVector3 StBET4pMakerImp::getVertex()
{
  StThreeVectorF vertex = mMuDstMaker->muDst()->event()->primaryVertexPosition();

  return TVector3(vertex.x(), vertex.y(), vertex.z());
}


