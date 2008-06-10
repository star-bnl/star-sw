// $Id: StBET4pMakerImp.cxx,v 1.58 2008/06/10 08:31:08 tai Exp $

#include "StBET4pMakerImp.h"

#include "CollectEnergyDepositsFromBEMC.h"


//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//StEvent
#include "StEventTypes.h"

////StEmc
#include "StEmcUtil/geometry/StEmcGeom.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackEmuFactory.h"

#include "StMuEmcPosition.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

const int StBET4pMakerImp::mNOfBemcTowers;

StBET4pMakerImp::StBET4pMakerImp(StMuDstMaker* uDstMaker,
				 CollectChargedTracksFromTPC* collectChargedTracksFromTPC,
				 CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC,
				 CollectEnergyDepositsFromEEMC *collectEnergyDepositsFromEEMC
				 )
  : mUseEndcap(false)
  , mMuDstMaker(uDstMaker)
  , _collectChargedTracksFromTPC(collectChargedTracksFromTPC)
  , _collectEnergyDepositsFromBEMC(collectEnergyDepositsFromBEMC)
  , _collectEnergyDepositsFromEEMC(collectEnergyDepositsFromEEMC)
{

}

void StBET4pMakerImp::Clear(Option_t* opt)
{
    
  for (FourList::iterator it = _tracks.begin(); it != _tracks.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  _tracks.clear();

  //reset pointers to Barrel hits
  for (int i = 1; i <= mNOfBemcTowers; ++i) {
    mNtracksOnTower[i] = 0;
  }

}

void StBET4pMakerImp::Make()
{
  TrackList trackList = _collectChargedTracksFromTPC->Do();

  FourList tpcFourMomentumList = constructFourMomentumListFrom(trackList);

  TowerEnergyDepositList bemcEnergyDepositList = _collectEnergyDepositsFromBEMC->Do();

  TowerEnergyDepositList bemcCorrectedEnergyDepositList = correctBemcTowerEnergyForTracks(bemcEnergyDepositList, trackList);

  FourList bemcFourMomentumList = constructFourMomentumListFrom(bemcCorrectedEnergyDepositList);


  _tracks.insert(_tracks.end(), tpcFourMomentumList.begin(), tpcFourMomentumList.end());
  _tracks.insert(_tracks.end(), bemcFourMomentumList.begin(), bemcFourMomentumList.end());

  if (!mUseEndcap) return;

  TowerEnergyDepositList eemcCorrectedEnergyDepositList = _collectEnergyDepositsFromEEMC->Do();

  FourList eemcFourMomentumList = constructFourMomentumListFrom(eemcCorrectedEnergyDepositList);

  _tracks.insert(_tracks.end(), eemcFourMomentumList.begin(), eemcFourMomentumList.end());

}

FourList StBET4pMakerImp::constructFourMomentumListFrom(const TrackList& trackList)
{
  FourList ret;

  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    const StMuTrack* track = (*it).first;

    StThreeVectorF momentum = track->momentum();
    double mass = 0.1395700; //assume pion+ mass for now
    float energy = sqrt(mass*mass + momentum.mag()*momentum.mag());

    TLorentzVector p4(momentum.x(), momentum.y(), momentum.z(), energy);

    StSpinJet::StMuTrackEmuFactory factory;
    StMuTrackFourVec* pmu = new StMuTrackFourVec(factory.createStMuTrackEmu(track), p4, track->charge(), (*it).second, kTpcId);
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

void StBET4pMakerImp::countTracksOnBemcTower(const StMuTrack& track)
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

StSpinJet::TowerEnergyDepositList StBET4pMakerImp::correctBemcTowerEnergyForTracks(const TowerEnergyDepositList &energyDepositList, const TrackList& trackList)
{
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


double StBET4pMakerImp::correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId)
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
  StMuDst* uDst = mMuDstMaker->muDst();
  StThreeVectorF vertex = uDst->event()->primaryVertexPosition();

  return TVector3(vertex.x(), vertex.y(), vertex.z());
}


