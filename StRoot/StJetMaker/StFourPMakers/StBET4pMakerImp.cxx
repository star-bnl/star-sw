// $Id: StBET4pMakerImp.cxx,v 1.51 2008/06/10 00:59:09 tai Exp $

#include "StBET4pMakerImp.h"

//STAR

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

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackEmuFactory.h"

#include "StMuEmcPosition.h"

#include <iostream>

using namespace std;
using namespace StSpinJet;

const int StBET4pMakerImp::mNOfBemcTowers;

StBET4pMakerImp::StBET4pMakerImp(StMuDstMaker* uDstMaker,  StBemcTables* bemcTables)
  : mUseEndcap(false)
  , mMuDstMaker(uDstMaker)
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
  , mEeGeom(0)
  , mEeDb(0)
  , _collectChargedTracksFromTPC(new CollectChargedTracksFromTPC(uDstMaker))
  , _collectEnergyDepositsFromBEMC(new CollectEnergyDepositsFromBEMC(uDstMaker, bemcTables))
{
  cout <<"StBET4pMakerImp::StBET4pMakerImp()"<<endl;
  assert(mMuDstMaker);
}

void StBET4pMakerImp::setUse2003Cuts(bool v)
{ 
  _collectEnergyDepositsFromBEMC->setUse2003Cuts(v);
}

void StBET4pMakerImp::setUse2005Cuts(bool v)
{ 
  _collectEnergyDepositsFromBEMC->setUse2005Cuts(v);
}

void StBET4pMakerImp::setUse2006Cuts(bool v)
{
  _collectChargedTracksFromTPC->setUse2006Cuts(v);
}

void StBET4pMakerImp::Init(StEEmcDbMaker* eedb)
{
    
  mEeGeom = new EEmcGeomSimple();
  mEeDb = eedb;
  mEeDb->setThreshold(3);
  
}

void StBET4pMakerImp::Clear(Option_t* opt)
{
  mDylanPoints = 0;
  mSumEmcEt = 0.;
    
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

  mSumEmcEt = sumEnergyOverBemcTowers(0.4, bemcEnergyDepositList);

  mDylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, bemcEnergyDepositList);

  if (mSumEmcEt > 200.) return;


  TowerEnergyDepositList bemcCorrectedEnergyDepositList = correctBemcTowerEnergyForTracks(bemcEnergyDepositList, trackList);

  FourList bemcFourMomentumList = constructFourMomentumListFrom(bemcCorrectedEnergyDepositList);


  _tracks.insert(_tracks.end(), tpcFourMomentumList.begin(), tpcFourMomentumList.end());
  _tracks.insert(_tracks.end(), bemcFourMomentumList.begin(), bemcFourMomentumList.end());

  if (!mUseEndcap) return;

  TowerEnergyDepositList eemcCorrectedEnergyDepositList = collectEnergyFromEEMC();

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

double StBET4pMakerImp::sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList)
{
  double ret(0.0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret += (*it).energy;

  return ret;
}

int StBET4pMakerImp::numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList)
{
  int ret(0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret ++;

  return ret;
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


StSpinJet::TowerEnergyDepositList StBET4pMakerImp::collectEnergyFromEEMC()
{
  StMuEmcCollection* muEmc = mMuDstMaker->muDst()->muEmcCollection();

  TowerEnergyDepositList energyDepositList;

  for (int id = 0; id < muEmc->getNEndcapTowerADC(); ++id) {

    int rawadc, sec, sub, etabin;
    muEmc->getEndcapTowerADC(id, rawadc, sec, sub, etabin);
    assert(sec >0 && sec <= MaxSectors);
	
    const EEmcDbItem *dbItem = mEeDb->getT(sec,sub-1+'A',etabin);
    assert(dbItem); 
	
    if(dbItem->fail) continue; //drop broken channels
    if(dbItem->stat) continue; // drop not working channels and jumpy pedestal channels
    if(dbItem->gain<=0.) continue; // drop it, unless you work with ADC spectra
    if(rawadc<dbItem->thr) continue; // drop raw ADC < ped+N*sigPed, N==3 in init
	    
    double adc = rawadc - (dbItem->ped);
    double energy = adc/(dbItem->gain);
    if(energy < 0.01) continue; // drop if less than 10MeV for now
	    
    TowerEnergyDeposit energyDeposit;
    energyDeposit.detectorId = kEndcapEmcTowerId;
    energyDeposit.towerId = (sec*5 + sub)*12 + etabin;
    energyDeposit.towerLocation = mEeGeom->getTowerCenter(sec-1,sub-1,etabin-1);
    energyDeposit.energy = energy;

    energyDepositList.push_back(energyDeposit);

  }

  return energyDepositList;

}
