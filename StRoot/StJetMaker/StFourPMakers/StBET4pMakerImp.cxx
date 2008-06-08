
#include "StBET4pMakerImp.h"

//STAR
#include "SystemOfUnits.h"
#include "StMessMgr.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"

//StEvent
#include "StEventTypes.h"

//StEmc
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcTables.h"

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//StJetMaker
#include "../StMuTrackFourVec.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackEmuFactory.h"

#include "StMuEmcPosition.h"

#include <vector>
#include <string>
#include <iostream>
#include <cmath>
#include <sys/times.h>

using namespace std;
using namespace StSpinJet;

const int StBET4pMakerImp::mNOfBemcTowers;

StBET4pMakerImp::StBET4pMakerImp(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : mUseEndcap(false)
  , mMuDstMaker(uDstMaker)
  , mTables(new StBemcTables(doTowerSwapFix))
  , mUse2003Cuts(false)
  , mUse2005Cuts(false)
  , mUse2006Cuts(false)
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
  , mEeGeom(0)
  , mEeDb(0)
  , _collectChargedTracksFromTPC(new CollectChargedTracksFromTPC(uDstMaker))
{
  cout <<"StBET4pMakerImp::StBET4pMakerImp()"<<endl;
  assert(mMuDstMaker);
}

Int_t StBET4pMakerImp::InitRun(Int_t runId, StBemcTables* tables)
{
  //  mTables->loadTables((StMaker*)this);
  mTables = tables;

  return kStOk;
    
}

void StBET4pMakerImp::setUse2006Cuts(bool v)
{
  mUse2006Cuts = v;
  _collectChargedTracksFromTPC->setUse2006Cuts(v);
}

void StBET4pMakerImp::Init(StEEmcDbMaker* eedb)
{
  cout <<"StBET4pMakerImp::Init()"<<endl;
    
  mEeGeom = new EEmcGeomSimple();
  //  mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
  mEeDb = eedb;
  assert(mEeDb); // eemcDB must be in the chain, fix it
  mEeDb->setThreshold(3);
  
}

void StBET4pMakerImp::Clear(Option_t* opt)
{
  LOG_DEBUG <<"void StBET4pMakerImp::Clear(Option_t* opt)" << endm;
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

  _bemcTowerHits.clear();
    
}

void StBET4pMakerImp::Make()
{
  TrackList trackList = _collectChargedTracksFromTPC->Do();

  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    const StMuTrack* track = (*it).first;
    countTracksOnBemcTower(*track);
  }

  FourList fourPList = constructFourMomentumListFrom(trackList);

  _tracks.insert(_tracks.end(), fourPList.begin(), fourPList.end());

  BemcTowerIdHitMap allBemcTowerHits = getTowerHitsFromBEMC();

  BemcTowerIdHitMap selectedBemcTowerHits = selectBemcTowerHits(allBemcTowerHits);

  mSumEmcEt = sumEnergyOverBemcTowers(0.4, selectedBemcTowerHits);

  mDylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, selectedBemcTowerHits);

  if (mSumEmcEt > 200.) {
    _tracks.clear();
    return;
  }

  for(BemcTowerIdHitMap::const_iterator it = selectedBemcTowerHits.begin(); it != selectedBemcTowerHits.end(); ++it)
    _bemcTowerHits.insert(*it);

  collectEnergyFromBEMC();
  collectEnergyFromEEMC();

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

void StBET4pMakerImp::countTracksOnBemcTower(const StMuTrack& track)
{
  StMuDst* uDst = mMuDstMaker->muDst();

  //check projection to BEMC and remember for later: ---------------------------------------------
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
      mNtracksOnTower[id]++; //increment number of tracks on this tower
    }
  }
}

void StBET4pMakerImp::collectEnergyFromBEMC()
{
  BemcTowerIdEnergyMap bemcEnergy = readBemcTowerEnergy(_bemcTowerHits);

  map<BemcTowerID, double> bemcCorrectedEnergy = correctBemcTowerEnergyForTracks(bemcEnergy);

  for(map<BemcTowerID, double>::iterator it = bemcCorrectedEnergy.begin(); it != bemcCorrectedEnergy.end(); ++it) {

    const int bemcTowerId = (*it).first;
    const double corrected_energy = (*it).second;

    TLorentzVector p4 = constructBemcFourMomentum(bemcTowerId, corrected_energy);
	    
    //now construct StMuTrackFourVec object for jetfinding
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, bemcTowerId, kBarrelEmcTowerId);
    _tracks.push_back(pmu); //for jet finding interface
  }
}

StBET4pMakerImp::BemcTowerIdEnergyMap StBET4pMakerImp::readBemcTowerEnergy(const BemcTowerIdHitMap &bemcTowerHits)
{
  BemcTowerIdEnergyMap ret;

  for(BemcTowerIdHitMap::const_iterator it = bemcTowerHits.begin(); it != bemcTowerHits.end(); ++it) {

    const int bemcTowerId = (*it).first;
    const StEmcRawHit* hit = (*it).second;

    if(hit->energy() <= 0) continue;

    ret[bemcTowerId] = hit->energy();
  }

  return ret;
}

StBET4pMakerImp::BemcTowerIdEnergyMap StBET4pMakerImp::correctBemcTowerEnergyForTracks(const BemcTowerIdEnergyMap &bemcEnergy)
{
  BemcTowerIdEnergyMap ret;

  for(BemcTowerIdEnergyMap::const_iterator it = bemcEnergy.begin(); it != bemcEnergy.end(); ++it) {

    const int bemcTowerId = (*it).first;
    const double energy = (*it).second;

    double corrected_energy = correctBemcTowerEnergyForTracks_(energy, bemcTowerId);
    
    if(corrected_energy <= 0) continue;

    ret[bemcTowerId] = corrected_energy;

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

TLorentzVector StBET4pMakerImp::constructBemcFourMomentum(int bemcTowerId, double energy)
{
    TVector3 towerLocation = getBemcTowerLocation(bemcTowerId);

    TVector3 momentum = towerLocation - getVertex();

    double mass(0); // assume photon mass

    double pMag = (energy > mass) ? sqrt(energy*energy - mass*mass) : energy;

    momentum.SetMag(pMag);

    return TLorentzVector(momentum.x(), momentum.y(), momentum.z(), energy);
}

TVector3 StBET4pMakerImp::getBemcTowerLocation(int bemcTowerId)
{
  StEmcGeom* geom = StEmcGeom::instance("bemc");

  float towerX, towerY, towerZ;

  geom->getXYZ(bemcTowerId, towerX, towerY, towerZ);

  return TVector3(towerX, towerY, towerZ);
}

TVector3 StBET4pMakerImp::getVertex()
{
  StMuDst* uDst = mMuDstMaker->muDst();
  StThreeVectorF vertex = uDst->event()->primaryVertexPosition();

  return TVector3(vertex.x(), vertex.y(), vertex.z());
}

void StBET4pMakerImp::collectEnergyFromEEMC()
{
  StMuDst* uDst = mMuDstMaker->muDst();

  if (mUseEndcap) {
	
    // Now add endcap points --------------------------
    StMuEmcCollection* muEmc = uDst->muEmcCollection();
    assert(muEmc);

    for (int id = 0; id < muEmc->getNEndcapTowerADC(); ++id) {

      int rawadc, sec, sub, etabin;
      muEmc->getEndcapTowerADC(id, rawadc, sec, sub, etabin);
      assert(sec >0 && sec <= MaxSectors);
	
      //find eta and phi values from sector, subsector and etabin assuming z=0,0,0
      TVector3 towerCenter = mEeGeom->getTowerCenter(sec-1,sub-1,etabin-1); //careful, this is indexed from 0
	
      const EEmcDbItem *dbItem = mEeDb->getT(sec,sub-1+'A',etabin);
      assert(dbItem); 
	
      if(dbItem->fail) continue; //drop broken channels
      if(dbItem->stat) continue; // drop not working channels and jumpy pedestal channels
      if(dbItem->gain<=0.) continue; // drop it, unless you work with ADC spectra
      if(rawadc<dbItem->thr) continue; // drop raw ADC < ped+N*sigPed, N==3 in init
	    
      double adc = rawadc - (dbItem->ped);
      double energy = adc/(dbItem->gain);
      if(energy < 0.01) continue; // drop if less than 10MeV for now
	    
      //construct four momentum
      double mass = 0.; //assume photon mass for now, that makes more sense for towers, I think.
      double pMag = (energy > mass) ? sqrt(energy*energy - mass*mass) : energy; //NOTE: this is a little naive treatment!

      //correct for eta shift
      StThreeVectorD towerLocation(towerCenter.X(), towerCenter.Y(), towerCenter.Z());
      StThreeVectorF vertex = uDst->event()->primaryVertexPosition();
      towerLocation -= vertex; //shift the origin to the vertex, not (0., 0., 0.)

      //construct momentum 3-vector
      StThreeVectorF momentum(1., 1., 1.);
      momentum.setPhi( towerCenter.Phi() );
      momentum.setTheta( towerLocation.theta() ); //use theta from vertex subtracted point.
      momentum.setMag(pMag);
      //      StLorentzVectorF p4(energy, momentum);
      TLorentzVector p4(momentum.x(), momentum.y(), momentum.z(), energy);
	    
      //now construct StMuTrackFourVec object for jetfinding
      int towerID= (sec*5 + sub)*12 + etabin;
      StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, towerID, kEndcapEmcTowerId);
      _tracks.push_back(pmu); //for jet finding interface
    }
  }
}

StBET4pMakerImp::BemcTowerIdHitMap StBET4pMakerImp::getTowerHitsFromBEMC()
{
  BemcTowerIdHitMap ret;

  StEmcCollection* emc = mMuDstMaker->muDst()->emcCollection();
  StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);

  static const int nBemcModules = 120;
  for(int m = 1; m <= nBemcModules; ++m) { //loop on modules...
    StEmcModule* module = detector->module(m);
  	
    StSPtrVecEmcRawHit& rawHits = module->hits();
    for(UInt_t k = 0; k < rawHits.size(); ++k) { //loop on hits in modules
      StEmcRawHit* theRawHit = rawHits[k];
  	    
      StEmcGeom* geom = StEmcGeom::instance("bemc"); 
      int bemcTowerID;
      geom->getId(theRawHit->module(), theRawHit->eta(), abs(theRawHit->sub()),bemcTowerID); // to get the software id

      ret[bemcTowerID] = theRawHit;
    }
  }
  return ret;
}

StBET4pMakerImp::BemcTowerIdHitMap StBET4pMakerImp::selectBemcTowerHits(const BemcTowerIdHitMap &bemcTowerHits)
{
  BemcTowerIdHitMap ret;

  for(map<BemcTowerID, const StEmcRawHit*>::const_iterator it = bemcTowerHits.begin(); it != bemcTowerHits.end(); ++it) {

    if (!shouldKeepThisBemcHit((*it).second, (*it).first))
      continue;

    ret.insert(*it);
  }
  return ret;
}

bool StBET4pMakerImp::shouldKeepThisBemcHit(const StEmcRawHit* theRawHit, int bemcTowerID)
{
  //now check the status: (//BTOW defined in StEmcRawMaker/defines.h
  int status;
  mTables->getStatus(BTOW, bemcTowerID, status);
  
  //check for ADC that is 2-sigma above RMS:
  float pedestal, rms;
  int CAP(0); //this arument matters only for SMD
  mTables->getPedestal(BTOW, bemcTowerID, CAP, pedestal, rms);
  
  int ADC = theRawHit->adc(); //not pedestal subtracted!

  if (mUse2003Cuts)
    if (ADC-pedestal>0 && (ADC-pedestal)>2.*rms && status==1 && accept2003Tower(bemcTowerID) )
      return true;
    else
      return false;
  else if (mUse2005Cuts)
    if (ADC-pedestal>0 && (ADC-pedestal)>2.*rms && status==1 && bemcTowerID <= 2400)
      return true;
    else
      return false;
  else
    if (ADC-pedestal>0 && (ADC-pedestal)>2.*rms && status==1)
      return true;
    else
      return false;
}


double StBET4pMakerImp::sumEnergyOverBemcTowers(double minE, const BemcTowerIdHitMap& bemcTowerHits)
{
  double ret(0.0);
  for(BemcTowerIdHitMap::const_iterator it = bemcTowerHits.begin(); it != bemcTowerHits.end(); ++it) {
    if((*it).second->energy() > minE)
      ret += (*it).second->energy();
  }
  return ret;
}

int StBET4pMakerImp::numberOfBemcTowersWithEnergyAbove(double minE, const BemcTowerIdHitMap& bemcTowerHits)
{
  int ret(0);
  for(BemcTowerIdHitMap::const_iterator it = bemcTowerHits.begin(); it != bemcTowerHits.end(); ++it) {
    if((*it).second->energy() > minE)
      ret++;
  }
  return ret;
}

bool StBET4pMakerImp::accept2003Tower(int id)
{
    if( id==555
	|| id==615
	|| id==656
	|| id==772
	|| id==1046
	|| id==1048
	|| id==1408
	|| id==1555
	|| id==1750
	|| id==1773
	|| id==2073
	|| id==2093
	|| id==2096
	|| (id>=1866 && id<=1894)
	|| id==511
	|| id==1614
	|| id==1615
	|| id==1616
	|| id==1636
	|| id==1899
	|| id==2127
	|| id==953
	|| id==1418
	|| id==1419
	|| id==1878
	|| id==1879
	|| id==1881
	|| (id>=1042 && id<=1045)
	|| (id>=1385 && id<=1387)
	|| (id>=1705 && id<=1708)
	|| (id>=1725 && id<=1728)
	|| (id>=1745 && id<=1748)
	|| (id>=1765 && id<=1768)
	|| (id>=1785 && id<=1788))
	{
	    cout <<"rejecting tower:\t"<<id<<endl;
	    return false;
	}
    else {
	return true;
    }
}
