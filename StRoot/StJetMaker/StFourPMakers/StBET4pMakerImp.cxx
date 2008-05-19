
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
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
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

#include <TLorentzVector.h>

#include <string>
#include <iostream>
#include <cmath>
#include <sys/times.h>

using namespace std;

const int StBET4pMakerImp::mNOfBemcTowers;

StBET4pMakerImp::StBET4pMakerImp(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : eta_high_lim(2.0)
  , eta_low_lim(-2.0)
  , mCorrupt(false)
  , mUseEndcap(false)
  , mMuDstMaker(uDstMaker)
  , mTables(new StBemcTables(doTowerSwapFix))
  , mUse2003Cuts(false)
  , mUse2005Cuts(false)
  , mUse2006Cuts(false)
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
  , mEeGeom(0)
  , mEeDb(0)
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

void StBET4pMakerImp::Init(StEEmcDbMaker* eedb, StEmcADCtoEMaker* adc2e)
{
  cout <<"StBET4pMakerImp::Init()"<<endl;
    
  mEeGeom = new EEmcGeomSimple();
  //  mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
  mEeDb = eedb;
  assert(mEeDb); // eemcDB must be in the chain, fix it
  mEeDb->setThreshold(3);
  
  _adc2e = adc2e;
}

void StBET4pMakerImp::Clear(Option_t* opt)
{
  LOG_DEBUG <<"void StBET4pMakerImp::Clear(Option_t* opt)" << endm;
  mCorrupt = false;
  mDylanPoints = 0;
  mSumEmcEt = 0.;
    
  for (FourList::iterator it = tracks.begin(); it != tracks.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  tracks.clear();

  //reset pointers to Barrel hits
  for (int i = 1; i <= mNOfBemcTowers; ++i) {
    mBTowHits[i] = 0;
    mNtracksOnTower[i] = 0;
  }
    
}

void StBET4pMakerImp::Make(StEvent* event)
{
  fillBemcTowerHits(event);

  mSumEmcEt = sumEnergyOverBemcTowers(0.4);

  mDylanPoints = numberOfBemcTowersWithEnergyAbove(0.4);

  //check for barrel corruption (only works for P04ik and later!
  if (mCorrupt) {
    tracks.clear();
    return;
  }

  if (mSumEmcEt > 200.) {
    tracks.clear();
    return;
  }

  collectChargedTracksFromTPC();
  collectEnergyFromBEMC();
  collectEnergyFromEEMC();

}

void StBET4pMakerImp::collectChargedTracksFromTPC()
{
  StMuDst* uDst = mMuDstMaker->muDst();

  //next, loop on tracks and add to the 4p list:
  long nTracks = uDst->numberOfPrimaryTracks();

  for(int i = 0; i < nTracks; ++i) {
    StMuTrack* track = uDst->primaryTracks(i);
    assert(track);

    if (!isUsableTrack(*track)) continue;

    countTracksOnBemcTower(*track);

    //construct four momentum
    StThreeVectorF momentum = track->momentum();
    double mass = 0.1395700; //assume pion+ mass for now
    float energy = sqrt(mass*mass + momentum.mag()*momentum.mag());
    //    StLorentzVectorF p4(energy, momentum);

    TLorentzVector p4(momentum.x(), momentum.y(), momentum.z(), energy);

    //now construct StMuTrackFourVec object for jetfinding
    StSpinJet::StMuTrackEmuFactory factory;
    StMuTrackFourVec* pmu = new StMuTrackFourVec(factory.createStMuTrackEmu(track), p4, track->charge(), i, kTpcId);
    tracks.push_back(pmu); //this is for expected interface to StJetMaker --> StppJetAnalyzer
  }
}

bool StBET4pMakerImp::isUsableTrack(const StMuTrack& track) const
{
    if(track.flag() < 0) 
      return false;

    if (track.dcaGlobal().mag() > 3.)
      return false;
      
    int dcaFlag=1;
    if (mUse2006Cuts){
      Double_t limit=3.-2.*track.pt();
      if(!((track.pt()<0.5&&track.dcaGlobal().mag()<=2.) ||
	   ((track.pt()>=0.5&&track.pt()<1.0)&&
	    track.dcaGlobal().mag()<=limit) ||
	   (track.pt()>=1.0&&track.dcaGlobal().mag()<=1.0))) dcaFlag=0;
    }
    if(dcaFlag == 0)
      return false;

    if (track.topologyMap().trackFtpcEast() || track.topologyMap().trackFtpcWest())
      return false;

    if(track.eta() < GetEtaLow())
      return false;

    if(track.eta() > GetEtaHigh())
      return false;

    if(static_cast<double>(track.nHits())/static_cast<double>(track.nHitsPoss()) < .51)
      return false;

  return true;
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
  StMuDst* uDst = mMuDstMaker->muDst();

  //now loop on Barrel hits, correct energy, and push back for jet finding:
  for (int bemcTowerId = 1; bemcTowerId <= mNOfBemcTowers; ++bemcTowerId) { //bemcTowerId==software bemcTowerId: [1,4800]
    StEmcRawHit* hit = mBTowHits[bemcTowerId];
    if (!hit) continue; //either no hit here or status!=1
	
    float energy = hit->energy();
    if(energy <= 0.) continue; //skip it, E=0 can happen from gain=0. in calib table

    double corrected_energy = correctBemcTowerEnergyForTracks(energy, bemcTowerId);
    
    if (corrected_energy <= 0.) continue;

    float eta, phi;
    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    geom->getEtaPhi(bemcTowerId,eta,phi); // to convert software bemcTowerId into eta/phi

    double mass = 0.; //assume photon mass for now, that makes more sense for towers, I think.

    double pMag = (corrected_energy > mass) ? sqrt(corrected_energy*corrected_energy - mass*mass) : corrected_energy;

    //now correct for eta-shift due to non-zero z_vertex (but note, no correction to Energy!)
    //double RSMD = 2.2625*100.; //radius of SMD in cm
    float towerX, towerY, towerZ;
    geom->getXYZ(bemcTowerId, towerX, towerY, towerZ);
    StThreeVectorF towerLocation(towerX, towerY, towerZ);

    StThreeVectorF vertex = uDst->event()->primaryVertexPosition();
    towerLocation -= vertex; //shift the origin to the vertex, not (0., 0., 0.)
	    
    StThreeVectorF momentum(1., 1., 1.);
    momentum.setPhi(phi);
    momentum.setTheta(towerLocation.theta()); //use corrected theta
    momentum.setMag(pMag);
    //    StLorentzVectorF p4(corrected_energy, momentum);
    TLorentzVector p4(momentum.x(), momentum.y(), momentum.z(), corrected_energy);
	    
    //now construct StMuTrackFourVec object for jetfinding
    StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, bemcTowerId, kBarrelEmcTowerId);
    tracks.push_back(pmu); //for jet finding interface
  }
}

double StBET4pMakerImp::correctBemcTowerEnergyForTracks(double energy, int bemcTowerId)
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
      StMuTrackFourVec* pmu = new StMuTrackFourVec(0, p4, 0, id, kEndcapEmcTowerId);
      tracks.push_back(pmu); //for jet finding interface
    }
  }
}

StEmcCollection *StBET4pMakerImp::find_StEmCCollection(StEvent* event) {

  StEmcCollection* emc(0);

  //new loop on data:
  /*first look for StEvent in memory.  This should only happen if
    (a) StEmcADC2EMaker is running (e.g., year 2003 and before)
    (b) StEmcSimulatorMaker is running (all pythia!).  In this case simulator recalculates ADC from
    scratch using DB gains, so we _don't_ take what's in the MuDst collection
  */

  //  StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (event) {
    LOG_DEBUG <<"StBET4pMakerImp::Make()\tRetrieve StEmcCollection from StEvent"<<endm;
    emc = event->emcCollection();
  } else {
    LOG_DEBUG <<"StBET4pMakerImp::Make()\tRetrieve StEmcCollection from MuDst"<<endm;
    emc = mMuDstMaker->muDst()->emcCollection();
  }
  assert(emc);

  return emc;
}

bool StBET4pMakerImp::isCorrupted(StEvent* event)
{
  StEmcCollection* emc = find_StEmCCollection(event);
  StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);

  //if detector==null, this means it's corrupt for pre-October 2004 BEMC code.  However, not all corrupt events give detector==0
  if (!detector) {
    return true;
  }
    
  //now, check for corruption in post-October 2004 BEMC code (P04k and later)
  //cout <<"StEmcTpcFourPMaker::Make()\tcheck  crate corruption with key: crateUnknown=0, crateNotPresent=1, crateOK=2, crateHeaderCorrupt=3"<<endl;
  for(int crate = 1; crate<=MAXCRATES; crate++) {
    StEmcCrateStatus crateStatus = detector->crateStatus(crate);
    //cout <<"crate:\t"<<crate<<"\tstauts:\t"<<crateStatus<<endl;
    if (crateStatus==crateHeaderCorrupt) {
      return true;
    }
  }

  //And now we can implement Alex's new StEmcAdc2EMaker test (thank god, this takes care of pre-P04k production)
  //  StEmcADCtoEMaker* adc2e = (StEmcADCtoEMaker*)GetMaker("Eread");
  if (!_adc2e) {
    LOG_ERROR <<"StBET4pMakerImp::fillBarrelHits()\tno adc2e in chain"<<endm;
  } else {
    LOG_DEBUG <<"StBET4pMakerImp::fillBarrelHits()\tfound adc2e in chain"<<endm;
    if (_adc2e->isCorrupted()) {
      return true;
    }
  }

  return false;
}


void StBET4pMakerImp::fillBemcTowerHits(StEvent* event)
{
  mCorrupt = isCorrupted(event);
  if(mCorrupt) return;

  StEmcCollection* emc = find_StEmCCollection(event);
  StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);

  static const int nBemcModules = 120;
  for(int m = 1; m <= nBemcModules; ++m) { //loop on modules...
    StEmcModule* module = detector->module(m);
    assert(module);
  	
    StSPtrVecEmcRawHit& rawHits = module->hits();
    for(UInt_t k = 0; k < rawHits.size(); ++k) { //loop on hits in modules
      StEmcRawHit* theRawHit = rawHits[k];
  	    
      StEmcGeom* geom = StEmcGeom::instance("bemc"); 
      int bemcTowerID;
      geom->getId(theRawHit->module(), theRawHit->eta(), abs(theRawHit->sub()),bemcTowerID); // to get the software id
  
      if (shouldKeepThisBemcHit(theRawHit, bemcTowerID)) 
	mBTowHits[bemcTowerID] = theRawHit;
      else
	mBTowHits[bemcTowerID] = 0;

    }
  }
  

}

bool StBET4pMakerImp::shouldKeepThisBemcHit(StEmcRawHit* theRawHit, int bemcTowerID)
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


double StBET4pMakerImp::sumEnergyOverBemcTowers(double minE)
{
  double ret(0.0);
  for(int bemcTowerID = 1; bemcTowerID <= mNOfBemcTowers; ++bemcTowerID) {
    if(mBTowHits[bemcTowerID] && mBTowHits[bemcTowerID]->energy() > minE) {
      ret += mBTowHits[bemcTowerID]->energy();
    }
  }
  return ret;
}

int StBET4pMakerImp::numberOfBemcTowersWithEnergyAbove(double minE)
{
  int ret(0);
  for(int bemcTowerID = 1; bemcTowerID <= mNOfBemcTowers; ++bemcTowerID) {
    if(mBTowHits[bemcTowerID] && mBTowHits[bemcTowerID]->energy() > 0.4) {
      ret++;
    }
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
