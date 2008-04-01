
#include "StBET4pMaker.h"

//STAR
#include "SystemOfUnits.h"
#include "StMessMgr.h"

//StMuDstMaker
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
#include "../StJetHist/StJetHistMaker.h"

//local subdirectory
#include "StMuEmcPosition.h"

#include <string>
#include <iostream>
#include <cmath>
#include <sys/times.h>

using namespace std;

bool accept2003Tower(int id);

ClassImp(StBET4pMaker)
    
    
StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
  : StFourPMaker(name, 0)
  , mCorrupt(false)
  , mUseEndcap(false)
  , mField(0.0)
  , mMuPosition(new StMuEmcPosition())
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
  cout <<"StBET4pMaker::StBET4pMaker()"<<endl;
  assert(mMuDstMaker);
}

Int_t StBET4pMaker::InitRun(Int_t runId)
{
  cout <<"Welcome to HistMaker::InitRun()"<<endl;
  mTables->loadTables((StMaker*)this);

  return kStOk;
    
}

Int_t StBET4pMaker::Init()
{
  cout <<"StBET4pMaker::Init()"<<endl;
    
  mEeGeom = new EEmcGeomSimple();
  mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
  assert(mEeDb); // eemcDB must be in the chain, fix it
  mEeDb->setThreshold(3);
    
  return StMaker::Init();
}

void StBET4pMaker::Clear(Option_t* opt)
{
  LOG_DEBUG <<"void StBET4pMaker::Clear(Option_t* opt)" << endm;
  mCorrupt = false;
  mDylanPoints = 0;
  mSumEmcEt = 0.;
  mField = 0.;
    
  for (FourList::iterator it = tracks.begin(); it != tracks.end(); ++it) {
    delete (*it);
    (*it) = 0;
  }
  tracks.clear();

  //reset pointers to Barrel hits
  for (int i = 1; i < 4801; ++i) {
    mBTowHits[i] = 0;
    mNtracksOnTower[i] = 0;
  }
    
  return StMaker::Clear(opt);
}

Int_t StBET4pMaker::Make()
{
  LOG_DEBUG <<"StBET4pMaker::Make()"<<endm;

  fillBemcTowerHits();

  mSumEmcEt = sumEnergyOverBemcTowers(0.4);

  mDylanPoints = numberOfBemcTowersWithEnergyAbove(0.4);

  //check for barrel corruption (only works for P04ik and later!
  if (mCorrupt) {
    tracks.clear();
    LOG_DEBUG <<"StEmcTpcFourPMaker::Maker():\tFlag this as a corrupt event.  Clear 4-p container and return"<<endm;
    return kStOk;
  }

  if (mSumEmcEt > 200.) {
    tracks.clear();
    LOG_DEBUG <<"StEmcTpcFourPMaker::Maker():\ttoo much energy:\t"<<mSumEmcEt<<"\tflag as corrupt. Clear 4-p container and return"<<endm;
    return kStOk;
  }

  collectChargedTracksFromTPC();
  collectEnergyFromBEMC();
  collectEnergyFromEEMC();

  LOG_DEBUG <<StMaker::GetName()<<"::Make()\tAdded:\t"<<tracks.size()<<"\tparticles to track container"<<endm;

  return StMaker::Make();
}

void StBET4pMaker::collectChargedTracksFromTPC()
{
  StMuDst* uDst = mMuDstMaker->muDst();

  //next, loop on tracks and add to the 4p list:
  long nTracks = uDst->numberOfPrimaryTracks();

  for(int i = 0; i < nTracks; ++i) {
    StMuTrack* track = uDst->primaryTracks(i);
    assert(track);
    if(track->flag() < 0) {
      continue;
    }
    //MLM 8/17/05 -- adapt to use with multiple vertices:
    if (track->dcaGlobal().mag() > 3.) continue;

    int dcaFlag=1;
    if (mUse2006Cuts){
      Double_t limit=3.-2.*track->pt();
      if(!((track->pt()<0.5&&track->dcaGlobal().mag()<=2.) ||
	   ((track->pt()>=0.5&&track->pt()<1.0)&&
	    track->dcaGlobal().mag()<=limit) ||
	   (track->pt()>=1.0&&track->dcaGlobal().mag()<=1.0))) dcaFlag=0;
    }
    if(dcaFlag==0) continue;

    if (track->topologyMap().trackFtpcEast() || track->topologyMap().trackFtpcWest()) {
      continue;
    }
    if(track->eta() < GetEtaLow()) { //GetEtaLow defined in StFourPMaker.cxx
      continue;
    }
    if(track->eta() > GetEtaHigh()) {
      continue;
    }
    if(static_cast<double>(track->nHits())/static_cast<double>(track->nHitsPoss()) < .51) {
      continue;
    }

    //check projection to BEMC and remember for later: ---------------------------------------------
    StThreeVectorD momentumAt, positionAt;
	
    mField = uDst->event()->magneticField()/10.0; //to put it in Tesla
    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    bool tok = mMuPosition->trackOnEmc(&positionAt, &momentumAt, track, mField, geom->Radius());
    if(tok) {
      int m,e,s,id=0;
      geom->getBin(positionAt.phi(), positionAt.pseudoRapidity(), m, e, s);
      int bad = geom->getId(m,e,s,id);
      if(bad == 0) {
	mNtracksOnTower[id]++; //increment number of tracks on this tower
      }
    }
	
    //construct four momentum
    StThreeVectorF momentum = track->momentum();
    double mass = 0.1395700; //assume pion+ mass for now
    float energy = sqrt(mass*mass + momentum.mag()*momentum.mag());
    StLorentzVectorF p4(energy, momentum);

    //now construct StMuTrackFourVec object for jetfinding
    StMuTrackFourVec* pmu = new StMuTrackFourVec();
    pmu->Init(track, p4, i, kTpcId );
    tracks.push_back(pmu); //this is for expected interface to StJetMaker --> StppJetAnalyzer
  }
}

void StBET4pMaker::collectEnergyFromBEMC()
{
  StMuDst* uDst = mMuDstMaker->muDst();

  //now loop on Barrel hits, correct energy, and push back for jet finding:
  for (int id = 1; id <= 4800; ++id) { //id==software id: [1,4800]
    StEmcRawHit* hit = mBTowHits[id];
    if (!hit) continue; //either no hit here or status!=1
	
    float energy = hit->energy();
    if(energy<=0.) continue; //skip it, E=0 can happen from gain=0. in calib table

    //Get eta, phi
    float eta, phi;
    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    geom->getEtaPhi(id,eta,phi); // to convert software id into eta/phi

    float pedestal, rms;
    int CAP=0; //this arument matters only for SMD (capacitor?)
    mTables->getPedestal(BTOW, id, CAP, pedestal, rms);
	    
    //construct four momentum
    double mass = 0.; //assume photon mass for now, that makes more sense for towers, I think.
	    
    float theta=2.*atan(exp(-eta));

    //do a quick correction for hadronic MIP eneryg deposition:
    double MipE = 0.261*(1.+0.056*eta*eta)/sin(theta); //GeV
    double nMipsOnTower = static_cast<double>(mNtracksOnTower[id]);
    double corrected_energy = energy - nMipsOnTower*MipE;
    //cout <<"Subtracting:\t"<<nMipsOnTower*MipE<<"\tfrom energy:\t"<<energy<<"\tfrom:\t"<<nMipsOnTower<<"\tmips"<<endl;
	    
    //quick, fill some histograms here:
    StJetHistMaker* histMaker = dynamic_cast<StJetHistMaker*>(GetMaker("StJetHistMaker"));
    if (histMaker!=0) {
      if (nMipsOnTower==1) {
	histMaker->mipHistVsEta->Fill(eta, hit->adc() - pedestal);
	histMaker->mipEvsEta->Fill( eta, hit->energy() );
      }
      histMaker->towerEvsId->Fill(id, hit->energy());
      histMaker->towerAdcvsId->Fill(id, hit->adc());
    }
	    
    if (corrected_energy<=0.) continue;

    //NOTE, MLM fix 11/07/05
    //double pMag = (energy>mass) ? sqrt(energy*energy - mass*mass) : energy; //NOTE: this is a little naive treatment!
    double pMag = (corrected_energy>mass) ? sqrt(corrected_energy*corrected_energy - mass*mass) : corrected_energy;

    //now correct for eta-shift due to non-zero z_vertex (but note, no correction to Energy!)
    //double RSMD = 2.2625*100.; //radius of SMD in cm
    float towerX, towerY, towerZ;
    geom->getXYZ(id, towerX, towerY, towerZ);
    StThreeVectorF towerLocation(towerX, towerY, towerZ);
    //cout <<"id:\t"<<id<<"\tlocation:\t"<<towerLocation<<"\teta:\t"<<towerLocation.pseudoRapidity()<<endl;

    StThreeVectorF vertex = uDst->event()->primaryVertexPosition();
    towerLocation -= vertex; //shift the origin to the vertex, not (0., 0., 0.)
    //cout <<"\tvert:\t"<<vertex.z()<<"\tnewEta:\t"<<towerLocation.pseudoRapidity()<<endl;
	    
    StThreeVectorF momentum(1., 1., 1.);
    momentum.setPhi(phi);
    momentum.setTheta( towerLocation.theta() ); //use corrected theta
    momentum.setMag(pMag);
    StLorentzVectorF p4(corrected_energy, momentum);
    //cout <<zVertex<<"\t"<<phi<<"\t"<<theta<<"\t"<<p4.phi()<<"\t"<<p4.theta()<<"\t"<<energy<<"\t"<<corrected_energy<<endl;
	    
    //now construct StMuTrackFourVec object for jetfinding
    StMuTrackFourVec* pmu = new StMuTrackFourVec();
    pmu->Init(0, p4, id, kBarrelEmcTowerId );
    tracks.push_back(pmu); //for jet finding interface

    //cout <<"corrected energy"<<endl;
    //cout <<"E:\t"<<pmu->e()<<"\tp:\t"<<pmu->p()<<"\tm:\t"<<pmu->mass()<<"\tE^2-p^2:\t"<<pmu->e()*pmu->e() - pmu->p()*pmu->p()<<endl;
	    
  }
}

void StBET4pMaker::collectEnergyFromEEMC()
{
  StMuDst* uDst = mMuDstMaker->muDst();

  if (mUseEndcap) {
	
    // Now add endcap points --------------------------
    StMuEmcCollection* muEmc = uDst->muEmcCollection();
    assert(muEmc);

    for (int id=0; id<muEmc->getNEndcapTowerADC(); ++id) {

      int rawadc, sec, sub, etabin;
      double adc, energy;

      muEmc->getEndcapTowerADC(id, rawadc, sec, sub, etabin);
      assert(sec>0 && sec<=MaxSectors);
	
      //find eta and phi values from sector, subsector and etabin assuming z=0,0,0
      TVector3 towerCenter = mEeGeom->getTowerCenter(sec-1,sub-1,etabin-1); //careful, this is indexed from 0
	
      const EEmcDbItem *dbItem = mEeDb->getT(sec,sub-1+'A',etabin);
      assert(dbItem); 
	
      if(dbItem->fail) continue; //drop broken channels
      if(dbItem->stat) continue; // drop not working channels and jumpy pedestal channels
      if(dbItem->gain<=0.) continue; // drop it, unless you work with ADC spectra
      if(rawadc<dbItem->thr) continue; // drop raw ADC < ped+N*sigPed, N==3 in init
	    
      adc = rawadc - (dbItem->ped);
      energy=adc/(dbItem->gain);
      if(energy < 0.01) continue; // drop if less than 10MeV for now
	    
      //construct four momentum
      double mass = 0.; //assume photon mass for now, that makes more sense for towers, I think.
      double pMag = (energy>mass) ? sqrt(energy*energy - mass*mass) : energy; //NOTE: this is a little naive treatment!

      //correct for eta shift
      StThreeVectorD towerLocation(towerCenter.X(), towerCenter.Y(), towerCenter.Z());
      StThreeVectorF vertex = uDst->event()->primaryVertexPosition();
      towerLocation -= vertex; //shift the origin to the vertex, not (0., 0., 0.)

      //construct momentum 3-vector
      StThreeVectorF momentum(1., 1., 1.);
      momentum.setPhi( towerCenter.Phi() );
      momentum.setTheta( towerLocation.theta() ); //use theta from vertex subtracted point.
      momentum.setMag(pMag);
      StLorentzVectorF p4(energy, momentum);
	    
      //now construct StMuTrackFourVec object for jetfinding
      StMuTrackFourVec* pmu = new StMuTrackFourVec();
      pmu->Init(0, p4, id, kEndcapEmcTowerId );
      tracks.push_back(pmu); //for jet finding interface
    }
  }
}

StEmcCollection *StBET4pMaker::find_StEmCCollection() {

  StEmcCollection* emc(0);

  //new loop on data:
  /*first look for StEvent in memory.  This should only happen if
    (a) StEmcADC2EMaker is running (e.g., year 2003 and before)
    (b) StEmcSimulatorMaker is running (all pythia!).  In this case simulator recalculates ADC from
    scratch using DB gains, so we _don't_ take what's in the MuDst collection
  */

  StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
  if (event) {
    LOG_DEBUG <<"StBET4pMaker::Make()\tRetrieve StEmcCollection from StEvent"<<endm;
    emc = event->emcCollection();
  } else {
    LOG_DEBUG <<"StBET4pMaker::Make()\tRetrieve StEmcCollection from MuDst"<<endm;
    emc = mMuDstMaker->muDst()->emcCollection();
  }
  assert(emc);

  return emc;
}

bool StBET4pMaker::isCorrupted()
{
  StEmcCollection* emc = find_StEmCCollection();
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
  StEmcADCtoEMaker* adc2e = (StEmcADCtoEMaker*)GetMaker("Eread");
  if (!adc2e) {
    LOG_ERROR <<"StBET4pMaker::fillBarrelHits()\tno adc2e in chain"<<endm;
  } else {
    LOG_DEBUG <<"StBET4pMaker::fillBarrelHits()\tfound adc2e in chain"<<endm;
    if (adc2e->isCorrupted()) {
      return true;
    }
  }

  return false;
}


void StBET4pMaker::fillBemcTowerHits()
{
  mCorrupt = isCorrupted();
  if(mCorrupt) return;

  StEmcCollection* emc = find_StEmCCollection();
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

bool StBET4pMaker::shouldKeepThisBemcHit(StEmcRawHit* theRawHit, int bemcTowerID)
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


double StBET4pMaker::sumEnergyOverBemcTowers(double minE)
{
  double ret(0.0);
  for(int bemcTowerID = 1; bemcTowerID <= 4800; ++bemcTowerID) {
    if(mBTowHits[bemcTowerID] && mBTowHits[bemcTowerID]->energy() > minE) {
      ret += mBTowHits[bemcTowerID]->energy();
    }
  }
  return ret;
}

int StBET4pMaker::numberOfBemcTowersWithEnergyAbove(double minE)
{
  int ret(0);
  for(int bemcTowerID = 1; bemcTowerID <= 4800; ++bemcTowerID) {
    if(mBTowHits[bemcTowerID] && mBTowHits[bemcTowerID]->energy() > 0.4) {
      ret++;
    }
  }
  return ret;
}

bool accept2003Tower(int id)
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
