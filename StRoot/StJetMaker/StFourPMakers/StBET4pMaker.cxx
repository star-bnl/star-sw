
using namespace std;
//std
#include <string>
#include <iostream>
#include <math.h>
#include <sys/times.h>

//STAR
#include "TFile.h"
#include "StChain.h"
#include "SystemOfUnits.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"

//StEvent
#include "StEventTypes.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcRawMaker/StEmcRawMaker.h"
#include "StEmcRawMaker/defines.h"

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/cstructs/eemcConstDB.hh"
#include "StEEmcUtil/EEfeeRaw/EEname2Index.h"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"

//StJetMaker
#include "StJetMaker/StMuTrackFourVec.h"

#include "StMuEmcPosition.h"
#include "StBET4pMaker.h"

ClassImp(StBET4pMaker)
    
    StBET4pMaker::StBET4pMaker(const char* name, StMuDstMaker* uDstMaker, StEmcADCtoEMaker* adc2e)
	: StFourPMaker(name, 0), mMuDstMaker(uDstMaker), mAdc2E(adc2e)
{
    cout <<"StBET4pMaker::StBET4pMaker()"<<endl;
    mCorrupt = false;
    mUseEndcap = false;
    mField = 0.;
    mMuPosition = new StMuEmcPosition();

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
    cout <<"void StBET4pMaker::Clear(Option_t* opt)";
    mCorrupt = false;
    mField = 0.;
    
    for (BET4Vec::iterator it=mVec.begin(); it!=mVec.end(); ++it) {
	delete (*it);
	(*it) = 0;
    }
    mVec.clear();

    //reset pointers to Barrel hits
    for (int i=1; i<4801; ++i) {
	mBTowHits[i] = 0;
	mNtracksOnTower[i] = 0;
    }
    
    StFourPMaker::Clear(opt);
    cout <<"\tvec:\t"<<mVec.size()<<"\ttracks:\t"<<tracks.size()<<endl;
    return;
}

Int_t StBET4pMaker::Make()
{

    cout <<"StBET4pMaker::Make()"<<endl;
    cout <<"\tvec:\t"<<mVec.size()<<"\ttracks:\t"<<tracks.size()<<endl;

    //get basic pointers:
    assert(mMuDstMaker);
    StMuDst* uDst = mMuDstMaker->muDst();
    assert(uDst);
    
    StMuEvent* uEvent = uDst->event();
    assert(uEvent);
    mField = uEvent->magneticField()/10.0; //to put it in Tesla

    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    assert(geom);


    //next fill Barrel hit array, subtract energy later:
    fillBarrelHits();

    //check for barrel corruption (only works for P04ik and later!
    if (mCorrupt==true) {
	tracks.clear();
	cout <<"StEmcTpcFourPMaker::Maker():\tFlag this as a corrupt event.  Clear 4-p container and return"<<endl;
	return kStOk;
    }
    
    

    //next, loop on tracks and add to the 4p list:
    long nTracks = uDst->numberOfPrimaryTracks();

    int ntkept, badflag, ftpc, loweta, higheta, badr, badhits;
    ntkept = badflag = ftpc = loweta = higheta = badr = badhits = 0;
    
    for(int i = 0; i < nTracks; i++)	{
	StMuTrack* track = uDst->primaryTracks(i);
	assert(track);
	if(track->flag() < 0) {
	    ++badflag;
	    continue;
	}
	if (track->topologyMap().trackFtpcEast()==true || track->topologyMap().trackFtpcWest()==true) {
	    ++ftpc;
	    continue;
	}
	if(track->eta() < GetEtaLow()) { //GetEtaLow defined in StFourPMaker.cxx
	    ++loweta;
	    continue;
	}
	if(track->eta() > GetEtaHigh()) {
	    ++higheta;
	    continue;
	}
	if(static_cast<double>(track->nHits())/static_cast<double>(track->nHitsPoss()) < .51) {
	    ++badhits;
	    continue;
	}
	++ntkept;



	//check projection to BEMC and remember for later: ---------------------------------------------
	StThreeVectorD momentumAt,positionAt;
	
	bool tok = mMuPosition->trackOnEmc(&positionAt, &momentumAt, track, mField, geom->Radius() );
	if(tok) {
	    int m,e,s,id=0;
	    float eta=positionAt.pseudoRapidity();
	    float phi=positionAt.phi();
	    int stat = geom->getBin(phi,eta,m,e,s);
	    stat = geom->getId(m,e,s,id);
	    if(stat==0) {
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
	mVec.push_back(pmu); //this is for memory ownership.  StBET4pMaker has to delete these in Clear()
	tracks.push_back(pmu); //this is for expected interface to StJetMaker --> StppJetAnalyzer
    }
    
    cout <<"skipped "<<badflag<<" for flag, "<<ftpc<<" for ftpc, "<<loweta<<" for loweta, "<<higheta<<" for higheta, "
	 <<badr<<" for badr, "<<badhits<<" for hits"<<endl;
    cout <<"Added:\t"<<ntkept<<"\ttracks to the container"<<endl;


    //now loop on Barrel hits, correct energy, and push back for jet finding:
    for (int id=1; id<=4800; ++id) { //id==software id: [1,4800]
	StEmcRawHit* hit = mBTowHits[id];
	if (!hit) continue; //either no hit here or status!=1
	
	    float energy = hit->energy();
	    if(energy<=0.) continue; //skip it, E=0 can happen from gain=0. in calib table

	    //Get eta, phi
	    float eta, phi;
	    geom->getEtaPhi(id,eta,phi); // to convert software id into eta/phi

	    
	    //construct four momentum
	    //double mass = 0.1349764; //assume pi-zero mass for now
	    double mass = 0.; //assume photon mass for now, that makes more sense for towers, I think.
	    
	    double pMag = (energy>mass) ? sqrt(energy*energy - mass*mass) : energy; //NOTE: this is a little naive treatment!
	    float theta=2.*atan(exp(-eta));

	    //do a quick correction for hadronic MIP eneryg deposition:
	    double MipE = 0.261*(1.+0.056*eta*eta)/sin(theta); //GeV
	    double nMipsOnTower = static_cast<double>(mNtracksOnTower[id]);
	    double corrected_energy = energy - nMipsOnTower*MipE;
	    //cout <<"Subtracting:\t"<<nMipsOnTower*MipE<<"\tfrom energy:\t"<<energy<<"\tfrom:\t"<<nMipsOnTower<<"\tmips"<<endl;
	    if (corrected_energy<=0.) continue;

	    //now correct for eta-shift due to non-zero z_vertex (but note, no correction to Energy!)
	    double RSMD = 2.2625*100.; //radius of SMD in cm
	    StThreeVectorD towerLocation(1., 1., 1.);
	    towerLocation.setPhi(phi);
	    towerLocation.setTheta(theta);
	    towerLocation.setMagnitude(RSMD/cos(theta));
	    StThreeVectorF vertex = uDst->event()->primaryVertexPosition();
	    towerLocation -= vertex; //shift the origin to the vertex, not (0., 0., 0.)

	    StThreeVectorF momentum(1., 1., 1.);
	    momentum.setPhi(phi);
	    momentum.setTheta( towerLocation.theta() ); //use corrected theta
	    momentum.setMag(pMag);
	    StLorentzVectorF p4(corrected_energy, momentum);
	    //cout <<zVertex<<"\t"<<phi<<"\t"<<theta<<"\t"<<p4.phi()<<"\t"<<p4.theta()<<"\t"<<energy<<"\t"<<corrected_energy<<endl;
	    
	    //now construct StMuTrackFourVec object for jetfinding
	    StMuTrackFourVec* pmu = new StMuTrackFourVec();
	    pmu->Init(0, p4, id, kBarrelEmcTowerId );
	    mVec.push_back(pmu);  //for memory ownership
	    tracks.push_back(pmu); //for jet finding interface
	    
    }


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
	    mVec.push_back(pmu);  //for memory ownership
	    tracks.push_back(pmu); //for jet finding interface
	}
    }
    
    cout <<StMaker::GetName()<<"::Make()\tAdded:\t"<<tracks.size()<<"\tparticles to track container"<<endl;

    return StMaker::Make();
}


void StBET4pMaker::fillBarrelHits()
{
    StMuDst* uDst = mMuDstMaker->muDst();
    assert(uDst);
    
    StEmcGeom* geom = StEmcGeom::instance("bemc"); // for towers
    assert(geom);

    //Get status tables.
    assert(mAdc2E);
    StBemcTables* tables = mAdc2E->getBemcData()->getTables();
    assert(tables);

    //Now loop on emc data
    StEmcCollection *emc = uDst->emcCollection();
    if (!emc) {
	StEvent* event = dynamic_cast<StEvent*>( GetInputDS("StEvent") );
	if (event) {
	    cout <<"StBET4pMaker::Make()\tRetrieve StEmcCollection from StEvent"<<endl;
	    emc = event->emcCollection();
	}
	else {
	    cout <<"StBET4pMaker::Make().  Could not find StEvent in memory"<<endl;
	}
    }
    assert(emc);
    
    // now it is like StEvent, getting energies for towers
    StEmcDetector* detector = emc->detector(kBarrelEmcTowerId);

    mCorrupt = false; //let's be optimistic to start
    
    //if detector==null, this means it's corrupt for pre-October 2004 BEMC code.  However, not all corrupt events give detector==0
    if (!detector) {
	mCorrupt=true;
	return;
    }
    
    //now, check for corruption in post-October 2004 BEMC code (P04k and later)
    //cout <<"StEmcTpcFourPMaker::Make()\tcheck  crate corruption with key: crateUnknown=0, crateNotPresent=1, crateOK=2, crateHeaderCorrupt=3"<<endl;
    for(int crate = 1; crate<=MAXCRATES; crate++) {
	StEmcCrateStatus crateStatus = detector->crateStatus(crate);
	//cout <<"crate:\t"<<crate<<"\tstauts:\t"<<crateStatus<<endl;
	if (crateStatus==crateHeaderCorrupt) {
	    mCorrupt=true;
	    return;
	}
    }

    //now gather hits into pointer array:
    
    for(int m = 1; m<=120;m++) { //loop on modules...
	StEmcModule* module = detector->module(m);
	assert(module);
	
	StSPtrVecEmcRawHit& rawHits = module->hits();
	
	for(UInt_t k=0;k<rawHits.size();k++) { //loop on hits in modules
	    StEmcRawHit* tempRawHit = rawHits[k];
	    
	    //Get eta, phi
	    int m = tempRawHit->module();
            int e = tempRawHit->eta();
            int s = abs(tempRawHit->sub());
            int id, status;
	    int ADC = tempRawHit->adc(); //not pedestal subtracted!
	    geom->getId(m,e,s,id); // to get the software id
	    
	    //now check the status: (//BTOW defined in StEmcRawMaker/defines.h
	    tables->getStatus(BTOW, id, status);

	    //check for ADC that is 2-sigma above RMS:
	    float pedestal, rms;
	    int CAP=0; //this arument matters only for SMD
	    tables->getPedestal(BTOW, id, CAP, pedestal, rms);

	    
	    //if the status is good, add it to the array, otherwise add a null pointer
	    if ( ADC-pedestal>0 && (ADC-pedestal)>2.*rms && status==1) { //it's good
		mBTowHits[id] = tempRawHit;
	    }
	    else { //marked as bad:
		mBTowHits[id] = 0;
	    }
	}
    }
}
