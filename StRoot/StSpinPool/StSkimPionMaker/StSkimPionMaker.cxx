/*******************************************************************************
 * 
 * Pi0 Maker for 2004 p+p real data
 *
 * Author: Frank Simon, MIT, May 2005
 * based on code from Andre Mischke 
 *******************************************************************************
 *  
 * Description: StFranksPi0Maker: 
 *
 * calculates minv(gamma,gamma) tower by tower. The tower is assigned by
 * the neutral bemc point with the highest energy.
 * A tree with pi0 information is filled for subsequent analysis
 *
 ******************************************************************************/
 
#include <vector>
#include <algorithm>

#include "TFile.h"
#include "TH2F.h"
#include "TObjArray.h"

// ROOT
#include "TROOT.h" // for gROOT variable
#include "TNtuple.h"
#include "TTree.h"

// StEvent
#include "StChain.h"
#include "StEvent.h"
#include "StEventTypes.h"
#include "StHit.h"
#include "StMeasuredPoint.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StL0Trigger.h"

//StMuDstMaker
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuEmcCollection.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

// EMC related
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"

//StSpinDbMaker
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"


#include "StPrimaryVertex.h"
#include "StPrimaryTrack.h"
#include "StDedxPidTraits.h"
#include "StTrackFitTraits.h"
#include "StTrackDetectorInfo.h"
#include "StTrackGeometry.h"
#include "StTpcDedxPidAlgorithm.h"
#include "StTriggerDetectorCollection.h"
#include "StCtbTriggerDetector.h"
#include "StZdcTriggerDetector.h"
#include "StEventTypes.h"
#include "StMemoryInfo.hh"

#include "StTrack.h"
#include "StVertex.h"
#include "StMaker.h"

// for trigger prescales
#include "StDetectorDbMaker/StDetectorDbTriggerID.h"

// for trigger simulation
//#include "StJetMaker/StJetSimuUtil/StJetEmcTrigSim.h"

// for MuTrack EMC matching
//#include "StJetMaker/StFourPMakers/StMuEmcPosition.h"
#include "StEmcUtil/projection/StEmcPosition.h"

// for Renees Trigger Sim:
#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/Bbc/StBbcTriggerSimu.h"
#include "StTriggerUtilities/Bemc/StBemcTriggerSimu.h"
#include "StTriggerUtilities/L2Emulator/StL2TriggerSimu.h"

// Issams Background maker (private copy)
//#include "StEmcPool/BckgFinder/StBemcBeamBckgFinderMaker.h"

#include "StThreeVectorF.hh"
#include "StSkimPionMaker.h"

//for BBC vertex information (needs a modified version of StBbcTriggerDetector.h / cxx
#include "StBbcTriggerDetector.h"

// for Pi0EventTree
#include "TSkimPionEvent.h"

ClassImp(StSkimPionMaker);
    
//________________________________________________________________________________________________________
StSkimPionMaker::StSkimPionMaker(const char *name, Bool_t doTracks, const char *outfile):StMaker(name)
{
    // constructor 
    mDoTracks = doTracks;
    mFileName = outfile;
    
    mPi = 3.1416;
    mPi0Mass = 0.13498;
    
    debug = false; // debug mode  
    fnummixed = 10; // number of events for event-mixing  
    
    // initialize counter values
    ievtot=0;
    iWrittenEvents = 0;
    /*
      ievaccep=0;
      inochainpt=0; 
      inoevpt=0;
      ibadrun=0;
      iemc=0;
      inoprimvert=0;
      itrig=0;
    */
}

//________________________________________________________________________________________________________
StSkimPionMaker::~StSkimPionMaker()
{
    // destructor	
}

//________________________________________________________________________________________________________
Int_t StSkimPionMaker::Init()
{
    //initializes histograms and ntuples
    
    if (debug) cout << "++++++++++++ StSkimPionMaker::Init()" << endl;
    
    mTables = new StBemcTables();
    
    mFile = new TFile(mFileName,"RECREATE");
    
    // list for photon candidates
    photonlist = new TObjArray();
    
    //creating Pi0Event and Tree
    pi0Tree = new TTree("pi0Tree", "Pi0 Event Tree");
    pi0Event = new TSkimPionEvent();
    pi0Tree->Branch("EventBranch", "TSkimPionEvent", &pi0Event);
    
    //#include "/star/u/ahoffman/Emc_calib/c_factors.txt" //taken out for first run

    if (debug) cout << "StSkimPionMaker: Init done..." << endl;
    
    return StMaker::Init();
}

//_________________________________________________________________________________________________________
Int_t StSkimPionMaker::InitRun(int runnumber)
{
    return StMaker::InitRun(runnumber);
}

//________________________________________________________________________________________________________
Int_t StSkimPionMaker::Make()
{
    //cout << "Entering StSkimPionMaker::Make, processed events: " << ievtot << endl;
    // the main function, gets called for each event
    
    ievtot++;
    if (debug) 
	cout <<"+++++++++++++++++++++++++++++++++++++++++ event: "<<ievtot<<endl;
	
    //if (ievtot%2000==0) pi0Tree->AutoSave(); //saveHistograms(); // save data regularly, always good in case the code crashes (not sure)
	
    //cout << "Getting MuDst...\n";
    mMuDst = (StMuDst*)GetInputDS("MuDst"); //mMuDstMaker->muDst();
    if (!mMuDst) return 1;
	
    // here is the actual analysis code
	
    StMuEvent* event = mMuDst->event();
	
    if (!event)
	{
	    if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() Cannot get the event pointer"<<endl;
	    return kStOK;
	}
    
    runN = event->runNumber();
    eventN = event->eventNumber();
    fillN = (Int_t)event->runInfo().beamFillNumber(yellow);

    pi0Event->SetRunNo((Int_t)runN);
    pi0Event->SetEventNo((Int_t)eventN);
    pi0Event->SetFillNo((Int_t)fillN);
	
    // get the magnetic field
    mField = event->magneticField()/10.; // bFld in Tesla
    //if (debug) 
    //cout <<" StEvent::summary()->magneticField() = "<<mField<<" [Tesla]"<< endl;
	
    if (fabs(mField)<0.01)
	{
	    if (debug) cout << "BField read back as 0, setting default." << endl;
	    mField = 0.497952;
	}
	
    // FInd out if we are running simulations:
    // check for presence of StMcEvent

    // Get additional trigger information, especially for simulations
    // BBC Triggers
    
    StTriggerSimuMaker* trigSim = dynamic_cast<StTriggerSimuMaker*>(GetMaker("StarTrigSimu"));
    if (trigSim) {
	//cout << "Found TriggSim Info for BBC\n";
	mBBCTrig = (trigSim->bbc)->triggerDecision(137611);
	//cout << "BBC Trigger: " << mBBCTrig << endl;
    }
    else {
	if (debug) cout << "StFranksPi0Maker::Make() could not find SimuTrig \n";
	mBBCTrig = 0;
    }
	
    pi0Event->SetBBCTrig(mBBCTrig);

    // Get Spin info if this is not a simulated event
	    
    StL0Trigger* trig=&(event->l0Trigger());
    Int_t mubx48=trig->bunchCrossingId();
    Int_t mubx7=trig->bunchCrossingId7bit(runN); 
    pi0Event->SetSpinBit(trig->spinBits(runN)); //SpinBit (Yellow,Blue): Up,Up = 5; Down,Up = 6; Up,Down = 9; Down,Down = 10;  
    pi0Event->SetBunchX48(mubx48);
    pi0Event->SetBunchX7(mubx7);
	    
    /*  StSpinDbMaker */
    StSpinDbMaker* spDb = (StSpinDbMaker*)GetMaker("spinDb"); 
    if (spDb) {
	Int_t val = 0;
	if ((spDb->offsetBX48minusBX7(mubx48,mubx7)==0)&&(spDb->isValid())) val = 1;
	pi0Event->SetValidSpin(val);
	pi0Event->SetPolLong(spDb->isPolDirLong());
	pi0Event->SetMaskedXing(spDb->isMaskedUsingBX48(mubx48));
	pi0Event->SetDbSpinBit(spDb->spin4usingBX48(mubx48));
    }
    else {
	pi0Event->SetValidSpin(0);	
    }

    int softwaretrigs[2] = {0,0};
    int mcMB = 0;
    int mcHTTPL2 = 0;
    if ((trigSim->bbc)->triggerDecision(137611)) mcMB = 1;
    if (trigSim) {
	//cout<<"Found Software Trigger Info...\n";
	if ((trigSim->isTrigger(137611)) || (trigSim->isTrigger(127611))) mcHTTPL2 = 1;
    }

    else {
	if (debug) cout<<"SkimPionMaker::Make() could not find the trigger emulator"<<endl;
	mHiTowerAdc6Bit = 0;
    }

    softwaretrigs[0] = mcMB;
    softwaretrigs[1] = mcHTTPL2;
	    
    pi0Event->SetHiTowerAdc6Bit(mHiTowerAdc6Bit);
    
    // also store mc vertex position, so that for simulations this can be used instead of reco vertex
    StThreeVectorF mcVertexPos;
		
    //cout << "Getting Triggers...\n";	
    // check for trigger condidtions
    int startriggers[4] = {117001, 137611, 5}; //mb, http-l2gamma, http-l2gamma-test 2006
		
    int trigs[3] = {0,0,0};
    int prescales[3] = {0,0,0};
    //if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() not MinBias or High Tower triggered events"<<endl;

    //see if we satisfy various triggers
    for (int i=0; i<3; ++i) {
	if (event->triggerIdCollection().nominal().isTrigger(startriggers[i])) {
	    trigs[i] = 1;
	    if (debug) cout << "Trigger condition " << i << " satisfied..." << endl;
	}
    }
    if (event->triggerIdCollection().nominal().isTrigger(127611)) trigs[1] = 1;
		
    //get trigger prescales:
    //cout<<"Checking for Prescales..."<<endl;
    StDetectorDbTriggerID& v = *(StDetectorDbTriggerID::instance());
    for (int i=0;i<3;++i) {
	prescales[i]=v.getTotalPrescaleByTrgId(startriggers[i]);
    }
    if (event->triggerIdCollection().nominal().isTrigger(127611)) prescales[1]=v.getTotalPrescaleByTrgId(127611);

    // I also have the global trigger variables that get used for histograms. Fill them here:
    mb = trigs[0];
    httpl2 = trigs[1];
    httpl2_test = trigs[2];
 
    pi0Event->SetTriggers(trigs);
    pi0Event->SetPrescales(prescales);
    pi0Event->SetSoftTriggers(softwaretrigs);
		
    // to save time, lets opt out here if it is not a useful trigger and not simulations
    //cout << "Accepting all triggers... If you want to change that, check out line 600" << endl;
	
    if (!((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2))) {
	//cout << " No Interesting trigger, opting out..." << endl;
	pi0Event->Clear();
	return kStOk;
    }		

    // now the existence of various pointers is checked to assure a good event	
    StThreeVectorF vPos;
    vPos = event->primaryVertexPosition(); 
	
    // get BBC vertex
    // not supported right now, could be later...
    StBbcTriggerDetector bbc = event->bbcTriggerDetector();
    //int west=bbc.tdcEarliestWest();
    //int east=bbc.tdcEarliestEast();
    //int diff=west-east;
    mBBCVertexZ = 0; // z is now in cm 
    pi0Event->SetBBCVertexZ(mBBCVertexZ);
  
    //for the TAMU relative luminosities, the BBC time bin is needed. This is derived from the time difference:
    pi0Event->SetBBCTimeBin(bbc.onlineTimeDifference());
    
    // so, if the vertex is not found try using the bbc vertex
    if((vPos.z() == 0)||(TMath::Abs(vPos.z()) > 300)) { 
	if (mBBCVertexZ == 0) mBBCVertexZ = 0.001;
	vPos.setZ(mBBCVertexZ);
	vPos.setX(0.);
	vPos.setY(0.);
	pi0Event->SetOnlyBBCVertex();
    }
  
    StEvent *stevent = (StEvent*)this->GetInputDS("StEvent");
    if (!stevent) {
	if (debug) cout << "++++++++++++ StSkimPionMaker::Make: Can't get StEvent pointer" << endl;
	return kStOk;
    }
    
    StEmcCollection* emccol = (StEmcCollection*)stevent->emcCollection();
    if (!emccol) {
	if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() No EMC Collection"<<endl;
	//iemc++;
	if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
	    iWrittenEvents++;
	    pi0Tree->Fill();		
	}
	pi0Event->Clear();			
	return kStOk;
    }
    
    StEmcDetector* bemc = emccol->detector(kBarrelEmcTowerId);
    if (!bemc) {
	if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() No BEMC detector"<<endl;
	//mEventInfoNtuple->Fill(eventInfo);
	if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
	    pi0Tree->Fill();		
	    iWrittenEvents++;
	}
	pi0Event->Clear();			
	return kStOk;
    }
    
    StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();

    // Fill histos with raw EMC information
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
  
    // get primary tracks
    long numberOfPrimaryTracks = mMuDst->primaryTracks()->GetEntries();
  
    // reject z_vertex==0 events 
    //cout << "StSkimPionMaker:  z_vertex: " << vPos.z() << endl;
    pi0Event->SetVertex(vPos.x(),vPos.y(),vPos.z());
    
    if (vPos.z()==0) {
	if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() z_vertex==0"<<endl;
	if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
	    pi0Tree->Fill();
	    iWrittenEvents++;
	}
	pi0Event->Clear();	
	return kStOk;
    }
    
    if (!(vPos.z()>-100. && vPos.z()<100.)) 
	{
	    if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() z_vertex out of range!"<<endl;
	    if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
		pi0Tree->Fill();		
		iWrittenEvents++;
	    }
	    pi0Event->Clear();			
	    return kStOk;
	}
  
    // calculate pt sum of good global tracks on both sides of TPC
    StMuTrack* t;
    Float_t chargedPtSum = 0;
	
    for (int i = 0; i < numberOfPrimaryTracks; i++) {
	t = mMuDst->primaryTracks(i);
	if ((t->nHits() > 15)&&(t->dca().mag() < 3.0)&&(t->eta() > -1.)&&(t->eta() < 1.)) //using full barrell now.
	    chargedPtSum += t->pt();
    }
    //cout << "Charged pt sum: " << chargedPtSum << endl;
    pi0Event->SetChargedPtSum(chargedPtSum);
	
	
    // for all practical purposes events are accepted here
    // No EMC points still lead to rejection, but that does not count, since most
    // MB events have no EMC hits
	
    // reject events with no BEMC hits  
    if (bEmcPoints.size()==0)
	{
	    if (debug) cout <<"++++++++++++ StSkimPionMaker::Make() No BEMC points"<<endl;
	    if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
		pi0Tree->Fill();
		iWrittenEvents++;
	    }
	    pi0Event->Clear();		
	    return kStOk;  // Check this again! In principle the event should be rejected if there are no BEMC points
	}	  
	
    // event characteristics
    if (debug) cout <<"--- Number of primary tracks in this event: "<<numberOfPrimaryTracks<<endl;
    // get reduced run number
    Int_t runnm = runN-5000000;
    if (debug) cout <<"--- event accepted... RUN_ID: "<<runN<<" ("<<runnm<<")"<<endl;
   
    // read EMC point list
    if (!readPointList()) // here the photon list gets filled
	{
	    if (debug) cout <<"+++  ERROR read readPointList()"<<endl;
	    if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
		pi0Tree->Fill();
		iWrittenEvents++;
	    }
	    pi0Event->Clear();	
	    return kStOk;
	}
   
   
    // look for track-point (TPC-BEMC) association
    // this is needed to veto photon candidates that have a charged 
    // track pointing to them (and thus probably are no real photons)
    // this information is used in getInvMass
   
    if (mDoTracks) associateTracksWithEmcPoints(this);
   
    // Get total neutral Energy in EMC
    Float_t neutralEnergy = getNeutralEnergySum(photonlist);
    //cout << "Total neutral Energy: " << neutralEnergy<< endl;
    pi0Event->SetNeutralEnergy(neutralEnergy);
	
	
    // some basic quality cuts before we go on: 
    // - There should be photon candidates
    // - Not to many EMC points (to exclude noisy events)	
	
    if (photonlist->GetEntries()>0 && bEmcPoints.size()<=100 && vPos.z()>-100 && vPos.z()<100) {
	if (debug) cout<< "--- new photon " << photonlist->GetEntries() << endl;
    
	// get the Pi0s, fill the ntuple
	getInvMass(0, photonlist, photonlist, vPos.z(), vPos, trigs, prescales);     
    }
	
    // write Pi0Event Tree
    if ((mb)||(httpl2)||(httpl2_test)||(mcHTTPL2)) {
	pi0Tree->Fill();
	iWrittenEvents++;
    }

    pi0Event->Clear();		
    // clean up
	
    photonlist->Clear();
    if (debug) cout <<"--- reset photon/charged_pion list done"<<endl;
	
    return kStOK;
}

//________________________________________________________________________________________________________

Int_t StSkimPionMaker::Finish()
{
    cout << "Events written to tree:    " << iWrittenEvents << endl;
  
    mFile->Write();
    mFile->Close();
    return kStOk; 
}

//________________________________________________________________________________________________________
StThreeVectorF StSkimPionMaker::getPoint(StEmcPoint *p, Int_t &id, Float_t &e, Float_t &pt, Int_t &n, Int_t &t, Float_t &eSMDe, Float_t &eSMDp, Float_t &eTower, Float_t &sSMDe, Float_t &sSMDp, Float_t &sTower)
{
    // right now, the point vector is defined as the vector from 0,0,0 and not the vector from the main vertex
    // might be a problem?

    StMuEvent* event = mMuDst->event();
    if(!event)
	{
	    cout << "++++++++++++ StSkimPionMaker::getPoint: Can't get Event pointer" << endl;
	    //       assert ( 0 && " Can't convert kStOk to StThreeVectorF. Please fix me!!!");
	    StThreeVectorF abortVector(-10000.,0.,0.);
	    return abortVector;
	}
    
    // get primary vertex
    StThreeVectorF MainVertexPosition;
    MainVertexPosition = event->primaryVertexPosition();
    
    const StThreeVectorF& pointVector = p->position();
    
    // get tower id // this gives the id of the tower that is closest to the cluster center
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    Int_t mod, eta, sub;
    emcGeom->getBin(pointVector.phi(), pointVector.pseudoRapidity(), mod, eta, sub);
    emcGeom->getId(mod, eta, sub, id);
  
    // better way of getting cluster size info
    StPtrVecEmcCluster& towerClus=p->cluster(kBarrelEmcTowerId);
    StPtrVecEmcCluster& etaClus=p->cluster(kBarrelSmdEtaStripId);
    StPtrVecEmcCluster& phiClus=p->cluster(kBarrelSmdPhiStripId);
    Int_t nEtaHits=0;
    Int_t nPhiHits=0;
    Int_t nTowerHits = 0;
    Float_t towerEnergy = 0;
    Float_t etaEnergy=0.;
    Float_t phiEnergy=0.;
    Float_t etaWidth=0.;
    Float_t phiWidth=0.;
    Int_t tId[5];
    Float_t tEnergy[5];
    for (int k=0;k<5;k++) {
	tId[k] = 0;
	tEnergy[k] = 0;
    }
    Float_t fraction = 0;
    
    if(towerClus.size() > 0) {
	StEmcCluster *clT=(StEmcCluster*)towerClus[0];
	StPtrVecEmcRawHit& hT=clT->hit();
	nTowerHits=hT.size();
	if (debug) cout<<"the number of towers is "<<nTowerHits<<endl;
	for (Int_t j=0; j<nTowerHits; j++) {
	    //cout<<"getting tower cluster info for raw hit "<<j<<endl;
	    StEmcRawHit *rawHit = (StEmcRawHit*)hT[j];
	    tId[j] = rawHit->softId(emcGeom->Detector());
	    //cout<<"t Id for "<<j<<" is "<<tId[j]<<endl;
	    if (tId[j] == 0) continue;
	    tEnergy[j] = rawHit->energy();
	    //cout<<"tEnergy for "<<j<<" is "<<tEnergy[j]<<endl;
	}
	id = tId[0];
	towerEnergy = tEnergy[0];
    }
    
    if(etaClus.size()>0){
	StEmcCluster *clE=(StEmcCluster*)etaClus[0];
	StPtrVecEmcRawHit& hE=clE->hit();
	nEtaHits=hE.size();
	etaEnergy=clE->energy();
	etaWidth=clE->sigmaEta();
    }
    if(phiClus.size()>0){
	StEmcCluster *clP=(StEmcCluster*)phiClus[0];
	StPtrVecEmcRawHit& hP=clP->hit();
	nPhiHits=hP.size();
	phiEnergy=clP->energy();
	phiWidth=clP->sigmaPhi();
    }

    // get point energy
    e = p->energy();

    // get fraction
    fraction = e/(tEnergy[0]+tEnergy[1]+tEnergy[2]+tEnergy[3]+tEnergy[4]);

    // get Alan's corrected energy from correction factors file
    //e = fraction*(tEnergy[0]*c_factors[tId[0]] + tEnergy[1]*c_factors[tId[1]] + tEnergy[2]*c_factors[tId[2]] + tEnergy[3]*c_factors[tId[3]] + tEnergy[4]*c_factors[tId[4]]);

    // get photon pT 
    Float_t theta = 0.;
    emcGeom->getTheta(mod, eta, theta);
    pt = e * sin(theta);
  
    // get point-track association
    n = p->nTracks();     

    // get point type 
    t=0;
    if (p->energyInDetector(StDetectorId(kBarrelEmcTowerId+2))) //smde
	t++;
    if (p->energyInDetector(StDetectorId(kBarrelEmcTowerId+3))) //smdp
	t+=2;
	
    eSMDe = etaEnergy; //p->energyInDetector(StDetectorId(kBarrelEmcTowerId+2));
    eSMDp = phiEnergy; //p->energyInDetector(StDetectorId(kBarrelEmcTowerId+3));
    eTower = towerEnergy; //p->energyInDetector(StDetectorId(kBarrelEmcTowerId));
    sSMDe = nEtaHits; //p->sizeAtDetector(StDetectorId(kBarrelEmcTowerId+2));
    sSMDp = nPhiHits; //p->sizeAtDetector(StDetectorId(kBarrelEmcTowerId+3));
    sTower = nTowerHits; //p->sizeAtDetector(StDetectorId(kBarrelEmcTowerId));
    if (debug) cout <<"++++++++++++ StSkimPionMaker::getPoint: tower, t, n, e, pt ---> "<<id<<" "<<t<<" "<<n<<" "<<e<<" "<<pt<< endl;
  
    return pointVector;
}


//________________________________________________________________________________________________________

Int_t StSkimPionMaker::doTrackPtHist(float eT, float threshold, TObjArray *photonlist)
{
    if (debug) cout <<"++++++++++++ StSkimPionMaker::doTrackPtHist: entered"<<endl;
    
    StEvent *event = (StEvent*)this->GetInputDS("StEvent");
    if (!event)
	{
	    if (debug) cout << "StSkimPionMaker::doTrackPtHist: Can't get Event pointer" << endl;
	    return kStOk;
	}
    
    // tracks
    int numberOfPrimaries= mMuDst->primaryTracks()->GetEntries();
    int numberOfGlobals= mMuDst->globalTracks()->GetEntries();
    
    StMuTrack* track;
    StThreeVectorF trackMomentum;
    
    for (int i = 0; i < numberOfGlobals; i++)
	{
	    track = mMuDst->globalTracks(i);
	    if (track && track->flag()>=0) {
		trackMomentum = track->p();
	    }
	    
	}

    if (debug) cout <<"++++++++++++ StSkimPionMaker::doTrackPtHist: got tracks, try to get points"<<endl;
    
    // EMC points
    for (Int_t i=0; i<photonlist->GetEntries(); i++)
	{
	    StEmcPoint *p = (StEmcPoint *) photonlist->At(i);
	    const StThreeVectorF& pointVector = p->position();
	    
	    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
	    //emcGeom = StEmcGeom::instance("bemc");
	    Int_t mod, eta, sub;
	    emcGeom->getBin(pointVector.phi(), pointVector.pseudoRapidity(), mod, eta, sub);
	    //emcGeom->getId(mod, eta, sub, id);
	    
	    Float_t e = p->energy();
	    
	    // get photon pT
	    Float_t theta = 0.;
	    emcGeom->getTheta(mod, eta, theta);
	    Float_t pt = e * sin(theta);	    
	}
    
    return kStOk;
}


//________________________________________________________________________________________________________
Float_t StSkimPionMaker::getHiTowerEt(StEmcCollection* emccol)
{
    // get the Et of the highest tower
    if (debug) cout <<"++++++++++++ StSkimPionMaker::getHiTowerEt: entered"<<endl;
    
    // Energy is converted to Et assuming z(vertex)=0
    // because this is how it's done in the trigger
    
    StEmcDetector* bemc = emccol->detector(kBarrelEmcTowerId);
    if (!bemc)
	{
	    if (debug) cout <<"++++++++++ StSkimPionMaker::getHiTowerEt(): No BEMC detector"<<endl;
	    return 0;
	}
    
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    Float_t hiTowerEt = 0;
    Float_t e, eT, theta;
    
    for (UInt_t i=1; i<=bemc->numberOfModules(); i++)
	{
	    StEmcModule* module = bemc->module(i);
	    StSPtrVecEmcRawHit& hits = module->hits();
	    StSPtrVecEmcRawHitIterator hIter;
    
	    for (hIter=hits.begin(); hIter!=hits.end(); hIter++)
		{
		    //YYY
		    e = (*hIter)->energy();
		    emcGeom->getTheta((*hIter)->module(),(*hIter)->eta(), theta);
		    eT = e * sin(theta);
		    
		    if (eT > hiTowerEt) hiTowerEt = eT;
		}
	} 
    return hiTowerEt;
}

///////////////////////////////////////////////////////// conversion done!	

//________________________________________________________________________________________________________
Int_t StSkimPionMaker::associateTracksWithEmcPoints(StMaker* anyMaker)
{
    // What it does:
    //
    // 1. Runs through all "good" global tracks. Each track is projected on BEMC using
    //    StEmcPosition::trackOnEmc(&pos, &mom, track, BFIELD);
    //
    // 2. Finds corresponding emc point
    //
    // 3. If such point exists, adds pointer to this track to the
    //    vector of tracks StPtrVecTrack (data member of StEmcPoint)
    
    if (debug) cout <<"++++++++++++ StSkimPionMaker::associateTracksWithEmcPoints: ENTERED"<<endl;
    
    StEvent *event = (StEvent*)anyMaker->GetInputDS("StEvent");
    if (!event)
	{
	    if (debug) cout << "++++++++++++ StSkimPionMaker::associateTracksWithEmcPoints: Can't get Event pointer" << endl;
	    return kStOk;
	}
    StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
    if (!emccol)
	{
	    if (debug) cout << "++++++++++++ StSkimPionMaker::associateTracksWithEmcPoints: No EMC Collection" << endl;
	    return kStOk;
	}
    StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
    if (bEmcPoints.size()==0)
	{
	    //cout << "++++++++++++ StSkimPionMaker::associateTracksWithEmcPoints: No BEMC points found" << endl;
	    return kStOk;
	}
  
    // reset all existing track-to-point associations
    for (StSPtrVecEmcPointIterator it=bEmcPoints.begin(); it!=bEmcPoints.end(); it++)
	(*it)->track().clear(); 
    // deletes all elements
    // this is NOT a structural container, so the tracks don't get deleted
  
  
    Double_t bFld = mField;
  
    Int_t mod, eta, sub;
    StEmcPosition* pos = new StEmcPosition();
    StThreeVectorD position, momentum;
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    // tracks
    int numberOfPrimaries= mMuDst->primaryTracks()->GetEntries();
    int numberOfGlobals= mMuDst->globalTracks()->GetEntries();
  
    StMuTrack* track;
    StTrack* dummyTrack = new StGlobalTrack();
    StThreeVectorF trackMomentum;
  
    for (int i = 0; i < numberOfGlobals; i++)
	{
	    track = mMuDst->globalTracks(i);
	    if (track && track->flag()>=0) {
		Bool_t ok = pos->trackOnEmc(&position, &momentum, track, bFld); // bFld in Tesla
		if (!ok) continue;
	      
		// get (mod, eta, sub) from track
		emcGeom->getBin(position.phi(), position.pseudoRapidity(), mod, eta, sub);
	      
		for (StSPtrVecEmcPointIterator it=bEmcPoints.begin(); it!=bEmcPoints.end(); it++)
		    {	    
			StPtrVecEmcCluster bEmcClusters = (*it)->cluster(kBarrelEmcTowerId);
		      
			for (StPtrVecEmcClusterIterator cIter=bEmcClusters.begin(); cIter!=bEmcClusters.end(); cIter++)
			    {
				StPtrVecEmcRawHit& bEmcHits = (*cIter)->hit();
			      
				for (StPtrVecEmcRawHitIterator hIter=bEmcHits.begin(); hIter!=bEmcHits.end(); hIter++)
				    {
					// compare (mod, eta, sub) with emc hit	    
					if (mod == (Int_t)(*hIter)->module() && 
					    eta == (Int_t)(*hIter)->eta() && 
					    sub == (Int_t)(*hIter)->sub())
					    {
						//cout << "Added Track to association! \n";
						(*it)->addTrack(dummyTrack);
						break;
					    }
				    }
			    }
		    }
	      
	    }
	}
    delete pos;
    delete dummyTrack;
    return kStOk;
}

//--------------------------------------------------------------------------------------------------------
Float_t StSkimPionMaker::getNeutralEnergySum(TObjArray *photonlist)
{
    Float_t eSum = 0;
    Int_t cntb=0;
    Int_t id1, n1, t1;
    Float_t e1, pt1;
    Float_t eSMDe1, eSMDp1;
    Float_t eTower1;
    Float_t sSMDe1, sSMDp1, sTower1;
    
    for (Int_t i=0; i<photonlist->GetEntries(); i++)
	{
	    StEmcPoint *p1 = (StEmcPoint *) photonlist->At(i);
	    StThreeVectorF v1 = getPoint(p1, id1,e1,pt1,n1,t1, eSMDe1, eSMDp1, eTower1,sSMDe1, sSMDp1, sTower1);
	    if ((n1 == 0)&&(id1 < 4801))
		eSum += e1;
	    if (v1(0) == -10000.)
		eSum = 0;
	}
    
    return eSum;
}

//________________________________________________________________________________________________________
void StSkimPionMaker::getPhotonSpectra(TObjArray *photonlist, int runnm, float vertex_z, int bemc_hits)
{
    // fill histograms with photon information
    
    if (debug) cout <<"++++++++++ StSkimPionMaker::GetPhotonSpectra: entered"<<endl;
    
    Int_t cntb=0;
    Int_t id1, n1, t1;
    Float_t e1, pt1;
    Float_t eSMDe1, eSMDp1;
    Float_t eTower1;
    Float_t sSMDe1, sSMDp1, sTower1;
    
    Float_t pt1_trg=0;
    Int_t id1_trg=0;	  
    
    for (Int_t i=0; i<photonlist->GetEntries(); i++)
	{
	    StEmcPoint *p1 = (StEmcPoint *) photonlist->At(i);
	    
	    // get EMC point information 
	    StThreeVectorF v1 = getPoint(p1, id1,e1,pt1,n1,t1, eSMDe1, eSMDp1, eTower1,sSMDe1, sSMDp1, sTower1);
	    if (v1(0) == -10000.)
		     return;
	    //cout <<"--------------------> pt1 = " <<pt1<<endl;
	    
	    if (debug) cout << "Photon track association: " << n1 << endl;
	    
	    if (n1==0)
		{
		    if (pt1>pt1_trg)
			{ 
			    pt1_trg = pt1;
			    id1_trg = id1;
			}
		    
		    // count number of photons
		    cntb++;
		}
	}    
    return;
}

//________________________________________________________________________________________________________
void StSkimPionMaker::getInvMass(int mode, TObjArray *photonlist1, TObjArray *photonlist2, 
				 float vertex_z, StThreeVectorF primaryVertex, int triggers[3], int prescales[3])
{
    if (debug) cout <<"++++++++++ StSkimPionMaker::getInvMass: list1 "<<photonlist1->GetEntries()<<endl;
    if (debug) cout <<"++++++++++ StSkimPionMaker::getInvMass: list2 "<<photonlist2->GetEntries()<<endl;
    
    Float_t chargedAssociation1 = 0;
    Float_t chargedAssociation2 = 0;
    
    // 1st particle
    for (Int_t i=0; i<photonlist1->GetEntries(); i++)
	{
	    if (debug) cout <<"++++++++++ StSkimPionMaker::getInvMass: first loop (mode "<<mode<<")"<<endl;
	    StEmcPoint *p1 = (StEmcPoint *) photonlist1->At(i);
	    
	    Int_t id1, n1, t1;
	    t1 = 0;
	    Float_t e1, pt1;
	    Float_t eSMDe1;
	    Float_t eSMDp1;
	    Float_t eTower1;
	    Float_t sSMDe1, sSMDp1, sTower1;
	    
	    StThreeVectorF v1 = getPoint(p1, id1,e1,pt1,n1,t1, eSMDe1, eSMDp1, eTower1,sSMDe1, sSMDp1, sTower1);
	    if (v1(0) == -10000.)
		return;
	    chargedAssociation1 = n1;
	    
	    // here I fill the hit container for Pi0Event
	    Float_t hitdat[16];
	    hitdat[0] = v1.x();
	    hitdat[1] = v1.y();
	    hitdat[2] = v1.z();
	    hitdat[3] = id1;
	    hitdat[4] = e1;
	    hitdat[5] = pt1;
	    hitdat[6] = n1;
	    hitdat[7] = t1;
	    hitdat[8] = eSMDe1;
	    hitdat[9] = eSMDp1;
	    hitdat[10] = eTower1;
	    hitdat[11] = sSMDe1;
	    hitdat[12] = sSMDp1;
	    hitdat[13] = sTower1;
	    hitdat[14] = v1.phi();
	    hitdat[15] = v1.pseudoRapidity();
	    
	    THit hitCand;
	    hitCand.SetAll(hitdat);
	    pi0Event->AddHit(hitCand);
	    
		
	    //cout << "!!!!!!!!!!! HitRadius1: " << v1.perp() << endl;
	    // 2nd particle
	    if (debug) cout <<"++++++++++ StSkimPionMaker::getInvMass: second loop (mode "<<mode<<")"<<endl; 
	    
	    Int_t j = 999;
	    if (mode==0) j=i+1;
	    if (mode==1) j=0;
	    
	    for (Int_t k=j; k<photonlist2->GetEntries(); k++)
		{
		    StEmcPoint *p2 = (StEmcPoint *) photonlist2->At(k);
		    
		    Int_t id2, n2, t2;
		    t2 = 0;
		    Float_t e2, pt2;
		    Float_t eSMDe2;
		    Float_t eSMDp2;
		    Float_t eTower2;
		    Float_t sSMDe2, sSMDp2, sTower2;
		    
		    StThreeVectorF v2 = getPoint(p2, id2,e2,pt2,n2,t2, eSMDe2, eSMDp2, eTower2,sSMDe2, sSMDp2, sTower2);
		    if (v2(0) == -10000)
			return;
		    chargedAssociation2 = n2;
		    //cout << "!!!!!!!!!!! HitRadius2: " << v2.perp() << endl;
		    // Pi0 CUTS 
		    //if (n1==0 && n2==0 && t1>-99 && t2>-99) // the n==0 is the requirement that no charged track leads to the EMC point
		    if (t1>-99 && t2>-99) // the n==0 is the requirement that no charged track leads to the EMC point
			{
			    if (debug) cout <<"++++++++++ StSkimPionMaker::getInvMass: get invariant mass for photon pair"<<endl;
			    
			    
			    // Up to here, the vectors are in global coordinates (e.g. not wrt the primary vertex)
			    // for the mass calculations, this should change! 
			    // define new vector relative to event vertex:
			    
			    StThreeVectorF v1rel; 
			    StThreeVectorF v2rel; 
			    StThreeVectorF vdist = v1 - v2;
			    
			    if (mode == 0) {
				v1rel = v1 - primaryVertex;
				v2rel = v2 - primaryVertex;
			    }
			    else {
				v1rel = v1;
				v2rel = v2;
			    }
			    
			    Float_t dist = vdist.mag();
			    double mInvPi0; 
			    StThreeVectorF pPi0;
			    Float_t asym;
			    Float_t cosAng;
			    Float_t phi1;
			    Float_t phi2;
			    
			    if (debug) cout << "Pi0 candidate: tower1: " << id1 << " tower2: " << id2 << "  triggerTower: " << triggerTower << endl;
			    getMass(v1rel,v2rel,e1,e2, mInvPi0,pPi0,asym,cosAng, phi1,phi2); // use v1 and v2 instead of v1rel and v2rel if you
			    // assume that the photons come from 0,0,0
			    
			    // get Pi0 minv distribution for a certain set of cuts

			    // fill array for Pi0Candidate;
			    Float_t cdat[24];
			    cdat[0]= pPi0.perp();
			    cdat[1] = mInvPi0;
			    cdat[2] = pPi0.pseudoRapidity();
			    cdat[3] = pPi0.phi();
			    cdat[4] = asym;
			    cdat[5] = cosAng;
			    cdat[6] = id1;
			    cdat[7] = id2;
			    cdat[8] = chargedAssociation1;
			    cdat[9] = chargedAssociation2;
			    cdat[10] = t1;
			    cdat[11] = t2;
			    cdat[12] = e1;
			    cdat[13] = e2;
			    cdat[14] = eTower1;
			    cdat[15] = eTower2;
			    cdat[16] = eSMDe1;
			    cdat[17] = eSMDp1;
			    cdat[18] = eSMDe2;
			    cdat[19] = eSMDp2;
			    cdat[20] = v1.pseudoRapidity(); //v1rel.pseudoRapidity();
			    cdat[21] = v1.phi(); //v1rel.phi();
			    cdat[22] = v2.pseudoRapidity(); //v2rel.pseudoRapidity();
			    cdat[23] = v2.phi(); //v2rel.phi();
			    
			    if ((mode==0)&&(cdat[0] > 0.5)) {
				TSkimPionCandidate piCand;
				piCand.SetAll(cdat);
				pi0Event->AddSkimPionCandidate(piCand);
			    }

			}
		}
	}
    return;
}


//________________________________________________________________________________________________________
Bool_t StSkimPionMaker::getMass(StThreeVectorF P1, StThreeVectorF P2, Float_t e1, Float_t e2, double &mInv, StThreeVectorF &pPi0, Float_t &asym, Float_t &cosAng, Float_t &phi1, Float_t &phi2)
{
    if (debug) cout << "++++++++++++ StSkimPionMaker::getMass(): enter pi0 minv calcualtion..." << endl;
    
    Double_t VecProd = P1.x()*P2.x() + P1.y()*P2.y() + P1.z()*P2.z();
    cosAng = VecProd/(P1.magnitude()*P2.magnitude());
    
    double mInv2 = 2*e1*e2*(1-cosAng);      
    if (mInv2>0) 
	{
	    mInv = TMath::Sqrt(mInv2);
	}
    else 
	mInv = -999.0;
    
    asym = fabs(e1-e2)/(e1+e2);
    
    // for correlation studies
    phi1 = P1.phi();
    phi2 = P2.phi();
    
    pPi0 = e1*P1/P1.mag() + e2*P2/P2.mag();
    
    if (debug) cout <<"++++++++++++ StSkimPionMaker::getMass(): mInv, asym, pPi0: "<<mInv<<" "<<asym<<" "<<pPi0<<endl;
    
    return kTRUE;
}      


//________________________________________________________________________________________________________
// Not sure if this is ever called.
void StSkimPionMaker::getTowerHitInfo()
{
    StEvent *event = (StEvent*)this->GetInputDS("StEvent");
    if (!event)
	{
	    if (debug) cout << "++++++++++++ StSkimPionMaker::getTowerHitInfo(): Can't get Event pointer" << endl;
	    return;
	}
    StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
    if (!emccol)
	{
	    if (debug) cout << "++++++++++++ StSkimPionMaker::getTowerHitInfo():  No EMC Collection" << endl;
	    return;
	}
    StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
    if (bEmcPoints.size()==0)
	{
	    //cout << "++++++++++++ StSkimPionMaker::getTowerHitInfo():  No BEMC points found" << endl;
	    return;
	}
    
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    
    // for(Int_t det=0; det<4; det++) det=0
    StDetectorId id = static_cast<StDetectorId>(0+kBarrelEmcTowerId);
    StEmcDetector* detector = emccol->detector(id);
    if (detector)
	{
	    for (UInt_t j=1; j<=120; j++)
		{
		    StEmcModule* module = detector->module(j);
		    if (module)
			{
			    StSPtrVecEmcRawHit& rawHit = module->hits();
			    for (Int_t k=0; k<(Int_t)rawHit.size(); k++)
				{
				    int tid;
				    emcGeom->getId(rawHit[k]->module(),rawHit[k]->eta(),rawHit[k]->sub(),tid);
				}
			}
		}
	} 
    else 
	if (debug) cout << "++++++++++ StSkimPionMaker::getTowerHitInfo(): no detector" << endl;
    
    return;
}

//________________________________________________________________________________________________________
Bool_t StSkimPionMaker::readPointList()
{
    // read all EMC points and fill them into the list of photon candidates
    // in addition, fill ntuple with point information for QA purposes
    
    StEvent *event = (StEvent*)this->GetInputDS("StEvent");
    if (!event)
	{
	    if (debug) cout << "++++++++++++ StSkimPionMaker::readPointList(): Can't get Event pointer" << endl;
	    return kStWarn;
	}
    StEmcCollection* emccol = (StEmcCollection*)event->emcCollection();
    if (!emccol)
	{
	    if (debug) cout << "++++++++++++ StSkimPionMaker::readPointList():  No EMC Collection" << endl;
	    return kStOk;
	}
    StSPtrVecEmcPoint& bEmcPoints = emccol->barrelPoints();
    if (bEmcPoints.size()==0)
	{
	    //cout << "++++++++++++ StSkimPionMaker::readPointList():  No BEMC points found" << endl;
	    return kStOk;
	}
    if (debug) cout << "++++++++++++ StSkimPionMaker::readPointList(): Number of BEMC points: " << bEmcPoints.size() << endl;
    
    // Ok, now we've got valid points, let's go on and do something with it, like filling the photon list and the ntuple
    
    StEmcPoint* point=0;
    for (StSPtrVecEmcPointIterator it=bEmcPoints.begin(); it!=bEmcPoints.end(); it++)
	{
	    point = *it;
	    photonlist->Add(point);
	    // fill the EMC point ntuple for each entry
	    const StThreeVectorF& pointVector = point->position();
	}
    
    if (debug) cout <<"++++++++++++ StSkimPionMaker::readPointList(): StEmcPoint point list: "<< photonlist->GetEntries() << endl;
    //cout <<"++++++++++++ StSkimPionMaker::readPointList(): StEmcPoint dummy list: "<< dummylist->GetEntries() << endl;
    
    if (photonlist->GetEntries()<=0)
	{
	    photonlist->Clear(); 
	    return kStOk;    
	}
    return true;
}

