/***************************************************************************
 *
 * $Id: StRichSpectraMaker.cxx,v 1.2 2001/02/25 22:11:46 lasiuk Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRichSpectraMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Uses the information in the pidtraits to produce a spectrum
 ***************************************************************************
 *
 * $Log: StRichSpectraMaker.cxx,v $
 * Revision 1.2  2001/02/25 22:11:46  lasiuk
 * quality assessment
 *
 * Revision 1.1  2000/12/12 21:35:08  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StRichDisplayActivate.h"
#include "StRichSpectraMaker.h"

#include <iostream.h>
#include <fstream.h>
#include <assert.h>

#include "StChain.h"
#include "St_DataSetIter.h"

#include "StGlobals.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

#define VERBOSE 0
#define ivb if(VERBOSE)cout
// StEvent
#include "StEventTypes.h"
#include "StRichPidTraits.h"
#include "StRichPid.h"
#include "StRichPhotonInfo.h" // must be in StEventTypes.h

#ifdef RICH_WITH_PAD_MONITOR
#include "StRrsMaker/StRichSinglePixel.h"
#include "StRichDisplayMaker/StRichPadMonitor.h"
#endif

// Database
#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichMomentumTransform.h"

// Internal Rch
#include "StRchMaker/StRichSimpleHit.h"

#include "StRichPIDMaker/StRichTrack.h"

ClassImp(StRichSpectraMaker) // macro
   
//-----------------------------------------------------------------

StRichSpectraMaker::StRichSpectraMaker(const char *name)
    : StMaker(name)
{
    //
    // Switches in the .h file
    //

    mNumberOfEvents = 0;
    mNumberOfGood2GevTracks = 0;
}

//-----------------------------------------------------------------

StRichSpectraMaker::~StRichSpectraMaker() {/* nopt */}

//-----------------------------------------------------------------

Int_t StRichSpectraMaker::Init() {
    cout << "StRichSpectraMaker::init()" << endl;
    
#ifdef RICH_SPECTRA_HISTOGRAM
    mFile = new TFile("/star/rcf/scratch/lasiuk/production/Spectra.root","RECREATE","Pid Ntuples");
    mFile->SetFormat(1);

    
    mPhotons = new TNtuple("photon", "pid", "evt:vtx:p:pt:theta:res:hyp:pipsi:pig:pia:pid:pitpsi:pitg:pita:pitd:pi1sig:pi2sig:pi1sigc:pi2sigc:piq:kpsi:kg:ka:kd:ktpsi:ktg:kta:ktd:k1sig:k2sig:k1sigc:k2sigc:kq:ppsi:pg:pa:pd:ptpsi:ptg:pta:ptd:p1sig:p2sig:p1sigc:p2sigc:pq:charge:piga:kga:pga:uvpi:uvk:uvp");

    //
    // event#, vertx.z global px,py,pz, pt p, local px,py,pz, theta (incident) residual
    // pi     Total: psi, #photon, area, density
    //    Truncated: psi, #photon, area, density 1sig, 2sig, 1sigconstant, 2sigconst
    // K
    // p

    mCorrected = new TNtuple("cphoton","pid2", "evt:vtx:p:pt:theta:res:pipsi:pig:pid:pi1sig:pi2sig:kpsi:kg:kd:k1sig:k2sig:ppsi:pg:pd:p1sig:p2sig:q:pia:pita:ka:kta:pa:pta");

    mEvt = new TNtuple("evt","evt1","p:pt:q:res:theta:lx:ly:lz:lp:rx:ry");

    mCerenkov = new TNtuple("cer","angle","no:px:py:pz:d:sigma:phi:x:y:c");
#endif

    this->initCutParameters();

    mGeometryDb = StRichGeometryDb::getDb();
    mMomTform  = StRichMomentumTransform::getTransform(mGeometryDb);  

    //
    // make the particles
    //
    mPion = StPionMinus::instance();
    mKaon = StKaonMinus::instance();
    mProton = StAntiProton::instance();

    mIndex = 1.28; // should take from materials db
    
    this->printCutParameters();

    return StMaker::Init();
}

void StRichSpectraMaker::initCutParameters() {
    //
    // Event Level
    //
    mVertexWindow = 30.*centimeter;
    
    //
    // Track Level
    //
    mPtCut = 0.*GeV; // GeV/c
    mEtaCut = 0.5; 
    mLastHitCut = 160.0*centimeter;
    mDcaCut = 3.0*centimeter;
    mFitPointsCut = 20;
    mPathCut = 500*centimeter;
    mPadPlaneCut = 2.0*centimeter;
    mRadiatorCut = 2.0*centimeter;

    mMomentumThreshold = 2.*GeV;
    mMomentumLimit = 3.*GeV;
}

//-----------------------------------------------------------------

Int_t StRichSpectraMaker::Make() {
    cout << "StRichSpectraMaker::Make()" << endl;
    mNumberOfEvents++;
    //
    // ptr initialization for StEvent
    //
    mTheRichCollection = 0;
    
    //
    // Try get StEvent Structure
    //
    mEvent = (StEvent *) GetInputDS("StEvent");

    //
    // Interogate StEvent structure
    //    
    if (!mEvent) {
	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tWARNING!!\n";
	cout << "\tCannot Get the StEvent*\n";
	cout << "\tReturn to chain" << endl;
	return kStWarn;
    }

    //
    // Check the RICH collection
    //
    if(!mEvent->richCollection()) {
	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tWARNING!!\n";
	cout << "\tCannot Get the StRichCollection*\n";
 	cout << "\tReturn to chain" << endl;
 	return kStWarn;
    }
    
    
    mMagField    = 2.49117;    
    if (mEvent->summary()) {
	mMagField  = mEvent->summary()->magneticField();
	cout << "  B field = " << mMagField << endl;
    } 
    else {
	cout << "StRichSpectraMaker::Make().\n";
	cout << "\tWARNING!\n";
	cout << "\tCannot get B field from mEvent->summary().\n";
	cout << "\tUse B= " << mMagField << endl;
    } 

    //
    // Vertex Position
    //
    if(!mEvent->primaryVertex()) {
    	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tWARNING!!\n";
	cout << "\tEvent has no Primary Vertex\n";
	cout << "\tReturn to chain" << endl;
	return kStWarn;
    }
    mVertexPos = mEvent->primaryVertex()->position();
    
    //
    // Number of tracks to loop over
    //
    mNumberOfPrimaries = mEvent->primaryVertex()->numberOfDaughters();  
    PR(mNumberOfPrimaries);
    //
    // does the hit collection exist
    //
    if(mEvent->tpcHitCollection()) {
	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tHit collection exists" << endl;
    }
    else {
	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tWARNING\n";
	cout << "\tHit collection DOES NOT exist!!!!" << endl;
	cout << "\tContinuing..." << endl;
    }

#ifdef RICH_WITH_PAD_MONITOR
    cout << "StRichSpectraMaker::Next Event? <ret>: " << endl;
    do {
	if(getchar()) break;
    } while (true);
    
    mPadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
    mPadMonitor->clearAll();
    
    this->drawRichPixels(mEvent->richCollection());
    this->drawRichHits(mEvent->richCollection());
#endif

     this->qualityAssessment();

    //
    //
    // The track loop
    //
    //

    cout << "Looping over " << mNumberOfPrimaries << " primary Tracks" << endl;
    for(size_t ii=0; ii<mNumberOfPrimaries; ii++) { // primaries

// 	cout << "==> Track " << ii << "/" << (mNumberOfPrimaries-1);
	StTrack* track = mEvent->primaryVertex()->daughter(ii);
// 	cout << " p= " << track->geometry()->momentum().mag() << endl;

	//if( !this->checkMomentumWindow(track) ) continue;
// 	if (!this->checkTrack(track)) continue;
	
	//
	// Get the PID traits, if there is an StrichPIDTrait:
	//
	const StPtrVecTrackPidTraits&
	    theRichPidTraits = track->pidTraits(kRichId);

	if(!theRichPidTraits.size()) continue;
  	cout << " (" << theRichPidTraits.size() << ") Pid Traits.   p= ";

	cout << (track->geometry()->momentum().mag()) << endl;

	StRichTrack* richTrack = new StRichTrack(track,mMagField);
	
// 	richTrack->assignMIP(&(mEvent->richCollection()->getRichHits()));
	
// 	StThreeVectorF projectedMIP  = richTrack->getProjectedMIP();
// 	if(!richTrack->getAssociatedMIP()) {
// 	    cout << "StRichSpectraMaker::Make()\n";
// 	    cout << "\tNo Associated MIP\n";
// 	    cout << "\tSkip this track" << endl;
// 	    delete richTrack;
// 	    continue;
// 	}	    
// 	StThreeVectorF associatedMIP = richTrack->getAssociatedMIP()->local();
// 	PR(projectedMIP);
// 	PR(associatedMIP);
// 	double residual = (projectedMIP - associatedMIP).perp();
// 	PR(residual);
	
// 	StRichPidTraits *richPidTrait =
// 	    dynamic_cast<StRichPidTraits*>(theRichPidTraits[0]);

// 	cout << "theRichPidTraits.begin()" << endl;

	StTrackPidTraits* theMostRecentTrait =
	    theRichPidTraits[theRichPidTraits.size()-1];
	
	StRichPidTraits *richPidTrait =
	    dynamic_cast<StRichPidTraits*>(theMostRecentTrait);

	//
	//
	//

	if( track->geometry()->momentum().mag()  < 1) {
	    delete richTrack;
	    continue;
	}
	this->evaluateEvent(richTrack, richPidTrait);
	
	if( !this->checkMomentumWindow(track) ) {
	    delete richTrack;
	    continue;
	}
	this->lookAtPhotonInfo(richTrack, richPidTrait);
	
	//
	//
	//
	
	delete richTrack;
	
    } // loop over the tracks

    
    return kStOk;
}


////////////////////////////////////////////////
////////////////////////////////////////////////
void StRichSpectraMaker::lookAtPhotonInfo(StRichTrack* richTrack,
					  StRichPidTraits* richPidTraits) {

    StTrack* track = richTrack->getStTrack();
    
    const int theTupleSize = 53;
    mTupleSize = theTupleSize;
    float tuple[theTupleSize];

    const int theTuple2Size = 28;
    mTuple2Size = theTuple2Size;
    float tuple2[theTuple2Size];
    

    
    for(int ii=0; ii<mTupleSize;ii++)
	tuple[ii] = -990;

    tuple[0] = mEvent->id();
    PR(mEvent->id());
    tuple[1] = mVertexPos.z();

    StThreeVectorF trackMomentum = track->geometry()->momentum();
    PR(trackMomentum.mag());
    tuple[2] = trackMomentum.mag();
    tuple[3] = trackMomentum.perp();

    tuple[4] = richTrack->getTheta()/degree;

    const StSPtrVecRichPid& theRichPids = richPidTraits->getAllPids();

    //
    cout << "theRichPids.size()= " << theRichPids.size() << endl;
    if(!theRichPids.size()) {
	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tWARNING:\n";
	cout << "\tSkip this track" << endl;
	cout << "\tReturning..." << endl;
	return;
    }

    PR((theRichPids[0]->getMipResidual()).perp());
    PR( abs(theRichPids[0]->getMipResidual()) );
    tuple[5] = (theRichPids[0]->getMipResidual().perp());
	
    // loop over the hypothesis
    tuple[6] = theRichPids.size();

    PR(theRichPids.size());
    for(size_t jj=0; jj<theRichPids.size(); jj++) { // pids

	cout << "type= " << theRichPids[jj]->getParticleNumber() << " ";

	float expectedUV =
	    this->expectedNumberOfPhotons(trackMomentum.mag(),
					  theRichPids[jj]->getParticleNumber());

	//
	// try the associatedhits
	//
	StPtrVecRichHit& associatedHits =
	    theRichPids[jj]->getAssociatedRichHits();
	
	const StSPtrVecRichPhotonInfo& infos =
	    theRichPids[jj]->getPhotonInfo();

	float cAngle  = theRichPids[jj]->getConstantAreaCut(); 
	PR(cAngle);
	    
	cout << "associatedHits + infos "
	     << associatedHits.size() << " " << infos.size() << endl;

	assert(associatedHits.size() == infos.size());

	int photonsInArea        = 0;
	int totalOneSigmaHits    = 0;
	int totalTwoSigmaHits    = 0;
	int consAreaOneSigmaHits = 0;
	int consAreaTwoSigmaHits = 0;
	float totalCharge        = 0;
	float constAngleAndArea  = 0;
	    
	size_t kk;
	for(kk=0; kk<infos.size(); kk++) { // info loop
	    
	    bool inArea   = false;
	    bool inCAngle = false;
	    
	    float d       = infos[kk]->d();
	    float sigma   = infos[kk]->sigma();
	    float azimuth = infos[kk]->azimuth();
	    
	    if( fabs(azimuth) > cAngle ) {
		inCAngle = true;
	    }
	    
	    if( (d>0) && (d<1) ) {
		inArea = true;
		photonsInArea++;
		
		totalCharge += associatedHits[kk]->charge();
	    }
	    
	    
	    if(inCAngle) {
		if( fabs(sigma)<= 1 )
		    totalOneSigmaHits++;
		
		if( fabs(sigma)<= 2 )
		    totalTwoSigmaHits++;
	    }
	    
	    
	    if(inCAngle && inArea) {
		constAngleAndArea++;
		
		if( fabs(sigma)<= 1 )
		    consAreaOneSigmaHits++;
		
		if( fabs(sigma)<= 2 )
		    consAreaTwoSigmaHits++;
	    }
	    
	} // info loop
	
	for(kk=0; kk<infos.size(); kk++) { // info loop
	    
	    switch(theRichPids[jj]->getParticleNumber()) {
	    case -211:
		    
		tuple[7] = theRichPids[jj]->getTotalAzimuth();
		tuple[8] = theRichPids[jj]->getTotalHits();
		tuple[9] = theRichPids[jj]->getTotalArea();
		tuple[10] = theRichPids[jj]->getTotalDensity();
		
		tuple[11] = theRichPids[jj]->getTruncatedAzimuth();
		tuple[12] = theRichPids[jj]->getTruncatedHits();
		tuple[13] = theRichPids[jj]->getTruncatedArea();
		tuple[14] = theRichPids[jj]->getTruncatedDensity();
		
		tuple[15] = totalOneSigmaHits;
		tuple[16] = totalTwoSigmaHits;
		tuple[17] = consAreaOneSigmaHits;
		tuple[18] = consAreaTwoSigmaHits;
		tuple[19] = totalCharge;
		tuple[47] = photonsInArea;
		tuple[50] = expectedUV;
		
		break;
		
	    case -321:
		tuple[20] = theRichPids[jj]->getTotalAzimuth();
		tuple[21] = theRichPids[jj]->getTotalHits();
		tuple[22] = theRichPids[jj]->getTotalArea();
		tuple[23] = theRichPids[jj]->getTotalDensity();
		
		tuple[24] = theRichPids[jj]->getTruncatedAzimuth();
		tuple[25] = theRichPids[jj]->getTruncatedHits();
		tuple[26] = theRichPids[jj]->getTruncatedArea();
		tuple[27] = theRichPids[jj]->getTruncatedDensity();
		
		tuple[28] = totalOneSigmaHits;
		tuple[29] = totalTwoSigmaHits;
		tuple[30] = consAreaOneSigmaHits;
		tuple[31] = consAreaTwoSigmaHits;
		tuple[32] = totalCharge;
		tuple[48] = photonsInArea;
		tuple[51] = expectedUV;
		
		break;
		
	    case -2212:
		tuple[33] = theRichPids[jj]->getTotalAzimuth();
		tuple[34] = theRichPids[jj]->getTotalHits();
		tuple[35] = theRichPids[jj]->getTotalArea();
		tuple[36] = theRichPids[jj]->getTotalDensity();
		
		tuple[37] = theRichPids[jj]->getTruncatedAzimuth();
		tuple[38] = theRichPids[jj]->getTruncatedHits();
		tuple[39] = theRichPids[jj]->getTruncatedArea();
		tuple[40] = theRichPids[jj]->getTruncatedDensity();
		
		tuple[41] = totalOneSigmaHits;
		tuple[42] = totalTwoSigmaHits;
		tuple[43] = consAreaOneSigmaHits;
		tuple[44] = consAreaTwoSigmaHits;
		tuple[45] = totalCharge;
		tuple[49] = photonsInArea;
		tuple[52] = expectedUV;
		
		break;
		
	    default:
		cout << "theRichPids[jj]->getParticleNumber()"
		     << theRichPids[jj]->getParticleNumber() << endl;
	    }
	} // loop over the pids

    }  // loop over the pids
    //
    // write the ntuple
    tuple[46] = track->geometry()->charge();
	
    mPhotons->Fill(tuple);

#ifdef RICH_WITH_PAD_MONITOR
    mPadMonitor->addTrack(richTrack);
    mPadMonitor->drawRings();
    mPadMonitor->update();
#endif

    for(int jj=0; jj<mTuple2Size; jj++)
	tuple2[jj] = -990;
	
    tuple2[0] = tuple[0];
    tuple2[1] = tuple[1];
    tuple2[2] = tuple[2];
    tuple2[3] = tuple[3];
    tuple2[4] = tuple[4];
    tuple2[5] = tuple[5];
    
    //
    // pion
    //
    if(tuple[7] > 0) {
	tuple2[6] = tuple[11]/tuple[7];                     // Azimuth Fraction
	tuple2[7] = (tuple[12]*2*M_PI/tuple[7])/tuple[50];  // Az/Mom corrected photons
	tuple2[8] = (tuple[14]*2*M_PI/tuple[7])/tuple[50];  // Az/Mom corrected density
	tuple2[9] = (tuple[17]*2*M_PI/tuple[7])/tuple[50];  // 1 sigma
	tuple2[10] = (tuple[18]*2*M_PI/tuple[7])/tuple[50]; // 2 sigma
	tuple2[22] = tuple[9];                              // area
	tuple2[23] = tuple[13];                             // truncated area
    }
    
    
    //
    // kaon
    //
    if(tuple[24] > 0) {
	tuple2[11] = tuple[24]/tuple[20];                     // Azimuth Fraction
	tuple2[12] = (tuple[25]*2*M_PI/tuple[24])/tuple[51];  // Az/Mom corrected photons
	tuple2[13] = (tuple[27]*2*M_PI/tuple[24])/tuple[51];  // Az/Mom corrected density
	tuple2[14] = (tuple[30]*2*M_PI/tuple[24])/tuple[51];  // 1 sigma
	tuple2[15] = (tuple[31]*2*M_PI/tuple[24])/tuple[51];  // 2 sigma
	tuple2[24] = tuple[22];                               //area;
	tuple2[25] = tuple[26];                               //tarea;
    }
    
    //
    // proton
    //
    if(tuple[37] > 0) {
	tuple2[16] = tuple[37]/tuple[33];                     // Azimuth Fraction
	tuple2[17] = (tuple[38]*2*M_PI/tuple[37])/tuple[52];  // Az/Mom corrected photons
	tuple2[18] = (tuple[40]*2*M_PI/tuple[37])/tuple[52];  // Az/Mom corrected density
	tuple2[19] = (tuple[43]*2*M_PI/tuple[37])/tuple[52];  // 1 sigma
	tuple2[20] = (tuple[44]*2*M_PI/tuple[37])/tuple[52];  // 2 sigma
	tuple2[26] = tuple[35];                               //area
	tuple2[27] = tuple[39];                               //tarea
    }

    tuple2[21] = track->geometry()->charge();
    mCorrected->Fill(tuple2);
}
////////////////////////////////////////////////
////////////////////////////////////////////////

bool StRichSpectraMaker::evaluateEvent(StRichTrack* richTrack, StRichPidTraits* richPidTraits) {

    StTrack* track = richTrack->getStTrack();

    const int theTupleSize = 11;
    float tuple[theTupleSize];

    tuple[0] = track->geometry()->momentum().mag();
    tuple[1] = track->geometry()->momentum().perp();
    tuple[2] = track->geometry()->charge();

    const StSPtrVecRichPid& theRichPids = richPidTraits->getAllPids();
    tuple[3] = theRichPids[0]->getMipResidual().perp();
    tuple[4] = richTrack->getTheta()/degree;
    
    //
    // Momentum at pad plane
    //
    StThreeVectorF padPlaneMomentum = richTrack->getMomentumAtPadPlane();
    tuple[5] = padPlaneMomentum.x();
    tuple[6] = padPlaneMomentum.y();
    tuple[7] = padPlaneMomentum.z();
    tuple[8] = padPlaneMomentum.mag();

    tuple[9] = theRichPids[0]->getMipResidual().x();
    tuple[10] = theRichPids[0]->getMipResidual().y();
    
    mEvt->Fill(tuple);
    mNumberOfGood2GevTracks++;
    
    return true;
}
//-----------------------------------------------------------------
void StRichSpectraMaker::PrintInfo() 
{
    printf("**************************************************************\n");
    printf("* $Id: StRichSpectraMaker.cxx,v 1.2 2001/02/25 22:11:46 lasiuk Exp $\n");
    printf("**************************************************************\n");
    if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------


Int_t StRichSpectraMaker::Finish() {

    cout << "StRichSpectraMaker::Finish()" << endl;

    cout << mNumberOfGood2GevTracks << " Good 2.0-2.5 GeV tracks in" << endl;
    cout << mNumberOfEvents         << " Events." << endl;
	
    this->printCutParameters();

	
#ifdef RICH_SPECTRA_HISTOGRAM
    if(mFile) {
	cout << "StRichSpectraMaker::Finish()\n";
	cout << "\tClose the Histogram files!!!!!!" << endl;

	mFile->Write();
	mFile->Close();
	delete mFile;
	mFile = 0;
    }
#endif

    return StMaker::Finish();
}

 
bool StRichSpectraMaker::checkTrack(StTrack* track) const {

    //
    // track -- quality cuts
    //       -- momentum and parameter
    //
    bool status = true;
    if (!track) {
	cout << "StRichSpectraMaker::checkTrack()";
	cout << " --> !track" << endl;
	status = false;
    }
    if (track->flag()<0) {
	cout << "StRichSpectraMaker::checkTrack() --> ";
	cout << "track->flag()<0 (" << track->flag() << ")" << endl;
	status = false;
    }

    if (!track->geometry()) {
	cout << "StRichSpectraMaker::checkTrack() --> ";
	cout << "!track->geometry()" << endl;
	status = false;
    }

    if(track->geometry()->helix().distance(mVertexPos)>mDcaCut) {
	cout << "StRichSpectraMaker::checkTrack() --> ";
	cout << "mDcaCut ("
	     << track->geometry()->helix().distance(mVertexPos) << ")" << endl;
	status = false;
    }
	
    if(track->fitTraits().numberOfFitPoints(kTpcId) < mFitPointsCut) {
	cout << "StRichSpectraMaker::checkTrack() --> ";
	cout << "mFitPointsCut ("
	     << track->fitTraits().numberOfFitPoints(kTpcId) << endl;
	status = false;
    }

    if( fabs(track->geometry()->momentum().pseudoRapidity()) > mEtaCut ) {
	cout << "StRichSpectraMaker::checkTrack() --> ";
	cout << "mEtaCut ("
	     << track->geometry()->momentum().pseudoRapidity() << ")" << endl;
	status = false;
    }

    if (track->geometry()->momentum().perp() < mPtCut) {
  	cout << "StRichSpectraMaker::checkTrack() --> ";
	cout << "mPtCut ("
	     << track->geometry()->momentum().perp() << ")" << endl;
	status = false;
    }
    
    return status;
}

bool StRichSpectraMaker::checkMomentumThreshold(StTrack* track) const {

    if (track->geometry()->momentum().mag() > mMomentumThreshold) {
	return true;
    }

    return false;

}

bool StRichSpectraMaker::checkMomentumLimit(StTrack* track) const {

    if (track->geometry()->momentum().mag() < mMomentumLimit) {
	return true;
    }

    return false;

}

bool StRichSpectraMaker::checkMomentumWindow(StTrack* track) const {

    if ( this->checkMomentumThreshold(track) &&
	 this->checkMomentumLimit(track) ) {
	return true;
    }

    return false;

}

float StRichSpectraMaker::expectedNumberOfPhotons(float p, int pid) const
{
    float index2 = mIndex*mIndex;

    float mass;

    switch(pid) {
    case -211:
	mass = mPion->mass();
	break;

    case -321:
	mass = mKaon->mass();
	break;

    case -2212:
	mass = mProton->mass();
	break;

    default:
	cout << "StRichSpectraMaker::expectedNumberOfPhotons()\n";
	cout << "\tWARNING\n";
	cout << "\tBad Pid number (" << pid << ") " << endl;
	return -999;
    }

    float beta2 = p*p/(p*p + mass*mass);

    float fraction = (beta2*index2-1)/(beta2*(index2-1));
    return fraction;
}

void StRichSpectraMaker::qualityAssessment() {

    //vertex
    cout << "StRichSpectraMaker::qualityAssessment()" << endl;
    PR(mVertexPos);
    PR(mNumberOfPrimaries);
    
    
    StPtrVecTrack richTracks = mEvent->richCollection()->getTracks();
    PR(richTracks.size());

    for(size_t ii=0; ii<richTracks.size(); ii++) {
	cout << " ptr: " << richTracks[ii] << endl;
	cout << " p:   " << richTracks[ii]->geometry()->momentum().mag() << endl;
	
	const StPtrVecTrackPidTraits&
	    thePidTraits = richTracks[ii]->pidTraits(kRichId);

	PR(thePidTraits.size());

 	for(size_t jj=0; jj<thePidTraits.size(); jj++) {
	    // loop over traits
	    StRichPidTraits* theRichPidTraits =
		dynamic_cast<StRichPidTraits*>(thePidTraits[jj]);

	    if(!theRichPidTraits) {
		cout << "Bad pid traits" << endl;
		continue;
	    }
 	    PR(theRichPidTraits[jj].productionVersion());

 	    if(!theRichPidTraits[jj].associatedMip()) {
 		cout << "\tNo Associated MIP\n";
 		cout << "\tNo MIP Residual" << endl;
 	    }
 	    else {
 		PR(theRichPidTraits[jj].associatedMip()->local());
 		PR(theRichPidTraits[jj].mipResidual());
 		PR(theRichPidTraits[jj].refitResidual());
 	    }
 	    PR(theRichPidTraits[jj].signedDca2d());
 	    PR(theRichPidTraits[jj].signedDca3d());

	    cout << " *** Try get the pids" << endl;
 	    const StSPtrVecRichPid& theRichPids =
 		theRichPidTraits[jj].getAllPids();

	    PR(theRichPids.size());
 	    for(size_t kk=0; kk<theRichPids.size(); kk++) {
 		cout << "kk= " << kk << " ";
 		PR(theRichPids[kk]->getParticleNumber());
		PR(theRichPids[kk]->getMipResidual());
		
		const StSPtrVecRichPhotonInfo& photonInfo =
		    theRichPids[kk]->getPhotonInfo();
		PR(photonInfo.size());

		const StPtrVecRichHit& hits =
		    theRichPids[kk]->getAssociatedRichHits();
		PR(hits.size());
		
 	    }
	    
 	} // jj --> traits
	

	
    }
    
    //loop over these tracks
    cout << "StRichSpectraMaker::qualityAssessment()\n";
    cout << "================== END ==================" << endl;
}

void StRichSpectraMaker::printCutParameters(ostream& os) const
{
    os << "==============================================" << endl;
    os << "StRichSpectraMaker::printCutParameters()" << endl;
    os << "----------------------------------------------" << endl;
    os << "Event Level:" << endl;
    os << "\tVertexWindow =  " << (mVertexWindow/centimeter)  << " cm"    << endl;
    os << "\nTrack Level:" << endl;
    os << "\tPtCut =         " << (mPtCut/GeV)                << " GeV/c" << endl;
    os << "\tEtaCut =        " << mEtaCut                                 << endl;
    os << "\tLastHitCut =    " << (mLastHitCut/centimeter)    << " cm"    << endl;
    os << "\tDcaCut =        " << (mDcaCut/centimeter)        << " cm"    << endl;
    os << "\tFitPointsCut =  " << mFitPointsCut                           << endl;
    os << "\tPathCut =       " << (mPathCut/centimeter)       << " cm"    << endl;
    os << "\tPadPlaneCut =   " << (mPadPlaneCut/centimeter)   << " cm"    << endl;
    os << "\tRadiatorCut =   " << (mRadiatorCut/centimeter)   << " cm"    << endl;
    os << "\tLower Mom =     " << (mMomentumThreshold/GeV)    << " GeV/c" << endl;
    os << "\tUpper Mom =     " << (mMomentumLimit/GeV)    << " GeV/c" << endl;
    os << "----------------------------------------------" << endl;
    os << "----------------------------------------------" << endl;


}

////////////////////////
////////////////////////
// Drawing routines
//
void StRichSpectraMaker::drawRichPixels(StRichCollection* collection) const
{
#ifdef RICH_WITH_PAD_MONITOR
    const StSPtrVecRichPixel&  pixels = collection->getRichPixels();
    if(!pixels.size()) {
	cout << "StRichSpectraMaker::drawRichPixels()";
	cout << "\tNo Pixels in the Collection\n";
	cout << "\tReturning" << endl;
	return;
    }

    for(size_t ii=0; ii<pixels.size(); ii++) {
	mPadMonitor->drawPad(StRichSinglePixel(pixels[ii]->pad(),
					       pixels[ii]->row(),
					       pixels[ii]->adc()));
    }
    mPadMonitor->update();
    
#endif
}

void StRichSpectraMaker::drawRichHits(StRichCollection* collection) const
{
#ifdef RICH_WITH_PAD_MONITOR
    const StSPtrVecRichHit&  hits = collection->getRichHits();
    if(!hits.size()) {
	cout << "StRichSpectraMaker::drawRichHits()";
	cout << "\tNo Hits in the Collection\n";
	cout << "\tReturning" << endl;
	return;
    }

    for(size_t ii=0; ii<hits.size(); ii++) {
	//mPadMonitor->drawHit(hits[ii]);
	mPadMonitor->drawMarker(hits[ii]->local(),5);
    }
    mPadMonitor->update();
    

#endif
}

void StRichSpectraMaker::drawTracks() const
{
#ifdef RICH_WITH_PAD_MONITOR

    mPadMonitor->update();
#endif
}
