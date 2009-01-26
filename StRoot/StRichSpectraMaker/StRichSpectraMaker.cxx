/***************************************************************************
 *
 * $Id: StRichSpectraMaker.cxx,v 1.22 2009/01/26 14:53:17 fisyak Exp $
 *
 * Author:  bl
 ***************************************************************************
 *
 * Description: RICH offline software:
 *              StRichSpectraMaker.cxx - ROOT/STAR Maker for offline chain.
 *              Uses the information in the pidtraits to produce a spectrum
 *
 * --> Feb 1, 2002 This version of the spectraMaker uses the outer
 *                 helix for extrapolation.  No correction for the
 *                 residual is coded (must be deduced)
 ***************************************************************************
 *
 * $Log: StRichSpectraMaker.cxx,v $
 * Revision 1.22  2009/01/26 14:53:17  fisyak
 * Clean up %n => \n
 *
 * Revision 1.21  2007/04/28 17:56:51  perev
 * Redundant StChain.h removed
 *
 * Revision 1.20  2007/04/27 13:11:48  hippolyt
 * Star logger recommendations
 *
 * Revision 1.19  2004/01/14 22:50:43  fisyak
 * Add include of <algorithm>
 *
 * Revision 1.18  2003/09/02 17:58:55  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.17  2003/04/30 20:38:16  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.16  2002/05/23 02:00:52  dunlop
 * Put mVertexWindow, etc. back to what was used for SL02[cde], so
 * we don't have a mishmash of versions in the MuDst's.
 *
 * Revision 1.15  2002/05/21 22:52:56  lasiuk
 * attempt 2
 *
 * Revision 1.13  2002/02/22 18:20:31  dunlop
 * Actually, rationalize return values
 *
 * Revision 1.12  2002/02/22 18:18:13  dunlop
 * condom to allow for 0 photons passing mean calculation without NAN.
 *
 * Revision 1.11  2002/02/22 14:30:51  dunlop
 * Fixed bug in filling of StEvent StRichSpectra.
 * Loosened cuts to match StRichPIDMaker.
 *
 * Revision 1.10  2002/02/19 04:26:50  lasiuk
 * addition of filling StEvent for inclusion in chain
 *
 * Revision 1.9  2002/02/12 15:31:35  lasiuk
 * changes to remove formatting of tuple structures
 *
 * Revision 1.8  2002/02/01 17:45:56  lasiuk
 * Mods for gcc(7.2)
 * outer helix usage
 * histo mods
 *
 * Revision 1.6  2001/12/19 20:18:38  lasiuk
 * Changeover in algorithm of isolating the Cherenkov angle
 *
 * Revision 1.5  2001/11/21 20:36:07  lasiuk
 * azimuth angle calculation, trace and retracing algorithms, rotation
 * matrices, clean up intersection calculation.  Addition of quick
 * rings for graphics, change definition of ntuples, and
 * removal of old PID method
 *
 * Revision 1.4  2001/08/22 19:33:35  lasiuk
 * remove trace of StPairD, and move some include files that
 * should ease parsing of CINT
 *
 * Revision 1.3  2001/08/21 17:58:34  lasiuk
 * for 2000 analysis
 *
 * Revision 1.2  2001/02/25 22:11:46  lasiuk
 * quality assessment
 *
 * Revision 1.1  2000/12/12 21:35:08  lasiuk
 * Initial Revision
 *
 **************************************************************************/
#include "StRichDisplayActivate.h"
#include "StRichSpectraMaker.h"

//#define WITH_GEANT_INFO 1
//#define P00hm 0

#include <Stiostream.h>
#include "Stiostream.h"
#include <assert.h>
#include <float.h>
#include <vector>
#include <algorithm>

#include "St_DataSetIter.h"

#include "StGlobals.hh"
#include "StThreeVector.hh"
#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
using std::vector;
using std::unique;
#endif

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
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichCoordinates.h"

// Internal Rch
#include "StRchMaker/StRichSimpleHit.h"
#include "StRichPIDMaker/StRichTrack.h"

// SpectraMaker
#include "StRichRayTracer.h"
#include "StRichCerenkovHistogram.h"

// g2t tables
// $STAR/pams/sim/idl
#include "tables/St_g2t_tpc_hit_Table.h"
#include "tables/St_g2t_rch_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "StRichGeantCalculator.h"

ClassImp(StRichSpectraMaker) // macro
//-----------------------------------------------------------------

void dump(g2t_rch_hit_st *rch_hit, g2t_track_st *track)
{
    cout << "StRichSpectraMaker::dump()" << endl;
    cout << " de=        " << rch_hit->de << endl;
    cout << " id=        " << rch_hit->id << endl;
    cout << " volume_id= " << rch_hit->volume_id << endl;
    StThreeVectorD p(rch_hit->p[0], rch_hit->p[1], rch_hit->p[2]); 
    cout << " p=         " << p << endl;
    cout << " track_p=   " << rch_hit->track_p << endl;
    cout << "   track[" << (rch_hit->track_p) << "].ge_pid: " << track[(rch_hit->track_p)].ge_pid << endl;
    cout << "   track[" << (rch_hit->track_p) << "].next_parent_p: " << track[(rch_hit->track_p)].next_parent_p << endl;

    cout << "   track[" << (track[(rch_hit->track_p)].next_parent_p) << "].ge_pid: "
	 <<     track[(track[rch_hit->track_p].next_parent_p)].ge_pid << endl;

    cout << "   track[" << (rch_hit->track_p) << "].is_showershower?" << track[rch_hit->track_p].is_shower << endl;
    StThreeVectorD parentp(track[(rch_hit->track_p)].p[0],
			   track[(rch_hit->track_p)].p[1],
			   track[(rch_hit->track_p)].p[2]);
    cout << "   track[" << (rch_hit->track_p) << "].p: " << parentp << endl;
    StThreeVectorD nextParentp(track[(track[(rch_hit->track_p)].next_parent_p)].p[0],
			       track[(track[(rch_hit->track_p)].next_parent_p)].p[1],
			       track[(track[(rch_hit->track_p)].next_parent_p)].p[2]);
    cout << "   track[" << (track[(rch_hit->track_p)].next_parent_p) << "].p: " << nextParentp << endl;
    cout << "   track[" << (rch_hit->track_p) << "].ptot: " << track[(rch_hit->track_p)].ptot << endl;
 }

void dumpMinus1(g2t_rch_hit_st *rch_hit, g2t_track_st *track)
{
    cout << "StRichSpectraMaker::dumpMinus1()" << endl;
    cout << " de=        " << rch_hit->de << endl;
    cout << " id=        " << rch_hit->id << endl;
    cout << " volume_id= " << rch_hit->volume_id << endl;
    StThreeVectorD p(rch_hit->p[0], rch_hit->p[1], rch_hit->p[2]); 
    cout << " p=         " << p << endl;
    cout << " track_p=   " << rch_hit->track_p << endl;
    int index = (rch_hit->track_p-1);
    cout << "   track[" << index << "].ge_pid: " << track[index].ge_pid << endl;
    cout << "   track[" << index << "].is_shower: " << track[index].is_shower << endl;
    cout << "   track[" << index << "].next_parent_p: " << track[index].next_parent_p << endl;

    int index2 = track[index].next_parent_p-1;
    cout << "   track[" << index2 << "].ge_pid: "          << track[index2].ge_pid << endl;
    cout << "   track[" << index2 << "].is_showershower? " << track[index2].is_shower << endl;
    StThreeVectorD pp(track[index].p[0],
		      track[index].p[1],
		      track[index].p[2]);
    cout << "   track[" << index << "].p: " << pp << endl;
    StThreeVectorD nextParentp(track[index2].p[0],
			       track[index2].p[1],
			       track[index2].p[2]);
    cout << "   track[" << index2 << "].p: " << nextParentp << endl;
    cout << "   track[" << index2 << "].ptot: " << track[index2].ptot << endl;
 }

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
    mFile = new TFile("/star/rcf/scratch/lasiuk/theta/S02_b0_bFull_129039outer_tc_down2mm.root","RECREATE","Pid Ntuples");

    
    //
    // this is at the event level
    //
    mEvt = new TNtuple("evt","Event Characteristics","vx:vy:vz:nPrim:nRich:ctb:zdc");
    
    mTrack = new TNtuple("track","Identified Tracks","vz:p:px:py:pz:pt:eta:sdca2:sdca3:x:y:dx:dy:cdx:cdy:theta:sig:np:peak:peaknp:totnp:mass2:q:flag:lint:lintr:alpha:lpx:lpy:lpz:nFit:dedx:fpx:fpy:fpz:chi:dpi:ndpi:dk:ndk:dp:ndp");
    //
    // this is at the Cerenkov photon level
    //
    mCerenkov = new TNtuple("cer","angle","p:px:py:pz:alpha:phx:phy:l3:phi:ce:qce:dx:dy:id:q:vz");

    //
    // geant
    // n-numberofhits, np-numberofPhotons
    mSimEvent = new TNtuple("sevt","evtii","n:np");
    // e-energy, l-lambda
    mSim = new TNtuple("sim","photon","e:l");
#endif

    this->initCutParameters();

    mGeometryDb = StRichGeometryDb::getDb();
    PR(mGeometryDb->localOriginR());
    PR(mGeometryDb->localOriginAngle());
    PR(mGeometryDb->localOriginZ());
    mAverageRadiationPlanePoint =
	StThreeVectorF(0.,
		       0.,
		       (mGeometryDb->proximityGap() +
			mGeometryDb->quartzDimension().z() +
			mGeometryDb->radiatorDimension().z()/2.));

    mAverageQuartzRadiationPlanePoint =
	StThreeVectorF(0.,
		       0.,
		       (mGeometryDb->proximityGap() +
			mGeometryDb->quartzDimension().z()/2.));

    mGlobalRichNormal =
	StThreeVectorF(mGeometryDb->normalVectorToPadPlane().x(),
		       mGeometryDb->normalVectorToPadPlane().y(),
		       mGeometryDb->normalVectorToPadPlane().z());

    PR(mAverageRadiationPlanePoint);
    PR(mAverageQuartzRadiationPlanePoint);
    PR(mGlobalRichNormal);
    
    mTopRadiator = StThreeVectorF(0.,
				  0.,
				  (mGeometryDb->proximityGap() +
				   mGeometryDb->quartzDimension().z() +
				   mGeometryDb->radiatorDimension().z()));
    mBottomRadiator = StThreeVectorF(0.,
				     0.,
				     (mGeometryDb->proximityGap() +
				      mGeometryDb->quartzDimension().z()));

    cout << "*** Calculated Radiation Point: \nProximity: " <<
	(mGeometryDb->proximityGap()) << "\nQuartz: " <<
	(mGeometryDb->quartzDimension().z()) << "\nRadiator/2: " <<
	(mGeometryDb->radiatorDimension().z()/2.) << "\nTotal: " <<
	(mGeometryDb->proximityGap() +
	 mGeometryDb->quartzDimension().z() +
	 mGeometryDb->radiatorDimension().z()/2.) << endl;

    mYExtreme = mGeometryDb->radiatorDimension().y();
    
    //
    // Coordinate and Momentum Transformation Routines
    //
    mTransform = StRichCoordinateTransform::getTransform(mGeometryDb);
    mMomTform  = StRichMomentumTransform::getTransform(mGeometryDb);  

    //
    // make the particles
    //
    mPion = StPionMinus::instance();
    mKaon = StKaonMinus::instance();
    mProton = StAntiProton::instance();

    mMeanWavelength = 176.*nanometer;
    PR(mMeanWavelength/nanometer);
    
    StRichMaterialsDb* materialsDb = StRichMaterialsDb::getDb();
    mIndex = materialsDb->indexOfRefractionOfC6F14At(mMeanWavelength);

    mIndex = 1.29039;

    mAlpha = 5.e-4;
    mTemperatureGradient = 7./mYExtreme;
    PR(mTemperatureGradient);
    
    { LOG_ERROR << "Freon Index in maker " << mIndex << endm; }
    
    ////////////////////////////////////////////////////////////////////////
    double ii;
    for(ii=160.*nanometer; ii<220.*nanometer; ii+=1.*nanometer)
 	cout << (ii/nanometer) << " ";
    cout << "\n nc6f14" << endl;
    for(ii=160.*nanometer; ii<220.*nanometer; ii+=1.*nanometer)
 	cout << (materialsDb->indexOfRefractionOfC6F14At(ii)) << " ";
    cout << "\n lc6f14" << endl;
    for(ii=160.*nanometer; ii<220.*nanometer; ii+=1.*nanometer)
 	cout << (materialsDb->absorptionCoefficientOfC6F14At(ii)) << " ";
    cout << "\n nquartz" << endl;
    for(ii=160.*nanometer; ii<220.*nanometer; ii+=1.*nanometer)
 	cout << (materialsDb->indexOfRefractionOfQuartzAt(ii)) << " ";
    cout << "\n lquartz" << endl;
    for(ii=160.*nanometer; ii<220.*nanometer; ii+=1.*nanometer)
 	cout << (materialsDb->absorptionCoefficientOfQuartzAt(ii)) << " ";
    cout << "\n csiqe" << endl;
    for(ii=160.*nanometer; ii<220.*nanometer; ii+=1.*nanometer)
 	cout << (materialsDb->quantumEfficiencyOfCsIAt(ii)) << " ";
    cout << endl;
    
    this->printCutParameters();

    mTracer = new StRichRayTracer(mMeanWavelength,mIndex);
    mTracer->setFreonRadiationPlane(mAverageRadiationPlanePoint);
    mTracer->setQuartzRadiationPlane(mAverageQuartzRadiationPlanePoint);

    mHistogram = new StRichCerenkovHistogram();
    mHistogram->doPhiCut(mDoPhiCut);
    
    return StMaker::Init();
}

////////////////////////////////////////////////////////////////////////////////////////
void StRichSpectraMaker::initCutParameters() {
    //
    // Event Level
    //
    mVertexWindow = 200.*centimeter;
    
    //
    // Track Level
    //
    mPtCut = 0.*GeV; // GeV/c
    mEtaCut = 0.5; 
    mLastHitCut = 100.0*centimeter;
    mDcaCut = 3.0*centimeter;
    mFitPointsCut = 20;
    mPathCut = 500*centimeter;
    mPadPlaneCut = 1.0*centimeter;
    mRadiatorCut = 1.0*centimeter;

    mMomentumThreshold = .5*GeV;
    mMomentumLimit = 7.*GeV;

    mDoPhiCut = false;
}

//-----------------------------------------------------------------

Int_t StRichSpectraMaker::Make() {
    cout << "StRichSpectraMaker::Make()" << endl;
    mNumberOfEvents++;
    //
    // ptr initialization for StEvent
    //
    mTheRichCollection = 0;

#ifdef WITH_GEANT_INFO
    StRichGeantCalculator calculator;
    ////////////////////////////////// <-------------------------
    if (!m_DataSet->GetList())  {
	St_DataSetIter geant(GetDataSet("geant"));
	St_g2t_rch_hit *g2t_rch_hit =
	    static_cast<St_g2t_rch_hit *>(geant("g2t_rch_hit"));

	if (!g2t_rch_hit) {
	    // For backwards compatibility look in dst branch
	    cout << "look in dst" << endl;
	    St_DataSetIter dstDstI(GetDataSet("dst"));
	    g2t_rch_hit = static_cast<St_g2t_rch_hit*>(dstDstI("g2t_rch_hit"));
	}
	if(!g2t_rch_hit){
	    cout << "StRichSpectraMaker::Make()";
	    cout << "\tNo g2t_rch_hit pointer";
	    cout << "\treturn from StRichSpectraMaker::Make()" << endl;
	    return kStWarn;
	}

	
	g2t_rch_hit_st *rch_hit     =  g2t_rch_hit->GetTable();
	int numberOfRichHits = g2t_rch_hit->GetNRows();
	PR(numberOfRichHits);

	St_g2t_track *g2t_track =
	    static_cast<St_g2t_track *>(geant("g2t_track"));

	
	if(!g2t_track){
	    cout << "StRichSpectraMaker::Make()\n";
	    cout << "\tNo g2t_track pointer\n";
	    cout << "\treturn from StRrsMaker::Make()" << endl;
	    return kStWarn;
	}
	
	int numberOfTracks          =  g2t_track->GetNRows();
	PR(numberOfTracks);

	g2t_track_st *track =  g2t_track->GetTable();

	 
	float simEventTuple[2];
	simEventTuple[0] = numberOfRichHits;

	
	float simTuple[2];
	int goodPhotons = 0;
 	vector<int> ptrackId;
 	vector<int> ptrackPtr;

	vector<int> ctrackId;
	vector<int> ctrackPtr;
	for(int ii=0; ii<numberOfRichHits; ii++) {

//  	    //dump(rch_hit, track);
//  	    dumpMinus1(rch_hit, track);

	    calculator.process(rch_hit, track);

	    if(rch_hit->de<0) {
// 		calculator.process(rch_hit, track);
		goodPhotons++;
		simTuple[0] = fabs(rch_hit->de);
		simTuple[1] = fabs(1240./rch_hit->de)/1.e9;
#ifdef RICH_SPECTRA_HISTOGRAM
		mSim->Fill(simTuple);
#endif
 		ptrackId.push_back(rch_hit->id);
 		ptrackPtr.push_back(rch_hit->track_p);
	    }
	    else {
		ctrackId.push_back(rch_hit->id);
		ctrackPtr.push_back(rch_hit->track_p);
	    }
	    rch_hit++;

	}
	PR(goodPhotons);
	simEventTuple[1] = goodPhotons;
#ifdef RICH_SPECTRA_HISTOGRAM
	mSimEvent->Fill(simEventTuple);
#endif
	calculator.status();
    }
#endif // WITH_GEANT_INFO
    ////////////////////////////////// <-------------------------
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
    
    
    //mMagField    = .249117*tesla;    
    if (mEvent->summary()) {
	mMagField  = mEvent->summary()->magneticField()*kilogauss;
	PR(mMagField);
	cout << "  B field = " << (mMagField/tesla) << " T" << endl;
    } 
    else {
	cout << "StRichSpectraMaker::Make().\n";
	cout << "\tWARNING!\n";
	cout << "\tCannot get B field from mEvent->summary().\n";
	cout << "\tUse B= " << (mMagField/tesla) << " T" << endl;
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
    if(fabs(mVertexPos.z())>mVertexWindow) {
	cout << "Vertex out of range...(" << mVertexPos.z() << ")" << endl;
	return kStWarn;
    }
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
	cout << "\tTpcHit collection exists" << endl;
    }
    else {
	cout << "StRichSpectraMaker::Make()\n";
	cout << "\tWARNING\n";
	cout << "\tTpcHit collection DOES NOT exist!!!!" << endl;
	cout << "\tContinuing..." << endl;
    }

#ifdef RICH_WITH_PAD_MONITOR
    cout << "StRichSpectraMaker::Next Event? <ret>: " << endl;
    char* dir = ".";
    char* name = "event";
    if(mPadMonitor)
	mPadMonitor->printCanvas("dir","name",mNumberOfEvents);
    do {
	if(getchar()) break;
    } while (true);
    
    mPadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
    mPadMonitor->clearAll();
    
    this->drawRichPixels(mEvent->richCollection());
    this->drawRichHits(mEvent->richCollection());
#endif

    this->evaluateEvent();
//     this->qualityAssessment();
     
    //
    //
    // The track loop
    //
    //
    float trackTuple[42] = {-999.};
    trackTuple[0] = mVertexPos.z();

//     cout << "Looping over " << mNumberOfPrimaries << " primary Tracks" << endl;
    for(size_t ii=0; ii<mNumberOfPrimaries; ii++) { // primaries

//  	cout << "==> Track " << ii << "/" << (mNumberOfPrimaries-1);
	StTrack* track = mEvent->primaryVertex()->daughter(ii);
  	//cout << "*****Track " << ii << "\tp= " << track->geometry()->momentum().mag() << endl;

	//if( !this->checkMomentumWindow(track) ) continue;
	//if (!this->checkTrack(track)) continue;

	float dEdx = 0;
	const StPtrVecTrackPidTraits&
	    theTpcPidTraits = track->pidTraits(kTpcId);
	for(size_t iTpc=0; iTpc<theTpcPidTraits.size(); iTpc++) {

	    StDedxPidTraits* pid = dynamic_cast<StDedxPidTraits*>(theTpcPidTraits[iTpc]);
	    if(pid && pid->method() == kTruncatedMeanId) {
		dEdx = pid->mean();
	    }
	}
	

	//
	// Get the PID traits, if there is an StrichPIDTrait:
	//
	const StPtrVecTrackPidTraits&
	    theRichPidTraits = track->pidTraits(kRichId);

	if(!theRichPidTraits.size()) continue;
//   	cout << " (" << theRichPidTraits.size() << ") Pid Traits.   p= ";

	//
	// For globals
	//
// 	StTrack* trackCutCriteria = track->node()->track(global);
	StThreeVectorF trackMomentum = track->geometry()->momentum();

	trackTuple[1] = abs(trackMomentum);
	trackTuple[2] = trackMomentum.x();
	trackTuple[3] = trackMomentum.y();
	trackTuple[4] = trackMomentum.z();
	trackTuple[5] = trackMomentum.perp();
	trackTuple[6] = trackMomentum.pseudoRapidity();

	//
	// info from the traits
	//
	
	StTrackPidTraits* theSelectedTrait =
	    theRichPidTraits[theRichPidTraits.size()-1];

	if(!theSelectedTrait) {
	    cout << "Error in the Selected Trait\nContinuing..." << endl;
	    continue;
	}
	
	StRichPidTraits *richPidTrait =
	    dynamic_cast<StRichPidTraits*>(theSelectedTrait);

	//
	// this should not be necessary for the
	// next round of production
	//

#ifdef P00hm
	trackTuple[7] = 0.;
	trackTuple[8] = 0.;

	//
	// set value for: "mMipResidual"
	//                "mAssociatedMip"
	//

	if(!this->assignMipResidual(track)) continue;
	
#else
	trackTuple[7] = richPidTrait->signedDca2d();
	trackTuple[8] = richPidTrait->signedDca3d();

	if(!richPidTrait->associatedMip()) continue;
// 	PR(richPidTrait->associatedMip()->local());
//  	PR(richPidTrait->mipResidual());
	
	mAssociatedMip = richPidTrait->associatedMip()->local();
	mMipResidual   = richPidTrait->mipResidual();
#endif

#ifdef RICH_WITH_PAD_MONITOR
	//
	// Put a box around the mip and
	// write its momentum and incidnet angle
	//
	
	// 25 = open box
	mPadMonitor->drawMarker(mAssociatedMip, 25, 3);
	mPadMonitor->update();
#endif

	// 	PR(residual);

	//
	//
	//
	trackTuple[9]  = mAssociatedMip.x();
	trackTuple[10] = mAssociatedMip.y();
	trackTuple[11] = mMipResidual.x();
	trackTuple[12] = mMipResidual.y();

	this->doIdentification(track);

	trackTuple[13] = mCalculatedResidual.x();
	trackTuple[14] = mCalculatedResidual.y();

	unsigned short iflag;
	double cerenkovAngle = mHistogram->cerenkovAngle(&iflag);
	trackTuple[15] = (cerenkovAngle/degree);
	trackTuple[16] = (mHistogram->cerenkovSigma()/degree);
	trackTuple[17] = (mHistogram->numberOfPhotons());

	short numberOfPhotons = 0;
	double peakAngle = mHistogram->peakAngle(&numberOfPhotons);
	trackTuple[18] = (peakAngle/degree);
	trackTuple[19] = numberOfPhotons;

	trackTuple[20] = mUniqueRingHits;
	

	double mass2 = sqr(abs(trackMomentum))*(sqr(mTheIndex*cos(cerenkovAngle))-1);
	PR(mass2);
	trackTuple[21] =  mass2;
	
	trackTuple[22] = track->geometry()->charge();
	trackTuple[23] = iflag;

	double totalPath=0;
	double lineIntegralRatio = mTracer->lineIntegral(&totalPath);
	trackTuple[24] = lineIntegralRatio;
	trackTuple[25] = totalPath;
	trackTuple[26] = mTracer->trackAngle()/degree;
	//
	// try calculate the ring length path integral
	//
	vector<StThreeVectorF> garbage =
	    mTracer->calculatePoints(mRadPoint,mass2);
	
	trackTuple[27] = track->detectorInfo()->lastPoint().x();
	trackTuple[28] = track->detectorInfo()->lastPoint().y();
	trackTuple[29] = track->detectorInfo()->lastPoint().z();
	trackTuple[30] = track->fitTraits().numberOfFitPoints();
	trackTuple[31] = dEdx;
	trackTuple[32] = track->detectorInfo()->firstPoint().x();
	trackTuple[33] = track->detectorInfo()->firstPoint().y();
	trackTuple[34] = track->detectorInfo()->firstPoint().z();
// 	PR(track->fitTraits().chi2());
	trackTuple[35] = track->fitTraits().chi2();

	trackTuple[36] = mD[0];
	trackTuple[37] = mNpd[0];
	trackTuple[38] = mD[1];
	trackTuple[39] = mNpd[1];
	trackTuple[40] = mD[2];
	trackTuple[41] = mNpd[2];
#ifdef RICH_SPECTRA_HISTOGRAM
	mTrack->Fill(trackTuple);
#endif

#ifdef FOR_RECONSTRUCTION
	////////////////////StEvent//////////////////////
	//
	// Add in the StEvent Value
	// for production.  The integer could
	// be used as a coded number
	// dynamic_cast<StRichPidTraits*>(theSelectedTrait)->setProbability(mass2);
	dynamic_cast<StRichPidTraits*>(theSelectedTrait)->setId((int)mHistogram->numberOfPhotons());

	StRichSpectra *oldSpectra =
	    dynamic_cast<StRichPidTraits*>(theSelectedTrait)->getRichSpectra();
	if(oldSpectra) oldSpectra->makeZombie();
	
	StRichSpectra *spectraInfo =
	    new StRichSpectra(mAssociatedMip.x(), mAssociatedMip.y(),
			      mMipResidual.x(), mMipResidual.y(),
			      mCalculatedResidual.x(), mCalculatedResidual.y(), 
			      cerenkovAngle,
			      mHistogram->cerenkovSigma()/degree,
			 (int)mHistogram->numberOfPhotons(),
			      peakAngle/degree, numberOfPhotons, mUniqueRingHits,
			      mass2, lineIntegralRatio, totalPath,
			      mTracer->trackAngle()/degree, iflag, dEdx,
			      mD[0], mD[1], mD[2],
			      int(mNpd[0]), int(mNpd[1]), int(mNpd[2]));
	//
	// default Production
	//
	spectraInfo->setVersion(20011);  
	dynamic_cast<StRichPidTraits*>(theSelectedTrait)->setRichSpectra(spectraInfo);
	//
	//////////////////////////////////////////////////
#endif	
    } // loop over the tracks

      cout << "try clear data from histogram" << endl;
      mHistogram->clearData();
    
    return kStOk;
}


//-----------------------------------------------------------------
void StRichSpectraMaker::PrintInfo() 
{
  { LOG_INFO << "************************************************************** \n * $Id: StRichSpectraMaker.cxx,v 1.22 2009/01/26 14:53:17 fisyak Exp $ \n **************************************************************" << endm; }
    if (Debug()) StMaker::PrintInfo();
}

//-----------------------------------------------------------------


Int_t StRichSpectraMaker::Finish() {

  { LOG_DEBUG << "StRichSpectraMaker::Finish() : " << mNumberOfGood2GevTracks << " Good 2.0-2.5 GeV tracks in " << mNumberOfEvents << " Events." << endm; }
	
    this->printCutParameters();

	
#ifdef RICH_SPECTRA_HISTOGRAM
    if(mFile) {
      { LOG_DEBUG << "StRichSpectraMaker::Finish() %n Close the Histogram files!!!!!!" << endm; }
      mFile->Write();
      mFile->Close();
      delete mFile;
      mFile = 0;
    }
#endif

    return StMaker::Finish();
}

// ----------------------------------------------------
bool StRichSpectraMaker::checkTrack(StTrack* track) const {

    //
    // track -- quality cuts
    //       -- momentum and parameter
    //
    bool status = true;
    if (!track) {
      { LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> !track" << endm; }
      status = false;
    }
    if (track->flag()<0) {
      { LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> track->flag()<0 (" << track->flag() << ")" << endm; }
      status = false;
    }

    if (!track->geometry()) {
      {	LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> !track->geometry()" << endm; }
      status = false;
    }

    if(track->geometry()->helix().distance(mVertexPos)>mDcaCut) {
      { LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> mDcaCut (" << track->geometry()->helix().distance(mVertexPos) << ")" << endm; }
      status = false;
    }
	
    if(track->fitTraits().numberOfFitPoints(kTpcId) < mFitPointsCut) {
      { LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> mFitPointsCut (" << track->fitTraits().numberOfFitPoints(kTpcId) << endm; }
      status = false;
    }

    if( fabs(track->geometry()->momentum().pseudoRapidity()) > mEtaCut ) {
      { LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> mEtaCut (" << track->geometry()->momentum().pseudoRapidity() << ")" << endm; }
      status = false;
    }

    if (track->geometry()->momentum().perp() < mPtCut) {
      { LOG_DEBUG << "StRichSpectraMaker::checkTrack() --> mPtCut (" << track->geometry()->momentum().perp() << ")" << endm; }
      status = false;
    }
    
    return status;
}

// ----------------------------------------------------
bool StRichSpectraMaker::checkMomentumThreshold(StTrack* track) const {

    if (track->geometry()->momentum().mag() > mMomentumThreshold) {
	return true;
    }

    return false;

}

// ----------------------------------------------------
bool StRichSpectraMaker::checkMomentumLimit(StTrack* track) const {

    if (track->geometry()->momentum().mag() < mMomentumLimit) {
	return true;
    }

    return false;

}

// ----------------------------------------------------
bool StRichSpectraMaker::checkMomentumWindow(StTrack* track) const {

    if ( this->checkMomentumThreshold(track) &&
	 this->checkMomentumLimit(track) ) {
	return true;
    }

    return false;

}

// ----------------------------------------------------
pair<double,double>
StRichSpectraMaker::expectedCerenkov(float p, int pid) const
{
    pair<double,double> values;
    values.first=-999.;
    values.second=-999.;
    
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
      { LOG_WARN << "StRichSpectraMaker::expectedNumberOfPhotons() %n WARNING %n Bad Pid number (" << pid << ") " << endm; }
      return values;
    }

    float beta2 = p*p/(p*p + mass*mass);

    double cosine = ::sqrt(p*p+mass*mass)/(p*mIndex);
    if(fabs(cosine)<=1)
	values.first = acos(cosine);
    else
	values.first = FLT_MAX;
    
    values.second = (beta2*index2-1)/(beta2*(index2-1));
    return values;
}

// ----------------------------------------------------
bool StRichSpectraMaker::evaluateEvent() {

#ifdef RICH_SPECTRA_HISTOGRAM
    float tuple[7];

    tuple[0] = mVertexPos.x();
    tuple[1] = mVertexPos.y();
    tuple[2] = mVertexPos.z();

    // primaries
    tuple[3] = mEvent->primaryVertex()->numberOfDaughters();
#ifdef P00hm
    tuple[4] = -999;
#else
    tuple[4] = mEvent->richCollection()->getTracks().size();
#endif

    StTriggerDetectorCollection* trig = mEvent->triggerDetectorCollection();
    double ctbAdc = 0;
    double zdcAdc = 0;
    if(trig) {

	for(unsigned int itray=0; itray<trig->ctb().numberOfTrays(); itray++) {
	    for(unsigned int islat=0; islat<trig->ctb().numberOfSlats(); islat++) {
		ctbAdc += trig->ctb().mips(itray,islat);
	    }
	}
	zdcAdc = trig->zdc().adcSum();

    }
//     PR(ctbAdc);
//     PR(zdcAdc);
    tuple[5] = ctbAdc; //ctb
    tuple[6] = zdcAdc; //zdc ;

    mEvt->Fill(tuple);
#endif
    
    return true;
}

// ----------------------------------------------------
void StRichSpectraMaker::qualityAssessment() {

  { LOG_DEBUG << "StRichSpectraMaker::qualityAssessment()" << endm; }

#ifndef P00hm 
    //vertex
    PR(mVertexPos);
    PR(mNumberOfPrimaries);
    
    StPtrVecTrack richTracks = mEvent->richCollection()->getTracks();
    PR(richTracks.size());

    for(size_t ii=0; ii<richTracks.size(); ii++) {
      { LOG_DEBUG << " ptr: " << richTracks[ii] << "%n p:   " << richTracks[ii]->geometry()->momentum().mag() << endm; }
	
	const StPtrVecTrackPidTraits&
	    thePidTraits = richTracks[ii]->pidTraits(kRichId);

	PR(thePidTraits.size());

 	for(size_t jj=0; jj<thePidTraits.size(); jj++) {
	    // loop over traits
	    StRichPidTraits* theRichPidTraits =
		dynamic_cast<StRichPidTraits*>(thePidTraits[jj]);

	    if(!theRichPidTraits) {
	      { LOG_WARN << "Bad pid traits" << endm; }
	      continue;
	    }
 	    PR(theRichPidTraits[jj].productionVersion());

 	    if(!theRichPidTraits[jj].associatedMip()) {
	      { LOG_WARN << "No Associated MIP %n No MIP Residual" << endm; }
 	    }
 	    else {
 		PR(theRichPidTraits[jj].associatedMip()->local());

 		PR(theRichPidTraits[jj].mipResidual());
 		PR(theRichPidTraits[jj].refitResidual());
 	    }
 	    PR(theRichPidTraits[jj].signedDca2d());
 	    PR(theRichPidTraits[jj].signedDca3d());

	    { LOG_DEBUG << " *** Try get the pids" << endm; }
 	    const StSPtrVecRichPid& theRichPids =
 		theRichPidTraits[jj].getAllPids();

	    PR(theRichPids.size());
 	    for(size_t kk=0; kk<theRichPids.size(); kk++) {
 	      { LOG_DEBUG << "kk= " << kk << " " <<  endm; }
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
#endif
    //loop over these tracks
    { LOG_INFO << "========= END ::qualityAssessment =====" << endm; }
}

// ----------------------------------------------------
void StRichSpectraMaker::doIdentification(StTrack* track) {

//     cout << "\nStRichSpectraMaker::doIdentification()\n";
    mHistogram->clearData();

    double p = track->geometry()->momentum().mag();
    mHistogram->setCerenkovQuantities(this->expectedCerenkov(p,-211));
    mHistogram->setCerenkovQuantities(this->expectedCerenkov(p,-321));
    mHistogram->setCerenkovQuantities(this->expectedCerenkov(p,-2212));

    const StSPtrVecRichHit& richHits = mEvent->richCollection()->getRichHits();
//     PR(richHits.size());
    
    const StPtrVecTrackPidTraits&
	thePidTraits = track->pidTraits(kRichId);

    //
    // loop over the PID traits
    // and extract the PIDs
    //

    vector<StRichHit*> theRingHits;
    vector<StRichPhotonInfo*> thePhotonInfo;
    
    StThreeVectorF trackMip(-999.,-999.,-999);
    bool doAssociation = false;
    
    for(size_t jj=0; jj<thePidTraits.size(); jj++) {

	StRichPidTraits* theRichPidTraits =
	    dynamic_cast<StRichPidTraits*>(thePidTraits[jj]);

	if(!theRichPidTraits) {
	  { LOG_WARN << "StRichSpectraMaker::doIdentification() %n Bad pid traits.  Continuing..." << endm; }
	    continue;
	}
#ifndef P00hm
 	if(!theRichPidTraits[jj].associatedMip()) {
	  { LOG_WARN << "StRichSpectraMaker::doIdentification() %n No Associated MIP %n No MIP Residual" << endm; }
	    if(richHits.size()) {
	      { LOG_INFO << "try association" << endm; }
	      doAssociation = true;
	    }
	    else {
	      trackMip =  theRichPidTraits[jj].associatedMip()->local();
	    }
	}
#endif
// 	PR(doAssociation);
	
	const StSPtrVecRichPid& theRichPids =
	    theRichPidTraits[jj].getAllPids();

	//
	// Loops over the "pid" structures for each
	// hypothesis and:
	// 1) copies the hits into a vector
	// 2) checks the photon Information
	//

	mD[0] = -100.; mD[1]=-100.; mD[2]=-100.;
	mNpd[0] = 0; mNpd[1]=0; mNpd[2]=0;
	for(size_t kk=0; kk<theRichPids.size(); kk++) {

	    //
	    // decide the largest azimuth (phi) angle
	    // that will be accepted for the points
	    // of a single particle type
	    //
	    
	    
	    mHistogram->setPhiCut(135.*degree);
	    //PR(mHistogram->currentPhiCut());

	    //
	    // photon info and hits
	    //
	    
	    const StSPtrVecRichPhotonInfo& photonInfo =
 		theRichPids[kk]->getPhotonInfo();
//  	    PR(photonInfo.size());

	    
	    const StPtrVecRichHit& hits =
		theRichPids[kk]->getAssociatedRichHits();
// 	    PR(hits.size());
	    assert(hits.size() == photonInfo.size());
	    
	    if(!hits.size()) {
	      { LOG_WARN << "StRichSpectraMaker:doIdentification() %n No hits in (" << kk << ")...next StRichPid" << endm; }
		continue;
	    }

	    double meanD = 0;
	    double numberOfCts = 0;
 	    for(size_t ll=0; ll<hits.size(); ll++) {
 		double normalizedD = photonInfo[ll]->d();
   		if(normalizedD>-1 && normalizedD<3) {

// 		    if(kk==0&&normalizedD>1) continue; // pion
// 		    if(kk==1&&p>2.8&&normalizedD>1.5) continue; // kaon
		    
		    meanD += normalizedD;
		    numberOfCts+=1.;
 		    theRingHits.push_back(hits[ll]);
// 		    thePhotonInfo.push_back(photonInfo[ll]);
   		}
 	    }
	    if (numberOfCts>0) {
		
		meanD /= numberOfCts;
	    }
	    else {
		meanD = -100.;
	    }
	    
	    
	    mD[kk] = meanD;
	    mNpd[kk] = numberOfCts;
// 	    PR(meanD);
	    
//  	    PR(theRingHits.size());
	}
    }
    
    //
    // only the unique hits
    //
//     PR(theRingHits.size());

    sort(theRingHits.begin(),theRingHits.end());
    vector<StRichHit*> uniqueRingHits( theRingHits.begin(),
				       unique(theRingHits.begin(),
					      theRingHits.end()) );

//     PR(uniqueRingHits.size());
    mUniqueRingHits = uniqueRingHits.size();
	    
    StRichTrack extrapolateTrack(track, mMagField/kilogauss);

    //
    // Argh this is wrong!
    //  but that is the way StRichTrack is coded
    //
    StThreeVectorF trackLocalMomentum = extrapolateTrack.getMomentumAtPadPlane()/GeV;
    StThreeVectorF impactPoint = extrapolateTrack.getImpactPoint();
//     PR(impactPoint);
    
    if(doAssociation) {
	extrapolateTrack.assignMIP(&richHits);
	if(!extrapolateTrack.getAssociatedMIP()) {
	  { LOG_WARN << "StRichSpectraMaker::doIdentification() %n Cannot get an associated Track" << endm; }
	}
	else {
	    trackMip = extrapolateTrack.getAssociatedMIP()->local();
// 	    PR(trackMip);
	}
    }

    //
    // calculate the radiation point and compare with the residuals
    // written on the DST to that calculated from StEvent info
    //
    StThreeVectorF calculatedQuartzRadiationPoint =
	this->calculateRadiationPoint(track, mAverageQuartzRadiationPlanePoint);
    mQuartzRadPoint = calculatedQuartzRadiationPoint;
    
    StThreeVectorF calculatedRadiationPoint =
	this->calculateRadiationPoint(track, mAverageRadiationPlanePoint);
    mRadPoint = calculatedRadiationPoint;

    this->calculateIndex(mRadPoint.y());
    
    
//     PR(mRadPoint);
    
//     PR(mQuartzRadPoint);
//     PR(trackLocalMomentum/GeV);
//     PR(impactPoint);
//     PR(calculatedRadiationPoint);
//     PR(trackResidual);
    
    float tuple[16];
    
    tuple[0] = abs(trackLocalMomentum);
    tuple[1] = trackLocalMomentum.x();
    tuple[2] = trackLocalMomentum.y();
    tuple[3] = trackLocalMomentum.z();

//     PR(calculatedRadiationPoint);
//     PR(mAverageRadiationPlanePoint);
//     PR(mAverageQuartzRadiationPlanePoint);
    mTracer->setTrack(trackLocalMomentum,
		      calculatedRadiationPoint,
		      calculatedQuartzRadiationPoint,
		      mTheIndex);
    tuple[4] = (mTracer->trackAngle()/degree);
//     PR(mTracer->trackAngle()/degree);
#ifdef RICH_WITH_PAD_MONITOR
    //
    // draw a line that shows the direction of the momentum
    // in the xy plane
    //
    
    StThreeVectorF endLine = mAssociatedMip + 10.*(trackLocalMomentum.unit());
    PR(mAssociatedMip);
    PR(endLine);
    mPadMonitor->drawLine(mAssociatedMip,endLine);

    //
    // draw the rings
    //
    // WARNING note that the code change in the calculation of the
    // radiation point will calculated the residual each time
    // a radiation point is calculated and foul up the
    // track tuple when drawing is ON....
    StThreeVectorF topPoint = this->calculateRadiationPoint(track, mTopRadiator);
    StThreeVectorF btmPoint = this->calculateRadiationPoint(track, mBottomRadiator);
    this->drawQuickRing(topPoint, btmPoint);

    //
    // draw the momentum and the inclination
    //
    char txt[50];
    sprintf(txt,"p=%f theta=%f\n",abs(trackLocalMomentum),mTracer->trackAngle()/degree);
    mPadMonitor->drawText(mAssociatedMip.x()+3, mAssociatedMip.y()+3, txt);


    mPadMonitor->update();
#endif

    for(size_t mm=0; mm<uniqueRingHits.size(); mm++) {
	
	StThreeVectorF photonPosition = uniqueRingHits[mm]->local();
// 	PR(photonPosition);
	
// 	double distance = abs(photonPosition - trackMip);
// 	PR(distance);
	
 	mTracer->setPhotonPosition(photonPosition);
	
 	tuple[5] = photonPosition.x();
 	tuple[6] = photonPosition.y();
	
 	double cerenkovAngle;
 	mTracer->processPhoton(&cerenkovAngle);

	//
	// remove photons if they are larger than physically allowable
	//
 	if(mTracer->cerenkovAngle()/degree > 40.5) continue;
//  	PR(mTracer->cerenkovAngle()/degree);
 	tuple[7] = (mTracer->epsilon());
 	tuple[8] = (mTracer->azimuth()/degree);
 	tuple[9] = (mTracer->cerenkovAngle()/degree);
// 	PR(mTracer->cerenkovAngle()/degree);
// 	PR(mTracer->quartzCerenkovAngle()/degree);
 	tuple[10] = (mTracer->quartzCerenkovAngle()/degree);

 	tuple[11] = mMipResidual.x();
 	tuple[12] = mMipResidual.y();
 	tuple[13] = mEvent->id();
	tuple[14] = track->geometry()->charge();
	tuple[15] = mVertexPos.z();
#ifdef RICH_SPECTRA_HISTOGRAM
 	mCerenkov->Fill(tuple);
#endif
	mHistogram->addEntry(StRichCerenkovPhoton(mTracer->cerenkovAngle(),
						  mTracer->azimuth(),
						  uniqueRingHits[mm]));
#ifdef RICH_WITH_PAD_MONITOR
 	// 4 = open circle
 	mPadMonitor->drawMarker(photonPosition,4,2);
 	mPadMonitor->update();
#endif

    } // loop over the hits

    mHistogram->calculate();
    
//     mHistogram->status();
//     unsigned short flag;
//     short np;
//     PR(mHistogram->cerenkovAngle(&flag)/degree);
//     PR(mHistogram->peakAngle(&np)/degree);
//     PR(np);


//     cout << "Next Event? <ret>: " << endl;
//     do {
// 	if(getchar()) break;
//     } while (true);
//     cout << "================== ID END ==================" << endl;
}

// ----------------------------------------------------
void StRichSpectraMaker::calculateResidual(StTrack* track)
{
    // never called
  { LOG_WARN << "StRichSpectraMaker::calculateResidual()" << endm; }
    abort();
    StRichTrack mattTrack(track,mMagField);
    PR(mattTrack.getImpactPoint());
    PR(mattTrack.getProjectedMIP());

    StRichLocalCoordinate lMip(mAssociatedMip.x(),
			       mAssociatedMip.y(),
			       .2);
    PR(lMip);
    StGlobalCoordinate gMip;
    (*mTransform)(lMip,gMip);
    
    StThreeVectorD globalMip(gMip.position().x(),
			     gMip.position().y(),
			     gMip.position().z());
    PR(globalMip);

    // note we use INNER helix here...
    StPhysicalHelixD theHelix = track->geometry()->helix();
    PR(theHelix);
    double sP = theHelix.pathLength(globalMip,mGlobalRichNormal);
    PR(sP);
//     StThreeVectorD globalImpact = theHelix.momentumAt(sP,mMagField);
    StThreeVectorD globalImpact = theHelix.at(sP);

    PR(globalImpact);

    StGlobalCoordinate gProjected(globalImpact.x(),
				  globalImpact.y(),
				  globalImpact.z());
    StRichLocalCoordinate lProjected;
    (*mTransform)(gProjected,lProjected);
    PR(lProjected);

    StThreeVectorF localProjected(lProjected.position().x(),
				  lProjected.position().y(),
				  lProjected.position().z());
    
    mCalculatedResidual = localProjected - mAssociatedMip;
    PR(mCalculatedResidual);
}

// ----------------------------------------------------
void StRichSpectraMaker::calculateIndex(double y)
{
  { LOG_DEBUG << "StRichSpectraMaker::calculateIndex()" << endm; }
    PR(y);
    
    double ypos = (y<0) ? (y+mYExtreme) : y;
    double deltaT    = mTemperatureGradient*ypos;
    
    mTheIndex = (mIndex*(1-mAlpha*deltaT));
    PR(mTheIndex);}
  

// ----------------------------------------------------
StThreeVectorF StRichSpectraMaker::calculateRadiationPoint(StTrack* track, StThreeVectorF& thePlane)
{

//     cout << "StRichSpectraMaker::calculateRadiationPoint()" << endl;
//     PR(thePlane);
//     PR(mAverageRadiationPlanePoint);

    //
    // define the plane (in local coordinates) where the
    // track should extrapolate too.
    //
    
    StRichLocalCoordinate loc(thePlane.x(),
			      thePlane.y(),
			      thePlane.z());
     
    StGlobalCoordinate gNormalRadiationPoint;

    (*mTransform)(loc,gNormalRadiationPoint);
    StThreeVectorD globalNormalRadPoint(gNormalRadiationPoint.position().x(),
					gNormalRadiationPoint.position().y(),
					gNormalRadiationPoint.position().z());

    //StPhysicalHelixD theHelix = track->geometry()->helix();
    StPhysicalHelixD theHelix = track->outerGeometry()->helix();
    double sP = theHelix.pathLength(globalNormalRadPoint,mGlobalRichNormal);
    StThreeVectorD gP = theHelix.momentumAt(sP,mMagField);

//     PR(gP);
//     PR(gP.mag());
//     PR(track->geometry()->momentum());
//     PR(track->geometry()->momentum().mag());
//     PR(track->geometry()->charge());
    StThreeVectorD globalRadPt = theHelix.at(sP);
//     PR(globalRadPt);
    StGlobalCoordinate g2(globalRadPt.x(),
			  globalRadPt.y(),
			  globalRadPt.z());
    StRichLocalCoordinate l2;
//     PR(g2); 
    (*mTransform)(g2,l2);
//     PR(l2);

    StThreeVectorF localRadPoint(l2.position().x(),
				 l2.position().y(),
				 l2.position().z());

//     PR(localRadPoint);
//     PR(track->geometry()->momentum().pseudoRapidity());
    //
    // Correction must be added (in local coordinates)
    // by hand to take into account
    // the residual distribution
    //
    double eta = track->geometry()->momentum().pseudoRapidity();
//     PR(eta);
    StThreeVectorD localCorrectedRadPoint(localRadPoint.x()-.32*eta,
					  localRadPoint.y()+.046*sign(localRadPoint.x()),
					  localRadPoint.z());
//     PR(localCorrectedRadPoint);
    StRichLocalCoordinate lCorrectedRadPt(localCorrectedRadPoint.x(),
					  localCorrectedRadPoint.y(),
					  localCorrectedRadPoint.z());
    StGlobalCoordinate gCorrectedRadPt;

    (*mTransform)(lCorrectedRadPt,gCorrectedRadPt);
    StThreeVectorD globalCorrectedRadPoint(gCorrectedRadPt.position().x(),
					   gCorrectedRadPt.position().y(),
					   gCorrectedRadPt.position().z());
    
    StPhysicalHelixD theNewHelix(theHelix.momentumAt(sP,mMagField),
				 globalCorrectedRadPoint,
				 mMagField,
				 theHelix.charge(mMagField));

    // Can we get a handle on the effect of the residual...
    // define the wire plane as the point of extrapolation
    //StRichLocalCoordinate lPadPlanePt(0.2*centimeter, 0.0, 0.0);
    StRichLocalCoordinate lPadPlanePt(0.0, 0.0,0.2*centimeter);
    StGlobalCoordinate    gPadPlanePt;

    (*mTransform)(lPadPlanePt,gPadPlanePt);
    StThreeVectorD globalPadPlanePoint(gPadPlanePt.position().x(),
				       gPadPlanePt.position().y(),
				       gPadPlanePt.position().z());

    double sPprime =
	theNewHelix.pathLength(globalPadPlanePoint,mGlobalRichNormal);
//     PR(sPprime);
    StThreeVectorD theExtrapolatedPoint = theNewHelix.at(sPprime);


    // translate to local coordinates...
    StGlobalCoordinate gExtrapPt(theExtrapolatedPoint.x(),
				 theExtrapolatedPoint.y(),
				 theExtrapolatedPoint.z());

    StRichLocalCoordinate lExtrapPt;
    (*mTransform)(gExtrapPt,lExtrapPt);

    StThreeVectorD localExtrapPoint(lExtrapPt.position().x(),
				    lExtrapPt.position().y(),
				    lExtrapPt.position().z());
//     PR(localExtrapPoint);
    mCalculatedResidual = localExtrapPoint - mAssociatedMip;
//     PR(mCalculatedResidual);
    
    return localCorrectedRadPoint;
}

// ----------------------------------------------------
bool StRichSpectraMaker::assignMipResidual(StTrack* track) {
	    
    StRichTrack* richTrack = new StRichTrack(track,mMagField);
    
    richTrack->assignMIP(&(mEvent->richCollection()->getRichHits()));
    
    StThreeVectorF projectedMIP  = richTrack->getProjectedMIP();
    if(!richTrack->getAssociatedMIP()) {
      { LOG_DEBUG << "StRichSpectraMaker::Make() %n No Associated MIP %n Skip this track" << endm; }
	delete richTrack;
	richTrack=0;
	return false;
    }
    
    mAssociatedMip = richTrack->getAssociatedMIP()->local();
//     PR(projectedMIP);
//     PR(mAssociatedMip);
    mMipResidual = (projectedMIP - mAssociatedMip);

    delete richTrack;
    richTrack = 0;
    return true;
}

// ----------------------------------------------------
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
    os << "==============================================" << endl;
    os << "\tmDoPhiCut=      " << (mDoPhiCut)              << endl;

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
      { LOG_INFO << "StRichSpectraMaker::drawRichPixels() : No Pixels in the Collection %n Returning" << endm; }
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
      { LOG_INFO << "StRichSpectraMaker::drawRichHits() : No Hits in the Collection %n Returning" << endm; }
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

void StRichSpectraMaker::drawQuickRing(StThreeVectorF& topPoint, StThreeVectorF& bottomPoint)
{
#ifdef RICH_WITH_PAD_MONITOR   
    //
    // generate the points
    //
    PR(mPion->mass());
    vector<StThreeVectorF> iPi = mTracer->calculatePoints(topPoint, mPion->mass());
    vector<StThreeVectorF> oPi = mTracer->calculatePoints(bottomPoint,mPion->mass());
    vector<StThreeVectorF> iKa = mTracer->calculatePoints(topPoint, mKaon->mass());
    vector<StThreeVectorF> oKa = mTracer->calculatePoints(bottomPoint,mKaon->mass());
    vector<StThreeVectorF> iPr = mTracer->calculatePoints(topPoint, mProton->mass());
    vector<StThreeVectorF> oPr = mTracer->calculatePoints(bottomPoint,mProton->mass());
    
    mPadMonitor->drawQuickRing(iPi, oPi, iKa, oKa, iPr, oPr);
    mPadMonitor->update();
#endif
}
