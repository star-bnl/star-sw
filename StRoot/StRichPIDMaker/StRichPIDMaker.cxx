/******************************************************
 * $Id: StRichPIDMaker.cxx,v 2.11 2000/11/01 17:45:21 lasiuk Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 2.11  2000/11/01 17:45:21  lasiuk
 * MAJOR. hitFilter overhaul. members reordered, padplane dimension kept as
 * a member.  addition of initTuple.  Additional dependencies of
 * min/max algorithms
 *
 * Revision 2.21  2000/11/27 17:19:40  lasiuk
 * fill the constant area in teh PID structure
 *
 * Revision 2.20  2000/11/26 15:08:56  lasiuk
 * move the setting of all flags to the pidtraits
 *
 * Revision 2.19  2000/11/25 12:27:12  lasiuk
 * mean angle -> psi.  Fill the photonInfo.  take care of flag
 * ambiguities where possible.  Remove the old commented hitFilter
 * code.  Store the TOTAL CONSTANT AREA and TOTAL CONSTANT AREA ON
 * ACTIVE PAD-PLANE
 *
 * Revision 2.18  2000/11/23 01:46:15  lasiuk
 * pt threshold modification
 *
 * Revision 2.17  2000/11/22 16:58:05  lasiuk
 * Uniform setting of flags in two places
 * remove dependence of dip angle on mean and sigma
 *
 * Revision 2.16  2000/11/21 19:49:13  lasiuk
 * fill the photon d in the StRichPid
 * remove parameterized dip angle dependence
 * of the mean/sigma d.  Setting of hitflags
 * in fillPidTraits
 *
 * Revision 2.15  2000/11/21 16:24:22  horsley
 * Major overhaul of StRichArea, introduced monte carlo integration cross check,
 * all possible areas, angles calculated together. StRichRingCalculator,
 * StRichPIDMaker modified to support new StRichArea. StRichPIDMaker's hit finder
 * typo corrected.
 *
 * Revision 2.14  2000/11/14 22:32:50  lasiuk
 * order of setting flags corrected
 *
 * Revision 2.13  2000/11/07 14:30:58  lasiuk
 * d<3 adjustment and checkTrack() naming of varaiables
 * corrected
 *
 * Revision 2.12  2000/11/07 14:11:39  lasiuk
 * initCutParameters() and diagnositis print added.
 * bins for <d> and sigma_d added.
 * TPC hits for RICH tracks written out.
 * (file) ptr checked before writing ntuple.
 * check flags on Hits instead of ADC value
 *
 * Revision 2.11  2000/11/01 17:45:21  lasiuk
 * MAJOR. hitFilter overhaul. members reordered, padplane dimension kept as
 * a member.  addition of initTuple.  Additional dependencies of
 * min/max algorithms
 *
 * Revision 2.10  2000/10/26 20:29:55  lasiuk
 * move filename of ntuple into ifdef'd region
 *
 * Revision 2.9  2000/10/19 15:41:57  horsley
 * added set format option to TFile, file->SetFormat(1);
 *
 * Revision 2.8  2000/10/19 06:12:52  horsley
 * fixed pointer problem in fillPIDNtuple member function
 *
 * Revision 2.7  2000/10/19 01:13:23  horsley
 * added member functions to StRichPIDMaker to make cuts on hits, tracks, events.
 * added normal distance sigma cut on hits, quartz and radiator pathlengths
 * for individual photons, modified minimization routine to correct boundary
 * problems
 *
 * Revision 2.6  2000/10/03 19:26:01  horsley
 * fixed error in StRichTrack correct member function, now returns bool.
 *
 * Revision 2.5  2000/10/02 23:21:29  horsley
 * *** empty log message ***
 *
 * Revision 2.4  2000/10/02 23:06:33  horsley
 * *** empty log message ***
 *
 * Revision 2.3  2000/09/29 17:55:51  horsley
 * fixed bug in Minimization routine, included StMagF stuff (commented out)
 * changed StRichRingPoint  HUGE_VALUE   ---> MAXFLOAT for default value
 *
 * Revision 2.2  2000/09/29 01:35:37  horsley
 * Many changes, added StRichRingHits, StRichMcSwitch, TpcHitvecUtilities
 * Modified the StRichCalculator, StRichTracks, StRichMCTrack, StRichRingPoint
 *
 * Revision 1.5  2000/06/16 02:37:11  horsley
 * many additions, added features to pad plane display (MIPS, rings, etc)
 * along with Geant info. Added StMcRichTrack. Modified access to hit collection.
 *
 * Revision 1.3  2000/05/22 15:14:44  horsley
 * modified StRichRings, StRichDrawableTRings to comply with sun compiler
 *
 * Revision 1.2  2000/05/19 19:06:10  horsley
 * many revisions here, updated area calculation ring calc, ring, tracks , etc...
 *
 * Revision 1.1  2000/04/03 19:36:08  horsley
 * initial revision
 ******************************************************/
#include "StRichPIDMaker.h"

#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::min;
using std::max;
#endif

// switches
#include "StRichDisplayActivate.h"
#include "StRichTrackingControl.h"
#include "StRichMcSwitch.h"
#define myrICH_WITH_NTUPLE 1
#include "StEvent/StTpcDedxPidAlgorithm.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StRichPidTraits.h"
#include "StEvent/StRichPid.h"

// root
#include "TH1.h"
#include "TH3.h"
#include "TNtuple.h"

#include <fstream.h>
#include <math.h>
#ifdef SUN
#include <ieeefp.h>
#endif

//
// Pad monitor (StRichDisplayActivate)
#ifdef RICH_WITH_PAD_MONITOR
#include "StRichDisplayMaker/StRichDrawableTTrack.h"
#include "StRichDisplayMaker/StRichDrawableTRings.h"
#include "StRichDisplayMaker/StRichPadMonitor.h"
#endif

#ifdef RICH_WITH_L3_TRACKS
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/L3/L3_Reader.hh"
#include "StDaqLib/TPC/trans_table.hh"
#include "StDAQMaker/StDAQReader.h"


#include "StRichTrackFilter.h"
// StarClassLibrary
#include "StGlobals.hh"
#include "StParticleTypes.hh"
#include "StParticleDefinition.hh"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "StMemoryInfo.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

// StEvent
  // initialize cuts
  mPtCut = 0.0; // GeV/c
  mEtaCut = 0.5; 
  mLastHitCut = 160.0*centimeter;
  mDcaCut = 3.0*centimeter;
  mFitPointsCut = 20;
  mAdcCut = 300; 
  mPathCut = 500*centimeter;
  mPadPlaneCut = 2.0*centimeter;
  mRadiatorCut = 2.0*centimeter;

  mThresholdMomentum=.5*GeV;
#include "StEventMaker/StRootEventManager.hh"

// StRrsMaker, StRchMaker
#include "StRrsMaker/StRichGeometryDb.h"
    mPtCut = 0.0*GeV; // GeV/c
    mEtaCut = 0.5; 
    mRefit = false;

    mLastHitCut = 160.0*centimeter;
    mDcaCut = 3.0*centimeter;
    mFitPointsCut = 20;
    mPathCut = 500*centimeter;
    mDoGapCorrection      = true;
    mPionSaturatedArea    = 100.0;
    mKaonSaturatedArea    = 100.0;
    mProtonSaturatedArea  = 100.0;
    
    mPadPlaneCut = 2.0*centimeter;
    mRadiatorCut = 2.0*centimeter;

    mThresholdMomentum=.3*GeV;
    mListOfParticles[2]  = proton;
    //
    // Convergence parameter for psi determination
    pion    = StPionMinus::instance();
    kaon    = StKaonMinus::instance();
    proton  = StAntiProton::instance();
}
    mListOfParticles.clear();
    mMomentumTransformation = StRichMomentumTransform::getTransform(mGeometryDb);
    mPadMonitor = 0;
    

    mRefit = false;

    mDoGapCorrection      = true;
    mPionSaturatedArea    = 100.0;
    if (mDefaultShortWave != mShortWave && mDefaultLongWave != mLongWave) {
    
    //
    // hard coded values for now!
    // can be used to change wavelenghts
    mPadPlaneDimension = mGeometryDb->padPlaneDimension();
    
    cout << " mGeometryDb->padPlaneDimension() " << mPadPlaneDimension << endl;
    
    mDefaultLongWave  = 200.0e-7;
    
    if ( (mDefaultShortWave != mShortWave) &&
	 (mDefaultLongWave  != mLongWave) ) {
	mMaterialDb->setWavelengthRange(mDefaultShortWave,mDefaultLongWave);
    }
    
    mPadPlaneDimension = mGeometryDb->padPlaneDimension();
  //
    this->printCutParameters();
	
    this->initNtuples();
    
    return StMaker::Init();
}
  // set pad plane display pointer to zero
  //
  mPadMonitor = 0;
  
  //
  //
  mDoGapCorrection      = true;
    
  
  //
  // hard coded values for now!
  // this->setWaveLenght(small,large)
  // can be used to change wavelenghts
    StMcEvent* mEvent = 0;
  mDefaultShortWave = 160.0e-7;
  mDefaultLongWave  = 200.0e-7;
  
  if ( (mDefaultShortWave != mShortWave) &&
       (mDefaultLongWave  != mLongWave) ) {
    mMaterialDb->setWavelengthRange(mDefaultShortWave,mDefaultLongWave);
  }
  
  mPadPlaneDimension = mGeometryDb->padPlaneDimension();
  
  //
  // print cut parameters upon initialization
  //
  this->printCutParameters();
  this->initNtuples();
  
  return StMaker::Init();
}


void StRichPIDMaker::Clear(Option_t *opt) {
    StMaker::Clear();
}


Int_t StRichPIDMaker::Make() { 
    
    cout << "StRichPIDMaker::Make()" << endl;
    mPrintThisEvent = false;
    mNumberOfRingHits=0;
    
#ifdef myRICH_WITH_MC
    // StMcEvent
    mEvent = 0;
    mEvent = ((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
    if (!mEvent) {
	cout << "StRichPIDMaker:Make() ---> No StMcEvent! Return kStWarn." << endl;
	return kStWarn;
    }
#endif
    
    //
    // grab StEvent 
    //
    StEvent* rEvent;
    rEvent = (StEvent *) GetInputDS("StEvent");  
    
    //
    // Load vertex and number of primaries
    //
    if (!this->checkEvent(rEvent)) return kStWarn;
    
    //
    // Initialize Parameters
    //
    mRichTracks        = 0;
    mNumberOfPrimaries = 0;
    mNegativePrimaries = 0;
    
    //
    // get hits, clusters, pixels from StEvent
    //
    const StRichCollection*     richCollection = rEvent->richCollection();
    
    if (!richCollection) {
	cout << "StRichPIDMaker::Make()\n";
	cout << "\tERROR: Cannot get richCollection from StEvent\n";
		cout << "StRichPIDMaker::Make()\n";
		cout << "\tWARNING: Cannot get the dstPoints\n";
		cout << "\tContinuing..." << endl;
	    }
	    else {
		for(ii=0; ii<mListOfStRichTracks.size(); ii++) {
		    StTrack* currentTrack = mListOfStRichTracks[ii]->getStTrack();
		    if(!currentTrack->detectorInfo()) {
			cout << "StRichPIDMaker::Make()\n";
			cout << "\tassocciated with the track.  Continuing..." << endl;
			continue;
    for (size_t ii=0; ii<mListOfStRichTracks.size(); ii++) {     
		    
	

    //
    // The StTpcHitCollection should exist at this
    // point whether it is created by the PIDMaker
    // or not

    
    // 
    // This is the beginning of the main PID loop
    //
    


	    if ( richTrack->fastEnough(mListOfParticles[iParticle]) &&
		 richTrack->isGood(mListOfParticles[iParticle])       ) {
		
		ringCalc = new StRichRingCalculator(richTrack,mListOfParticles[iParticle]);
		
		this->hitFilter(pRichHits,ringCalc);

		this->fillPIDTraits(ringCalc);

		delete ringCalc;  

		ringCalc = 0;
	    }
	if(richTrack->getStTrack()->detectorInfo())

      
	  // and if the back portion of the ring is not refracted away
	  //
	  if ( richTrack->fastEnough(mListOfParticles[iParticle]) &&
	       richTrack->isGood(mListOfParticles[iParticle]) ) {
//  	unsigned short numberOfTpcHits = tpcHits.size();
	    track->addPidTraits(richTrack->getPidTrait());
	    //
	
	    this->hitFilter(pRichHits,ringCalc);
		 richTrack->isGood(mListOfParticles[iParticle]) ) {
	    this->fillPIDTraits(ringCalc);
	    
	    delete ringCalc;  
	    
	    ringCalc = 0;
	  }
	StRichTrack* richTrack = mListOfStRichTracks[ii];
	
	
	StRichPidTraits* theTraits = richTrack->getPidTrait();

	if(!theTraits) {
	    cout << "StRichPIDMaker::Make()\n";
	    cout << "\tERROR Processing the PIDTraits\n";
	  track->addPidTraits(richTrack->getPidTrait());
	    cout << "\tContinuing..." << endl;
	    continue;
	}
    	if(!this->reprocessTheTraits(theTraits)) {
	    //
	    // if necessary I can take action here
	    //
	}

	//
    
    // 
    // test the correction to tracks trajectory
    //  
    //   StRichTrack* richTrack = 0;
    //   StParticleDefinition* particle  = 0;
    //   ringCalc  = 0;
    //   for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {     
    //     richTrack = mListOfStRichTracks[trackIndex];
    //       richTrack->correct();
    
    //       cout << "Track original p = " << richTrack->getUnCorrectedMomentum() << endl;
    //       cout << "Track p = " << richTrack->getMomentum() << endl;
    
    
    //       richTrack->clearHits();
    //       for (size_t particleIndex=0; particleIndex<mListOfParticles.size(); particleIndex++) {
    // 	particle = mListOfParticles[particleIndex];
    // 	if (richTrack->fastEnough(particle) && richTrack->isGood(particle)) { 
    // 	  ringCalc = new StRichRingCalculator(richTrack,particle);
    // 	  this->hitFilter(pRichHits,ringCalc);
    // 	  delete ringCalc;  ringCalc = 0;
    // 	}
    //   if (kWriteNtuple) this->fillCorrectedNtuple();
    cout << "StRichPIDMaker::Finish()  writing file to tape" << endl;
    file->Write();
    file->Close();
    delete file;
    file = 0;
    // 
    // end of PID loop
    //
    
    //
    // draw rings on padplane, fill software monitor 
    //
    this->drawPadPlane(rEvent,mPrintThisEvent);
    this->fillRichSoftwareMonitor(rEvent);
    
    //
    // fill ntuples
    //
#ifdef  myRICH_WITH_NTUPLE 
    if (kWriteNtuple) this->fillEvtNtuple(rEvent, mNumberOfPrimaries, mNegativePrimaries, pRichHits,pRichPixels);
    if (kWriteNtuple) this->fillPIDNtuple();
    if (kWriteNtuple) this->fillHitNtuple(pRichHits,pRichClusters);
    if (kWriteNtuple) this->fillOverLapHist(pRichHits);
    
#ifdef myRICH_WITH_MC
    if (kWriteNtuple) this->fillMcTrackNtuple(pRichClusters);
    if (kWriteNtuple) this->fillMcPixelNtuple(pRichPixels);
    if (kWriteNtuple) this->fillMcPhotonNtuple(mEvent,pRichClusters,pRichHits);
    if (kWriteNtuple) this->fillGeantHitNtuple();
#endif

#endif    
    
    return kStOK;
}

Int_t StRichPIDMaker::Finish() {
    
    cout << "StRichPIDMaker::Finish()" << endl;
    this->printCutParameters();

#ifdef  myRICH_WITH_NTUPLE 

    if(file) {
	cout << "StRichPIDMaker::Finish()  writing file to tape" << endl;
	file->Write();
	file->Close();
	delete file;
	file = 0;
    }
#endif
    
    return kStOK;
}

void StRichPIDMaker::setTrackRefit(bool refit) {mRefit = refit;}

void StRichPIDMaker::clearTrackList() {
    
    for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
		if (mRefit) tempTrack->correct();

	delete mListOfStRichTracks[trackIndex];
	mListOfStRichTracks[trackIndex] = 0;
    }
    mListOfStRichTracks.clear();
    mListOfStRichTracks.resize(0);
}

Int_t StRichPIDMaker::fillTrackList(StEvent* tempEvent, const StSPtrVecRichHit* richHits) {
  
    mNumberOf1GeV=0;
    this->clearTrackList();
    
    mNumberOfPrimaries = tempEvent->primaryVertex()->numberOfDaughters();  
    
    for (int ii=0; ii<mNumberOfPrimaries; ii++)  {

	StTrack* track = tempEvent->primaryVertex()->daughter(ii);

	if (this->checkTrack(track)) {
	    
#ifdef myRICH_WITH_MC    
	    StRichMCTrack* tempTrack = new StRichMCTrack(track,mMagField);
	    cout << "StRichPIDMaker::fillTrackList()  -->  creating StMcTrack!" << endl;
#else 
	    StRichTrack* tempTrack   = new StRichTrack(track,mMagField);
    if (!makeTrackAssociations(mEvent,pRichHits)) {

//     ofstream os("/dev/null",ios::app);
//     os << "**PDG " <<  ringCalculator->
// 	getRing(eInnerRing)->getParticleType()->pdgEncoding() << endl;
		// set the eAssociatedMIP flag
		//
		tempTrack->assignMIP(richHits);

		if (tempTrack->getMomentum().mag()>1) mNumberOf1GeV++;	

		//
		// Add an StRichPidTrait to the StRichTrack
		// ...this will be passed to StEvent
		//
		tempTrack->addPidTrait(new StRichPidTraits());

		mListOfStRichTracks.push_back(tempTrack);
	    }
	    else {
		delete tempTrack;
		tempTrack=0;
	    }
	}
	//	if (tempTrack->geometry()->charge() < 0) {
//     os << "Track Momentum: " << ( abs(trackMomentum) ) << endl;
	    mNegativePrimaries++;
	}
    }
 
    //
    // make monte carlo associations 
    //
#ifdef myRICH_WITH_MC
    if (!makeTrackAssociations(mEvent,richHits)) {
	cout << "can not make track associations. Return kStWarn! " << endl;
	return kStWarn;
    }
#endif

//     os << "CENTRAL HIT: " << central << endl; 
    // make sure the hits exist
    //
    if(!richHits) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tERROR no hits..." << endl;
	return;
    }

    StThreeVectorF pointOnRing;

    StRichTrack* currentTrack = ringCalculator->getRing(eInnerRing)->getTrack();
//     os << "particleType " << particleType << endl;
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tERROR no track" << endl;
	return;
    }

    StThreeVectorF trackMomentum = currentTrack->getMomentum();
//      os << "Track Momentum: " << ( abs(trackMomentum) ) << endl;

//     PR( abs(trackMomentum) );
	cout << "\tCentral Hit is not classified by the\n";
//     os << "Drawing central " << central << endl;
    if(!checkTrackMomentum(abs(trackMomentum))) return;
    
    StRichHit* centralHit = currentTrack->getAssociatedMIP();
    if(!centralHit) {
//     os << "LOOP OVER HITS start" << endl;
	cout << "\tERROR no associated MIP" << endl;
	return;
    float distHits[28];

    //
    // TBA a data member
    //
    double mPrecision = 100*micrometer;
    
    // IF the 
    if( !centralHit->isSet(eMip) ) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tWARNING!\n";
	cout << "\tCentral Hit is not classified as\n";
	cout << "\ta MIP (" << centralHit->charge() << ")\n";
	cout << "\tContinuing...for now..." << endl;
	//return;
    }
    //
    // simple counters for # photons per ring
    //
	if (! this->checkHit( (*hitIter) ) ) continue;
    int pi2=0;
    int ka=0;
    int ka2=0;
    int pr=0;
    
    StThreeVectorF central = centralHit->local();
//      os << "CENTRAL HIT: " << central << endl; 

    //
    // Careful in assignment of projected MIP the position
    // is taken at the Anode wire plane
    //
    StThreeVectorF mipResidual =
	( central - currentTrack->getProjectedMIP() ).perp(); 

    StParticleDefinition* particle =
	ringCalculator->getRing(eInnerRing)->getParticleType();
    int particleType = particle->pdgEncoding();
//      os << "particleType " << particleType << endl;


// 	os << "\n***\noldsigma " << oldsigma << endl;
// 	os << "looping hit" << hit << endl;
    mPadMonitor->drawMarker(central);
#endif
    
//      os << "LOOP OVER HITS start" << endl;

#ifdef myRICH_WITH_NTUPLE
    float distHits[29];
#endif

    //
// 	os << "referenceLine " << referenceLine << endl;
    StSPtrVecRichHitConstIterator hitIter;
    for (hitIter = richHits->begin();
	 hitIter != richHits->end(); hitIter++) {

	if( (*hitIter) == centralHit) continue;
	if ( !(*hitIter)->isSet(ePhotoElectron) ) continue;

	float idist = ringCalculator->getInnerDistance(hit,innerAngle);
// 	os << "idist " << idist << "\tinnerAngle " << innerAngle << endl;

	//
	// we are waiting for the database
// 	    os << "StRichPIDMaker::hitFilter()\n";
// 	    os << "\tPANIC\n";
// 	    os << "\tINITIAL ANGLE DOES NOT CORRESPOND";
// 	    os << "\tTO A POINT ON THE PAD PLANE. SKIP" << endl;
	// OLD VARIABLE CALCULATION
	//
// 	os << "pointOnRing" << pointOnRing << endl;
	outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
	mPadMonitor->drawMarker(hit,26);
// 	os << "ringPointLine " << ringPointLine << endl;
	double quartzPath    = ringCalculator->getMeanPathInQuartz();
 	float idist = ringCalculator->getInnerDistance(hit,innerAngle);
// 	os << "psi " << psi << endl;
	double olddist  = ((outerDistance/ringWidth > 1 &&
			    (innerDistance/ringWidth < outerDistance/ringWidth )) ? 
			   (-1.0):(1.0))*innerDistance/ringWidth;
	double oldsigma = this->getHitSigma(olddist);
	  

	photonNumber++;
//  	os << "\n***\noldsigma " << oldsigma << endl;
//  	os << "looping hit" << hit << endl;

#ifdef RICH_WITH_PAD_MONITOR 
// 	mPadMonitor->drawMarker(hit,26);
#endif

	//
	// Find the angle Psi on the inner ring that intersects
	// the line from the MIP to the hit
	// 
	StThreeVectorF referenceLine = (hit-central);
// 	os << "sineTheta* (" << signOfTheta << ") " << sineTheta << endl;
#ifdef RICH_WITH_PAD_MONITOR 
	//mPadMonitor->drawLine(central,hit);
#endif
	// 	os << "minDistToRefLine " << minDistToRefLine << endl;
	// Find the minimum distance to the InnerRing
	// from the hit
	//
//  	float idist = ringCalculator->getInnerDistance(hit,innerAngle);
//  	os << "idist " << idist << "\tinnerAngle " << innerAngle << endl;
		
	ringCalculator->getRing(eInnerRing)->getPoint(innerAngle,pointOnRing);
	if(pointOnRing.x() == FLT_MAX) {
//  	    os << "StRichPIDMaker::hitFilter()\n";
//  	    os << "\tPANIC\n";
//  	    os << "\tINITIAL ANGLE DOES NOT CORRESPOND";
//  	    os << "\tTO A POINT ON THE PAD PLANE. SKIP" << endl;
	    continue;
	}
// 	os << "initial step " << step << endl;

	StThreeVectorF ringPointLine = pointOnRing - central;
//  	os << "ringPointLine " << ringPointLine << endl;

	double psi = innerAngle;
//  	os << "psi " << psi << endl;

	//
	// Find the distance of closest approach from the ring
	// point to the reference line:
	//
	//        ^   ^
	// | A |  A x B
	//
	// where:
	//        A = ringPointLine
	//        B = referenceLine
	//
	// z component =
	    ctr++; // os << "ctr* " << ctr << "\t" << "psi " << psi << endl;

	int signOfTheta =
// 	    os << "newPointOnRing " << newPointOnRing << endl;
	double sineTheta = abs( (ringPointLine.unit()).cross(referenceLine.unit()) ); 
// 		os << "StRichPIDMaker::hitFilter()\n";
// 		os << "REFRACTED AWAY" << endl;
	float minDistToRefLine =
	    fabs(ringPointLine.mag())* sineTheta;
// 	os << "minDistToRefLine " << minDistToRefLine << endl;

		    
	StThreeVectorF newPointOnRing;
	bool anotherIteration = true;
	bool convergence      = false;

	double step      = 1.*degree;
	double maxChange = 5.*degree;
// 	    os << "sineNewTheta (" << signOfNewTheta << ") " << sineTheta << endl;
	//
	// take the initial step according to the sign of theta
	//
	
// 	    os << "newDistToRefLine " << newDistToRefLine << endl;
// 	    os << "sineTheta " << sineTheta << endl;

	int ctr = 0;
	while (anotherIteration && ctr<maxIterForInitialPsi) {

	    //
	    // make sure you are in range
	    //
// 		    if(psi>0 && psi>M_PI) {
// 			os << "Adjustment (-1)" << endl;
// 	    os << "step " << step << endl;
// 			step *= -1.;
// 		    }
// 		    if(psi<0 && psi< (-1.*M_PI) ) {
// 			os << "Adjustment (+1)" << endl;
// 			psi += (2.*M_PI);
// 			step *= -1.;
// 		    }
		    
	    psi += step;
	    ctr++;
// 	    os << "ctr* " << ctr << "\t" << "psi " << psi << endl;

	    ringCalculator->getRing(eInnerRing)->getPoint(psi,newPointOnRing);
//  	    os << "newPointOnRing " << newPointOnRing << endl;
	    if(newPointOnRing.x() == FLT_MAX) {
// 		    os << "BRAKES step " << step << endl;
//  		os << "REFRACTED AWAY" << endl;
		psi -= step;
		step *= 0.5;
		continue;
	    }
// 	    os << "step " << step << endl;
	    ringPointLine = newPointOnRing - central;

	    int signOfNewTheta =
		sign(ringPointLine.x()*referenceLine.y() -
		     ringPointLine.y()*referenceLine.x());
	    sineTheta = abs( (ringPointLine.unit()).cross(referenceLine.unit()) ); 
//  	    os << "sineNewTheta (" << signOfNewTheta << ") " << sineTheta << endl;
		    
	    double newDistToRefLine = 
		abs(ringPointLine)* sineTheta;

// 	os << "Drawing innerRingPoint " << innerRingPoint << endl;
//  	    os << "sineTheta " << sineTheta << endl;

	    if(ctr > maxIterForRingPsi)
		anotherIteration = false;

	    if (newDistToRefLine<mPrecision) {
		convergence = true;
// 	os << "current best guess for psi " << psi << endl;
	    }
		    
//  	    os << "step " << step << endl;

	    if( (signOfTheta != signOfNewTheta) ||
		( (signOfTheta == signOfNewTheta) &&
		  (newDistToRefLine > minDistToRefLine) ) ) {
		// wrong way
		signOfTheta = signOfNewTheta;
		step *= -0.5;
	    }
	    else {
		//make the step size bigger, unless you are close
		//change in the distances (check angles)
		step = (step>0) ?
		    min(maxChange,(step*1.2)) : max(-1.*maxChange,step*1.2);

		if(newDistToRefLine < 3.*centimeter) {
// 		os << "An OuterRingPoint DOES NOT EXIST with this psi..( "
// 		   << iterationNumber << ")" << endl;
			min(1.*degree,step) : max(-1.*degree,step);
		}

	    } // else
//  	    os << "step " << step << endl;

	    minDistToRefLine = newDistToRefLine;
// 	os << "old psi " << psi << endl;
	};
// 	os << "new (starting) psi " << psi << endl;
	if(!convergence) continue;

	//
	// assign the point on the ring
	//
	StThreeVectorF innerRingPoint = newPointOnRing;
//  	os << "Drawing innerRingPoint " << innerRingPoint << endl;
#ifdef RICH_WITH_PAD_MONITOR 
// 	    os << "ERROR: PANIC BAD POINT...continue" << endl;
#endif
	//
	// Given an initial guess for psi, find the "line of constant psi"
	// which crosses both rings and intersects with the hit
	//
//  	os << "current best guess for psi " << psi << endl;
		
	//
	// determine the outerRingPoint with the corresponding Psi
// 	os << "consPsiVector " << consPsiVector << endl;
	StThreeVectorF outerRingPoint;
// 	mPadMonitor->drawLine(innerRingPoint,outerRingPoint);
	int iterationNumber = 0;
	double littleStep   = 1.*degree;
	double modifiedPsi = psi;
	if(modifiedPsi < 0) littleStep *= -1.;

	while(iterationNumber<maxIterForInitialPsi) {
	    iterationNumber++;
	    ringCalculator->getRing(eOuterRing)->getPoint(modifiedPsi,outerRingPoint);
		    
	    if( (outerRingPoint.x() == FLT_MAX) ||
		( (fabs(outerRingPoint.x() > mGeometryDb->padPlaneDimension().x())) ||
		  (fabs(outerRingPoint.y()) > mGeometryDb->padPlaneDimension().y()) ) ) {
//  		os << "An OuterRingPoint DOES NOT EXIST with this psi..( "
//  		   << iterationNumber << ")" << endl;
		modifiedPsi+=littleStep;
	    }
	    else {
		// we found a point
		break;
	    }
	};
//  	os << "old psi " << psi << endl;
	psi = modifiedPsi;
//  	os << "new (starting) psi " << psi << endl;

	ringCalculator->getRing(eInnerRing)->getPoint(psi,innerRingPoint);
	ringCalculator->getRing(eOuterRing)->getPoint(psi,outerRingPoint);

// 	os << "sineTheta2 (" << signOfTheta << ") " << sineTheta << endl;
	mPadMonitor->drawMarker(innerRingPoint,2,1);
	mPadMonitor->drawMarker(outerRingPoint,2,1);
	    (fabs(innerRingPoint.y()) > mPadPlaneDimension.y())  ) {
//  	    os << "ERROR: PANIC BAD POINT...continue" << endl;
// 	os << "minDistToConsPsiRefLine " << minDistToConsPsiRefLine << endl;
	}


	//
	// the constant Psi line between the inner and outer rings
	//
	
	StThreeVectorF consPsiVector = outerRingPoint - innerRingPoint; 
//  	os << "consPsiVector " << consPsiVector << endl;
#ifdef RICH_WITH_PAD_MONITOR 
// 	os << "initial step " << step << endl;
// 	mPadMonitor->drawMarker(innerRingPoint,2,1);
// 	mPadMonitor->drawMarker(outerRingPoint,2,1);
#endif

	//
	// the line from the innerRing Point to the hit
	//
	StThreeVectorF consPsiRefLine = hit - innerRingPoint;

	//
	// minimize the distance of closest approach between the
	// two lines as a function of the angle of the psi line
	//
	//        ^   ^
	// | A |  A x B
	//
	// where:
	//        A = consPsiRefLine
	//        B = consPsiVector
	//
	// z component =
	//               AxBy - AyBx
	//
// 	    os << "newInnerPointOnRing " << newInnerPointOnRing << endl;
// 	    os << "newOuterPointOnRing " << newOuterPointOnRing << endl;
	    sign(consPsiRefLine.x()*consPsiVector.y() -
		 consPsiRefLine.y()*consPsiVector.x());
	sineTheta = abs( (consPsiRefLine.unit()).cross(consPsiVector.unit()) ); 
// 		os << "REFRACTED AWAY" << endl;
		
	double minDistToConsPsiRefLine =
	    abs(consPsiRefLine) * sineTheta; 

//  	os << "minDistToConsPsiRefLine " << minDistToConsPsiRefLine << endl;
		
	anotherIteration = true;
	convergence      = false;
	step = 1.*degree;

	StThreeVectorF newInnerPointOnRing;
	StThreeVectorF newOuterPointOnRing;
	double newMinDistToConsPsiRefLine;

	if(signOfTheta>0) step *= -1.;
//  	os << "initial step " << step << endl;
	ctr = 0;
	while (anotherIteration || ctr<maxIterForRingPsi) {

	    //
// 	    os << "sineTheta 2*(" << signOfNewTheta << ") " << sineTheta << endl;
	    //
// 		    if(psi>0 && psi>M_PI) {
// 			os << "Adjustment (-2)  psi " << psi << endl;
// 			psi -= (2.*M_PI);
// 	    os << "newMinDistToConsPsiRefLine " << newMinDistToConsPsiRefLine << endl;
// 		    }
// 		    if(psi<0 && psi< (-1.*M_PI) ) {
// 			os << "Adjustment (+2)  psi " << psi << endl;
// 			psi += (2.*M_PI);
// 			step *= -1.;
// 		    }
		    
	    psi += step;
	    ctr++; // os << "ctr " << ctr << "\t" << "psi " << psi << endl;

	    ringCalculator->getRing(eInnerRing)->getPoint(psi,newInnerPointOnRing);
	    ringCalculator->getRing(eOuterRing)->getPoint(psi,newOuterPointOnRing);

//  	    os << "newInnerPointOnRing " << newInnerPointOnRing << endl;
//  	    os << "newOuterPointOnRing " << newOuterPointOnRing << endl;

	    if( (newInnerPointOnRing.x() == FLT_MAX) ||
		(newOuterPointOnRing.x() == FLT_MAX) ) {
//  		os << "REFRACTED AWAY" << endl;
		psi-=step;
		step *=0.5;
		continue;
	    }
// 		    os << "BRAKES(2) " << endl;
	    consPsiRefLine = hit - newInnerPointOnRing;
	    consPsiVector  = newOuterPointOnRing - newInnerPointOnRing; 

	    //
	    //        ^   ^
	    // | A |  A x B
// 	    os << "step " << step << endl;
	    // where:
	    //        A = consPsiRefLine
	    //        B = consPsiVector
	    
// 	os << "minDistToConsPsiRefLine " << minDistToConsPsiRefLine << endl;
// 	os << "psi " << psi << endl;
		     consPsiRefLine.y()*consPsiVector.x());
	    sineTheta = abs( (consPsiRefLine.unit()).cross(consPsiVector.unit()) ); 
//  	    os << "sineTheta 2*(" << signOfNewTheta << ") " << sineTheta << endl;

	    newMinDistToConsPsiRefLine =
		abs(consPsiRefLine) * sineTheta;

//  	    os << "newMinDistToConsPsiRefLine " << newMinDistToConsPsiRefLine << endl;
		    
	    if( ctr > maxIterForRingPsi )
		anotherIteration = false;

	    if(newMinDistToConsPsiRefLine < mPrecision) {
		convergence = true;
// 	os << "photonNumber " << photonNumber << endl;
// 	os << "abs(lengthOfD) " << abs(lengthOfD) << endl;
// 	os << "consPsiVector " << abs(consPsiVector) << endl;
	    if( (signOfTheta != signOfNewTheta) ||
		( (signOfTheta == signOfNewTheta) &&
		  (newMinDistToConsPsiRefLine > minDistToConsPsiRefLine) ) ) {
		//wrong way
		signOfTheta = signOfNewTheta;
// 	os << "abs(hitDVector) " << abs(hitDVector) << endl;
// 	os << "consPsiRefLine " << consPsiRefLine << endl;
	    else {
		// make the step size bigger if we are moving
		// in the right direction
		step = (step>0) ?
// 	os << "cosineOfD " << cosineOfD << endl;

		if(newMinDistToConsPsiRefLine < 3.*centimeter) {
// 	os << "normalizedD " <<  normalizedD << endl;
	StThreeVectorF hitDVector = hit - innerRingPoint;
	    }

	    minDistToConsPsiRefLine = newMinDistToConsPsiRefLine;
//  	    os << "step " << step << endl;
	};

	if(!convergence) continue;
		
//  	os << "minDistToConsPsiRefLine " << minDistToConsPsiRefLine << endl;
//  	os << "psi " << psi << endl;
		
#ifdef RICH_WITH_PAD_MONITOR 
// 	mPadMonitor->drawMarker(newInnerPointOnRing,2,1);
// 	mPadMonitor->drawLine(newInnerPointOnRing,hit);
#endif
	
	//
	// Calculated Quantities
	
	
	//
	distHits[9]  = hit.x();
	distHits[10] = hit.y();
	StThreeVectorF lengthOfD = newOuterPointOnRing - newInnerPointOnRing;
//  	os << "photonNumber " << photonNumber << endl;
	distHits[11] = central.x();
	distHits[12] = central.y();

	distHits[13] = trackMomentum.x();
	distHits[14] = trackMomentum.y();
	distHits[15] = trackMomentum.z();

	StThreeVectorF hitDVector = hit - newInnerPointOnRing;

	distHits[16] = ringCalculator->getRing(eInnerRing)->getTrack()->getTheta()/degree;
	distHits[17] = mEventN;
		
	distHits[18] = photonNumber;
		
	distHits[19] = mipResidual.x();
	distHits[20] = mipResidual.y();
	distHits[21] = abs(mipResidual);
	
	distHits[22] = particleType;
	distHits[23] = cosineOfD;
	distHits[24] = minDistToConsPsiRefLine;
	
	distHits[25] = abs(trackMomentum);
	distHits[26] = currentTrack->getStTrack()->geometry()->helix().h();
	distHits[27] = mVertexPos.z();

#ifdef myRICH_WITH_NTUPLE
	distHits[0] = innerRingPoint.x();
	distHits[1] = innerRingPoint.y();
	distHits[2] = outerRingPoint.x();
	distHits[3] = outerRingPoint.y();
	distHits[4] = psi;
	distHits[5] = abs(lengthOfD);
	double sigmaOfD = .96;
	double meanOfD  = .65;
	// OLD labels
	    bin = 2;
	else if(trackDipAngle>0 && trackDipAngle<5)
	    bin = 3;
	else if(trackDipAngle>5 && trackDipAngle<10)
	    bin = 4;
	else if(trackDipAngle>10)
	    bin = 5;
	if( normalizedD<-1. || normalizedD>2. ) continue;
	if(currentTrack->getStTrack()->geometry()->helix().h() > 0)
	    charge = 1; // positive
	else
	    charge =0; // negative
	// First Attempt
	meanOfD = meanD[bin][charge];
	sigmaOfD = sigmaD[bin][charge];

// 	int charge, bin;
	// For helix h>0
	    (*hitIter)->setBit(eAssignedToRingPi);
// 	    bin = 0;
// 	else if(trackDipAngle>-10 && trackDipAngle<-5)
// 	    bin = 1;
	    if( (*hitIter)->isSet(eAssignedToRingPi) )
		(*hitIter)->setBit(eMultiplyAssignedToRing);
// 	else if(trackDipAngle>5 && trackDipAngle<10)
// 	    bin = 4;
// 	else if(trackDipAngle>10)
// 	    bin = 5;


	
	// constant area
	//
	    if( fabs(sigma)<1 ) (*hitIter)->setBit(e1SigmaPi);
	    if( fabs(sigma)<2 ) {pi2++;(*hitIter)->setBit(e2SigmaPi);}
	    (*hitIter)->setBit(eAssignedToRingK);
// 	    if( (*hitIter)->isSet(eAssignedToRingPi) )
// 		(*hitIter)->setBit(eMultiplyAssignedToRing);

// 	    if( fabs(sigma)<1 ) (*hitIter)->setBit(e1SigmaPi);
// 	    if( fabs(sigma)<2 ) {pi2++;(*hitIter)->setBit(e2SigmaPi);}
	    if( normalizedD>=0 && normalizedD<=1 )
		(*hitIter)->setBit(eInAreaPi);
		
		}
	    if( (*hitIter)->isSet(eAssignedToRingK) )
		(*hitIter)->setBit(eMultiplyAssignedToRing);
		addHit((*hitIter),normalizedD,sigma,meanAngle,radPath,quartzPath,particle);
	    }
	    
	    if( fabs(sigma)<1 ) (*hitIter)->setBit(e1SigmaK);
	    if( fabs(sigma)<2 ) {ka2++;(*hitIter)->setBit(e2SigmaK);}
	    (*hitIter)->setBit(eAssignedToRingp);
// 	    if( (*hitIter)->isSet(eAssignedToRingK) )
// 		(*hitIter)->setBit(eMultiplyAssignedToRing);

// 	    if( fabs(sigma)<1 ) (*hitIter)->setBit(e1SigmaK);
// 	    if( fabs(sigma)<2 ) {ka2++;(*hitIter)->setBit(e2SigmaK);}
	    if( normalizedD>=0 && normalizedD<=1 )
		(*hitIter)->setBit(eInAreaK);
	    
		}
	    if( (*hitIter)->isSet(eAssignedToRingp) )
		(*hitIter)->setBit(eMultiplyAssignedToRing);
		addHit((*hitIter),normalizedD,sigma,meanAngle,radPath,quartzPath,particle);
	    }
	
	    if( fabs(sigma)<1 ) (*hitIter)->setBit(e1Sigmap);
	    if( fabs(sigma)<2 ) (*hitIter)->setBit(e2Sigmap);
//     cout << "pi= " << pi << " pi2s= " << pi2 << endl;
//     cout << "ka= " << ka << " ka2s= " << ka2 << endl;
//     cout << "pr= " << pr << endl;
	    (*hitIter)->setBit(eAssignedToRingp);
		    (*hitIter)->setBit(eInMultipleAreap);
// 	    if( fabs(sigma)<1 ) (*hitIter)->setBit(e1Sigmap);
// 	    if( fabs(sigma)<2 ) (*hitIter)->setBit(e2Sigmap);
	    if( normalizedD>=0 && normalizedD<=1 )
		(*hitIter)->setBit(eInAreap);
	    
		}
		else {
		    (*hitIter)->setBit(eInAreap);
		addHit((*hitIter),normalizedD,sigma,meanAngle,radPath,quartzPath,particle);
	    
	    }

	    ringCalculator->
		getRing(eInnerRing)->
		getTrack()->
		addHit((*hitIter),normalizedD,sigma,psi,radPath,quartzPath,particle);
	    break;
	    
	default:
	    cout << "Unknown particle type:" << endl;

	    break;
	} // switch
		getRing(eInnerRing)->
//      cout << "pi= " << pi << " pi2s= " << pi2 << endl;
//      cout << "ka= " << ka << " ka2s= " << ka2 << endl;
//      cout << "pr= " << pr << endl;
		getTrack()->
		addHit((*hitIter),normalizedD,sigma,psi,radPath,quartzPath,particle);
	}


// void StRichPIDMaker::hitFilter(const StSPtrVecRichHit* richHits,
// 			       StRichRingCalculator* ringCalculator) {

//     if(!richHits) {
// 	cout << "StRichPIDMaker::hitFilter()\n";
// 	cout << "\tERROR no hits..." << endl;
// 	return;
//     }
    
//
//     //
//     // which ring am I dealing with
//     //
//     StParticleDefinition* particle = ringCalculator->getRing(eInnerRing)->getParticleType();
//     int particleType = particle->pdgEncoding();
// //     cout << "particleType " << particleType << endl;

//     int pi=0;
//     int pi2=0;
//     int ka=0;
//     int ka2=0;
//     int pr=0;
// //     cout << "mom= "
// // 	 << ringCalculator->getRing(eInnerRing)->getTrack()->getMomentum().mag()
// // 	 << "\t richHits->size() " << richHits->size() << endl;
//     StSPtrVecRichHitConstIterator hitIter;
//     for (hitIter = richHits->begin();
// 	 hitIter != richHits->end(); hitIter++) {

// 	StRichHit* tempHit = (*hitIter);
	
// 	if (! this->checkHit(tempHit) ) continue;

// 	ringCalculator->clear();  
// 	StThreeVectorF hit = tempHit->local();
	
// 	innerDistance = ringCalculator->getInnerDistance(hit,innerAngle);
// 	outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
// 	meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
// 	ringWidth     = ringCalculator->getRingWidth();
// 	double quartzPath    = ringCalculator->getMeanPathInQuartz();
// 	double radPath       = ringCalculator->getMeanPathInRadiator();
     
// 	if (!ringWidth) continue;
	    
	  
// 	double dist  = ((outerDistance/ringWidth > 1 &&
// 			 (innerDistance/ringWidth < outerDistance/ringWidth )) ? 
// 			(-1.0):(1.0))*innerDistance/ringWidth;
// 	double sigma = this->getHitSigma(dist);
	  

// 	//
// 	// Only consider those within a normalized dist of 5

// 	if (fabs(dist)<2) {//dist
	    
// 	    switch(particleType) {//switch
// 	    case -211:  // pion
// 		pi++;
// 		tempHit->setBit(eAssignedToRingPi);
		
// 		if( fabs(sigma)<1 ) tempHit->setBit(e1SigmaPi);
// 		if( fabs(sigma)<2 ) {pi2++;tempHit->setBit(e2SigmaPi);}
// 		if( dist>=0 && dist<=1 )
// 		    tempHit->setBit(eInAreaPi);
		
// 		ringCalculator->
// 		    getRing(eInnerRing)->
// 		    getTrack()->
// 		    addHit(tempHit,dist,sigma,meanAngle,radPath,quartzPath,particle);
// 		break;
	    
// 	    case -321:  // kaon
// 		ka++;
// 		tempHit->setBit(eAssignedToRingK);
		
// 		if( fabs(sigma)<1 ) tempHit->setBit(e1SigmaK);
// 		if( fabs(sigma)<2 ) {ka2++;tempHit->setBit(e2SigmaK);}
// 		if( dist>=0 && dist<=1 )
// 		    tempHit->setBit(eInAreaK);

// 		ringCalculator->
// 		    getRing(eInnerRing)->
// 		    getTrack()->
// 		    addHit(tempHit,dist,sigma,meanAngle,radPath,quartzPath,particle);
// 		break;
	    
// 	    case -2212:  //proton
// 		pr++;
// 		tempHit->setBit(eAssignedToRingp);
		
// 		if( fabs(sigma)<1 ) tempHit->setBit(e1Sigmap);
// 		if( fabs(sigma)<2 ) tempHit->setBit(e2Sigmap);
// 		if( dist>=0 && dist<=1 )
// 		    tempHit->setBit(eInAreap);

// 		ringCalculator->
// 		    getRing(eInnerRing)->
// 		    getTrack()->
// 		    addHit(tempHit,dist,sigma,meanAngle,radPath,quartzPath,particle);
	
// 		break;
		
// 	    default:
// 	    cout << "Unknown particle type:" << endl;
// 	    continue;
// 	    break;
// 	    } // switch

// 	}// dist
	
//     }     // loop over hits
// //     cout << "pi= " << pi << " pi2s= " << pi2 << endl;
// //     cout << "ka= " << ka << " ka2s= " << ka2 << endl;
// //     cout << "pr= " << pr << endl;
// }


	else {
	    cout << "StRichPIDMaker::hitfilter()\n";
	    cout << "\tWARNING:\n";
	    cout << "Unknown particle type: ("
		 << particleType << ")" << endl;
	}

    
    }     // loop over hits
    
    //
    // Report from the counter for the rings of this particle
    //
}


void StRichPIDMaker::hitFilter(StRichRingCalculator* ringCalculator,
			          StThreeVectorF& hit, float& angle, float& dist) {
  
  // calculate distance from inner,mean, outer rings

  ringCalculator->clear();  
  angle = -999;
  dist  = -999;
  innerDistance = ringCalculator->getInnerDistance(hit,innerAngle);
  outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
  meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
  ringWidth     = ringCalculator->getRingWidth();
  if (ringWidth==0) {return;}  
  angle = meanAngle;
  dist  = ((outerDistance/ringWidth > 1 && (innerDistance/ringWidth < outerDistance/ringWidth )) ? 
	   (-1.0):(1.0))*innerDistance/ringWidth;
  return;
}

void StRichPIDMaker::tagMips(StEvent* tempEvent, StSPtrVecRichHit* hits) {
  
    cout << "StRichPIDMaker::tagMips()  ---> Tagging Mips using global tracks ..." << endl;

    StSPtrVecTrackNode& theTrackNodes = tempEvent->trackNodes();
    for (size_t nodeIndex=0;nodeIndex<theTrackNodes.size();nodeIndex++) {
	for (size_t trackIndex=0;
	     trackIndex<theTrackNodes[nodeIndex]->entries(global);
	     trackIndex++) {
	    StTrack* track =
		theTrackNodes[nodeIndex]->track(global,trackIndex);
	    if (track  &&
		track->flag()>=0 &&
		track->fitTraits().numberOfFitPoints(kTpcId) >= 10 ) {

		StRichTrack* tempTrack = new StRichTrack(track,mMagField);
		/// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! path length hard coded!!!!!!!!!
    
		    for (hitIter = hits->begin(); hitIter != hits->end(); ++hitIter) {
			testThisResidual = ((*hitIter)->local() - trackPredictor);      
			if ( (testThisResidual.perp() < smallestResidual) &&
			     ((*hitIter)->charge() > adcCut) ) {
			    smallestResidual = testThisResidual.perp();   
			    mAssociatedMIP   = *hitIter;
			}
		    }
		    // mAssociatedMIP->setMipFlag();
		    mAssociatedMIP = 0;
		}
	cout << "\tWARNING! Cannot get B field from event->summary().  Use B= " << mMagField << endl;
    }
}

bool StRichPIDMaker::checkEvent(StEvent* event) {

    //
    // right now just checks if event and vertex exist
    // later will implement cut on vertex, nparticles, etc...
    //
    if (!event) {
	cout << "StRichPIDMaker::checkEvent()\n";
	cout << "\tERROR: Bad Event. Skipping..." << endl;
	return false;
	}
    
    if(!event->primaryVertex()) {
	cout << "StRichPIDMaker::checkEvent()\n";
	cout << "\tERROR: No Vertex. Skipping..." << endl;
	return false;
    }

    mVertexPos = event->primaryVertex()->position();
    mEventN = event->id();

    if( fabs(mVertexPos.z()) > mVertexWindow) {
	cout << "StRichPIDMaker::checkEvent()\n";
	cout << "\tVertex Cut not satisfied (";
	cout << (mVertexWindow/centimeter) << " cm)\n";
	cout << "\tz=" << mVertexPos.z() << endl;
	return false;
    }
//     cout << "StRichPIDMaker::checkEvent() =>\n";
//     cout << "event vertex z = " << mVertexPos.z() << endl;
  if ( fabs(extrapolatedPosition.x()) < (mGeometryDb->radiatorDimension().x() - mPadPlaneCut) &&
       fabs(extrapolatedPosition.y()) < (mGeometryDb->radiatorDimension().y() - mPadPlaneCut) &&
       fabs(extrapolatedPosition.x()) > (mPadPlaneCut) &&
       fabs(extrapolatedPosition.y()) > (mPadPlaneCut) &&
       fabs(impactPoint.x()) < (mGeometryDb->radiatorDimension().x() - mRadiatorCut) &&
       fabs(impactPoint.y()) < (mGeometryDb->radiatorDimension().y() - mRadiatorCut) &&
       fabs(impactPoint.x()) > (mRadiatorCut) &&
       fabs(impactPoint.y()) > (mRadiatorCut) &&
       (track->getPathLength()>0 && track->getPathLength()<mPathCut) &&
       track->getLastHit().perp()>mLastHitCut) {
    return true;
  }

  return false;

    if (track &&
	track->flag()>=0 &&
	track->geometry() && 
	(track->geometry()->helix().distance(mVertexPos)<mDcaCut) &&
	(track->fitTraits().numberOfFitPoints(kTpcId) >= mFitPointsCut) &&
	(fabs(track->geometry()->momentum().pseudoRapidity()) < mEtaCut) &&
	(track->geometry()->momentum().perp() > mPtCut)) {
  
	return true;
    }
  
    return false;
}

bool StRichPIDMaker::checkTrack(StRichTrack* track) {

    StThreeVectorF extrapolatedPosition = track->getProjectedMIP();
    StThreeVectorF impactPoint          = track->getImpactPoint();

    //
    // values for the pad plane and radiator dimension should
    // be kept as data members to avoid the dereferencing
    //
    if ( fabs(extrapolatedPosition.x()) < (mGeometryDb->padPlaneDimension().x() - mPadPlaneCut) &&
	 fabs(extrapolatedPosition.y()) < (mGeometryDb->padPlaneDimension().y() - mPadPlaneCut) &&
	 fabs(extrapolatedPosition.x()) > (mPadPlaneCut) &&
	 fabs(extrapolatedPosition.y()) > (mPadPlaneCut) &&
	 fabs(impactPoint.x()) < (mGeometryDb->radiatorDimension().x() - mRadiatorCut) &&
	 fabs(impactPoint.y()) < (mGeometryDb->radiatorDimension().y() - mRadiatorCut) &&
	 fabs(impactPoint.x()) > (mRadiatorCut) &&
	 fabs(impactPoint.y()) > (mRadiatorCut) &&
	 (track->getPathLength()>0 && track->getPathLength()<mPathCut) &&
    
    return false;

    if (!track || !part) return;

    if (!track->getPidTrait()) {
      cout << "StRichPIDMaker::fillPIDTraits()\n";
      cout << "\tERROR no PID trait\n";
      cout << "\tSkip Track" << endl;
      return;
  }

  double saturatedArea  = 0;
  UShort_t totalHits    = 0;
  UShort_t constantHits = 0;
 
  if (part==pion)        saturatedArea = mPionSaturatedArea;
  else if (part==kaon)   saturatedArea = mKaonSaturatedArea;
  else if (part==proton) saturatedArea = mProtonSaturatedArea;
  else {
    
    //
    // check for correct particle types
  }

  if (track->fastEnough(part) && track->isGood(part)) {

      StRichPid* pid = new StRichPid();
      pid->setRingType(part);
      double totalArea=0;
      pid->setTruncatedArea(ringCalc->
			    calculateConstantArea(saturatedArea,mDoGapCorrection,totalArea));
      pid->setTotalArea(totalArea);
      pid->setTotalAzimuth(ringCalc->getTotalAngle());
      pid->setTruncatedAzimuth(ringCalc->getConstantAreaAngle());

      totalHits    = 0;
      constantHits = 0;
      vector<StRichRingHit*> hits = track->getRingHits(part);
      cout << "StRichPIDMaker::fillPidTraits()";
    //cout << "StRichPIDMaker::fillPIDTraits()" << endl;
      // Add the stripped StRichHit to the StRichPid
    //
      for (size_t i=0;i<hits.size();i++) {
	pid->setTruncatedAzimuth(ringCalc->getTotalConstantAngleOnActivePadPlane());

	//
	      totalHits++;

	      if (fabs(hits[i]->getAngle())>ringCalc->getConstantAreaAngle()) {
		  constantHits++;
		  if(part==pion)
		      hits[i]->getHit()->setBit(eInConstantAreaPi);
		  if(part==kaon)
		      hits[i]->getHit()->setBit(eInConstantAreaK);
		  if(part==proton)
		      hits[i]->getHit()->setBit(eInConstantAreap);
	      }  
	      if(part==kaon)
      }
      //cout << "tot hits = " << totalHits << "const hits = ";
      //cout << constantHits << endl << endl << endl;
      
      //if (part==kaon && constantHits>2 && fabs(mVertexPos.z())<30) {
//        if (fabs(mVertexPos.z())<30) {
//  	  mPrintThisEvent=true;
//        }

      StThreeVectorD residual(-999,-999,-999);
      if (track->getAssociatedMIP()) {
			theCurrentHit->setBit(eInAreap);
		    
	    }  
// 		     << theCurrentHit->isSet(eInMultipleCAnglep)
      
      pid->setTotalHits(totalHits);
      pid->setTruncatedHits(constantHits);
      pid->setMipResidual(residual);

      //
      // assign the pid to the StRichTrack
      //
       track->getPidTrait()->addPid(pid);
  }

// 	cout << "sig1hit= " << sig1 << " sig2hit= " << sig2 << endl;
	StThreeVectorD residual;
	if (track->getAssociatedMIP()) {
      }
	    residual.setX(track->getProjectedMIP().x()-track->getAssociatedMIP()->local().x());
	    residual.setY(track->getProjectedMIP().y()-track->getAssociatedMIP()->local().y());
	    residual.setZ(track->getProjectedMIP().z()-track->getAssociatedMIP()->local().z());
	}
	//  	  mPrintThisEvent=true;
	pid->setTotalHits(totalHits);
	pid->setTruncatedHits(constantHits);

	pid->setTotalHits(totalHitsInArea);
	pid->setTruncatedHits(hitsInConstantArea);
 
	    cout << "\treturning" << endl;
	track->getPidTrait()->addPid(pid);
    }
		//cout << "\tCANNOT HAVE MORE HITS!!!!\n";
		//cout << "\tAnother Track has touched this..." << endl;
		break;
	    }
	    
	    pid->setTruncatedHits(hitsInConstantArea);
  mPadMonitor->clearTracks();
  mPadMonitor->drawZVertex(mVertexPos.z(),mNumberOfPrimaries,mRichTracks);
  mPadMonitor->drawEventInfo(rEvent->runId(),rEvent->id());
  mPadMonitor->drawFileName(fileName);

  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
    mPadMonitor->addTrack(mListOfStRichTracks[trackIndex]);
  }
  
  mPadMonitor->drawRings();
  mPadMonitor->hiLiteHits(eInAreaPi);
  //mPadMonitor->hiLiteHits();
  mPadMonitor->hiLiteHits(e2SigmaPi);
void StRichPIDMaker::printCutParameters(ostream& os) const
    os << "Positives" << endl;
    os << meanD[0][1] << "\t" << sigmaD[0][1] << endl;
    os << meanD[1][1] << "\t" << sigmaD[1][1] << endl;
    os << meanD[2][1] << "\t" << sigmaD[2][1] << endl;
    os << meanD[3][1] << "\t" << sigmaD[3][1] << endl;
    os << meanD[4][1] << "\t" << sigmaD[4][1] << endl;
    os << meanD[5][1] << "\t" << sigmaD[5][1] << endl;
    os << "----------------------------------------------" << endl;
void StRichPIDMaker::setAdcCut(int cut) {mAdcCut = cut;}
    os << "----------------------------------------------\n" << endl;
    os << "++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    os << "Convergence Precision:" << endl;
void StRichPIDMaker::setLastHitCut(float cut) {mLastHitCut = cut;}
void StRichPIDMaker::setDcaCut(float cut) {mDcaCut = cut;}
void StRichPIDMaker::setPtCut(float cut)  {mPtCut = cut;}
void StRichPIDMaker::setEtaCut(float cut) {mEtaCut = cut;}
void StRichPIDMaker::setFitPointsCut(int cut) {mFitPointsCut = cut;}
//     os << meanD[0][0] << "\t" << sigmaD[0][0] << endl;
void StRichPIDMaker::setPadPlaneCut(float cut) {mPadPlaneCut = cut;}
void StRichPIDMaker::setRadiatorCut(float cut) {mRadiatorCut = cut;}
//     os << meanD[3][0] << "\t" << sigmaD[3][0] << endl;
//     os << meanD[4][0] << "\t" << sigmaD[4][0] << endl;
//     os << meanD[5][0] << "\t" << sigmaD[5][0] << endl;

//     os << "Positives" << endl;
//     os << meanD[0][1] << "\t" << sigmaD[0][1] << endl;
//     os << meanD[1][1] << "\t" << sigmaD[1][1] << endl;
//     os << meanD[2][1] << "\t" << sigmaD[2][1] << endl;
//     os << meanD[3][1] << "\t" << sigmaD[3][1] << endl;
//     os << meanD[4][1] << "\t" << sigmaD[4][1] << endl;
//     os << meanD[5][1] << "\t" << sigmaD[5][1] << endl;
//     os << "----------------------------------------------" << endl;
//     os << "----------------------------------------------\n" << endl;

}

// Event Level
void StRichPIDMaker::setVertexWindow(float window) {
    cout << "StRichPIDMaker::setVertexWindow() " << window << endl;
    mVertexWindow = window;
}

// Hit Level
void StRichPIDMaker::setAdcCut(int cut) {mAdcCut = cut;}

// Track Level
void StRichPIDMaker::setLastHitCut(float cut)    {mLastHitCut = cut;}
void StRichPIDMaker::setDcaCut(float cut)        {mDcaCut = cut;}
void StRichPIDMaker::setPtCut(float cut)         {mPtCut = cut;}
void StRichPIDMaker::setEtaCut(float cut)        {mEtaCut = cut;}
void StRichPIDMaker::setFitPointsCut(int cut)    {mFitPointsCut = cut;}
void StRichPIDMaker::setPathLengthCut(float cut) {mPathCut = cut;}
void StRichPIDMaker::setPadPlaneCut(float cut)   {mPadPlaneCut = cut;}
void StRichPIDMaker::setRadiatorCut(float cut)   {mRadiatorCut = cut;}
float StRichPIDMaker::getHitSigma(float hitDist) {
  float sigma = 0.4;
  float mean = 0.45;
  return fabs(hitDist-mean)/sigma;
}


void StRichPIDMaker::DefineSaveDirectory(char* directory) {
  mySaveDirectory=directory;
}


void StRichPIDMaker::setFileName(char * name){
    fileName = name;
}


void StRichPIDMaker::setWavelengthRange(double shortest , double longest) {
  mShortWave = shortest;
  mLongWave  = longest;
  mMaterialDb = StRichMaterialsDb::getDb();
  mMaterialDb->setWavelengthRange(mShortWave,mLongWave);
}


void StRichPIDMaker::fillEvtNtuple(StEvent* event, int pri, 
				   int npri, const StSPtrVecRichHit* hits,
				   const StSPtrVecRichPixel* pixels ) {

    //StThreeVectorF VertexPos(-999,-999,-999);
    //if (event->primaryVertex()) {VertexPos = event->primaryVertex()->position();}  

  float array[9];  
	resid<0.3*centimeter &&  track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId)>40 &&
	track->getLastHit().perp()>160.0*centimeter) {
  array[1] = mNumberOf1GeV;
  array[2] = pri;
  array[3] = npri;
  array[4] = mVertexPos.x();
  array[5] = mVertexPos.y();
  array[6] = mVertexPos.z();
  array[7] = 0;
  if (hits) array[7] = hits->size(); 
  array[8]=0;
  if (pixels) array[8] = pixels->size();
  evtNtup->Fill(array);
}



void StRichPIDMaker::fillOverLapHist(const StSPtrVecRichHit* richHits) {


  StThreeVectorF out,in;
  StRichTrack* track=0;
  
  for (size_t i=0; i<mListOfStRichTracks.size(); i++) {
    
    track = mListOfStRichTracks[i];
    float p     = track->getMomentum().mag();
    float theta = track->getTheta()/degree;
    float resid = 999;
    float kaonTag=0;
    float pionTag=0;
    
    if (track->getAssociatedMIP()) resid = (track->getAssociatedMIP()->local()- track->getProjectedMIP()).perp();
    
    if (track->getStTrack()  && track->fastEnough(kaon) && track->isGood(kaon) && track->isGood(pion) &&
	resid<0.3*centimeter) {
      
      StRichRingCalculator* pionCalculator = new StRichRingCalculator(track,pion);
      StRichRingCalculator* kaonCalculator = new StRichRingCalculator(track,kaon);
      
      // calculate distance from inner,mean, outer rings
      if (richHits) {
	
	float adcCut = 300;

	StSPtrVecRichHitConstIterator hitIter;
	for (hitIter = richHits->begin(); hitIter != richHits->end(); hitIter++) {
	  
	  if ((*hitIter)->charge()<adcCut) {
	    
	    pionCalculator->clear();  
	    kaonCalculator->clear();  
	    
	    StThreeVectorF hit = (*hitIter)->local();

	    pionTag=0;
	    kaonTag=0;

	    vector<StRichRingHit*> pihits = track->getRingHits(pion);
	    for (size_t i=0; i<pihits.size() && pionTag==0; i++) {
	      if ((*hitIter)==pihits[i]->getHit() &&
		  pihits[i]->getDist()>0          &&
		  pihits[i]->getDist()<1)
		  pionTag=1;
	    vector<StRichRingHit*> kahits = track->getRingHits(kaon);
	    for (size_t i=0; i<kahits.size() && kaonTag==0; i++) {
	      if ((*hitIter)==kahits[i]->getHit() &&
		  kahits[i]->getDist()>0          &&
		  kahits[i]->getDist()<1)
		  kaonTag=1;
	    }
	    
	    innerDistance = kaonCalculator->getInnerDistance(hit,innerAngle);
	    outerDistance = pionCalculator->getOuterDistance(hit,outerAngle);
	    
	    in  = kaonCalculator->getInnerRingPoint();
	    out = pionCalculator->getOuterRingPoint();
	    
	    ringWidth     = (in  - out).perp();
    
    if (mListOfStRichTracks[i]->getPidTrait() &&
	mListOfStRichTracks[i]->getPidTrait()->getPid(pion)) {
	constAngle = mListOfStRichTracks[i]->getPidTrait()->getPid(pion)->getTruncatedAzimuth();
	goodHits =  mListOfStRichTracks[i]->getPidTrait()->getPid(pion)->getTruncatedHits();


  
  float psi1save = -999;
  float psi2save = 999;
  float psi1,psi2;
  int counts=mListOfStRichTracks.size();
  int goodHits=0;
  float x1,y1,x2,y2;

  for (int i=0;i<counts;i++) {
    float constAngle=-999;
    
    psi1save = -999;
    psi2save = 999;
        
    StRichPid* pionPid = 0;
    StRichPidTraits* pidTrait=0;
    if (mListOfStRichTracks[i]->getPidTrait()) {
      pidTrait = mListOfStRichTracks[i]->getPidTrait();
      
      //
      // need to get the last entry in the list
      //
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
	if (thepids[pidcounter]->getRingType()==pion)  {
	  pionPid   = thepids[pidcounter]; 
	  constAngle = pionPid->getTruncatedAzimuth();
	  goodHits = pionPid->getTruncatedHits();
	}
      }
    }
    
     

    vector<StRichRingHit*> hits = mListOfStRichTracks[i]->getRingHits(pion);
    
    if (goodHits>1) {
      
      for (size_t j=0; j<hits.size(); j++) {
	for (size_t k=j+1; k<hits.size(); k++) {
	  
	  psi1=hits[j]->getAngle();
	  psi2=hits[k]->getAngle();
	  
	  if (fabs(psi1)>constAngle      &&
	      fabs(psi2)>constAngle      &&
	      (hits[j]->getNSigma() < 2) &&
	      (hits[k]->getNSigma() < 2) ) {		 
	    
	      if (psi1<0) psi1 = psi1 + 2.0*M_PI;
	      if (psi2<0) psi2 = psi2 + 2.0*M_PI;
	      
	      if ( fabs(psi1-psi2) < fabs(psi1save-psi2save) ) {
		  psi1save=psi1;
		  psi2save=psi2;
		  x1 = hits[j]->getHit()->local().x();
		  y1 = hits[j]->getHit()->local().y();
		  x2 = hits[k]->getHit()->local().x();
		  y2 = hits[k]->getHit()->local().y();
	      }
	  } 
	}
      }
      
      if (psi1save != -999 && psi2save != 999) { 
	 	      	      
	  closeHitNtup->Fill(mListOfStRichTracks[i]->getMomentum().mag(),
			     mListOfStRichTracks[i]->getTheta()/degree,
			     goodHits,constAngle/degree,
			     psi1save/degree,psi2save/degree,
			     x1,y1,x2,y2);
      }
    }
  }
  
  int counter=0;
  int clusternumber,npads,clusterFromTrack,found,tpchits;
  double adcsum,x,y,p,maxAmp,theta,quartz,rad,ang;
  if (clusters) {
    if (hits) {
      StSPtrVecRichHitConstIterator hitIter;
      for (hitIter=hits->begin();
	   hitIter != hits->end();
	   hitIter++) {
	
	counter++;
	quartz=0;
	rad=0;
	clusternumber  = (*hitIter)->clusterNumber();
	npads          = (*clusters)[clusternumber]->numberOfPads();
	adcsum         = (*clusters)[clusternumber]->amplitudeSum();
	maxAmp         = (*hitIter)->maxAmplitude();
	x = (*hitIter)->local().x();
	y = (*hitIter)->local().y();
	clusterFromTrack = 0;
	found = 0;
	p=0;
	StThreeVectorF resid(-999,-999,-999);
	tpchits=0;
	StRichTrack* track = 0;
	for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
	  track = mListOfStRichTracks[trackIndex];
	  p = track->getMomentum().mag();
	  theta = track->getTheta()/degree;
	  if (track->getAssociatedMIP()==(*hitIter)) {
	    clusterFromTrack=10;
	    if (track->getStTrack()) { 
	      tpchits = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
	    }
	    if (track->getAssociatedMIP()) { 
	      resid = (track->getAssociatedMIP()->local() - track->getProjectedMIP());
	    }
	  }

	 
	  for (size_t particleIndex=0;
	       particleIndex<mListOfParticles.size() && found==0; 
	       particleIndex++) {
	    vector<StRichRingHit*> hits 
	      = track->getRingHits(mListOfParticles[particleIndex]);	    
	    for (size_t i=0;i<hits.size() && found==0; i++) {
	      if ((*hitIter) == hits[i]->getHit()  && hits[i]->getNSigma()<2 ) {
		quartz = hits[i]->getMeanPathInQuartz();
		rad = hits[i]->getMeanPathInRadiator();
		ang = hits[i]->getAngle()/degree;
		found=1;
	      }
	    }
	  }
	}

	float array[16];
	array[0] = npads;
	array[1] = adcsum;
	array[2] = clusterFromTrack;
	array[3] = x;
	array[4] = y;
	array[5] = found;
	array[6] = p;
	array[7] = resid.perp();
	array[8] = tpchits;
	array[9] = maxAmp;
	array[10] = resid.x();
	array[11] = resid.y();
	array[12] = theta;
	array[13] = quartz;
	array[14] = rad;
	array[15] = ang;
	hitNtup->Fill(array);
	
      }
    }
  }
} 


#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillMcPixelNtuple(const StSPtrVecRichPixel* pixels) {
  
  if (!pixels) return;
  
  // loop over hits
  StRichMCPixel* monteCarloRichPixel = 0;
  for (StSPtrVecRichPixelConstIterator iter = pixels->begin();iter != pixels->end(); ++iter) {
    
    monteCarloRichPixel = dynamic_cast<StRichMCPixel*>(*iter);   
    if (monteCarloRichPixel) {
    Long_t  gid[4]   = {0,0,0,0};
    Float_t gq[4]    = {0,0,0,0};
    Long_t  gproc[4] = {0,0,0,0};
    
    unsigned int n = monteCarloRichPixel->contributions();
    unsigned int limit = (n>4)?(4):(n); 
    for (int i=0;i<limit;i++) {
      gid[i]   = monteCarloRichPixel->getMCInfo()[i]->gid();
      gq[i]    = monteCarloRichPixel->getMCInfo()[i]->charge();
      gproc[i] = monteCarloRichPixel->getMCInfo()[i]->process();
    }
    
    geantPixelNtuple->Fill(monteCarloRichPixel->adc(),monteCarloRichPixel->contributions(),
			   gid[0],gid[1],gid[2],gid[3],gq[0],gq[1],gq[2],gq[3],
			   gproc[0],gproc[1],gproc[2],gproc[3]);
    } 
  }
}
#endif



#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillGeantHitNtuple() {
  
  StRichMCTrack* richMcTrack=0;
  
  float constAngle=-999;
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {
    richMcTrack = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);    
    if (richMcTrack) {
      if (mListOfStRichTracks[trackIndex]->getPidTrait() && 
	  mListOfStRichTracks[trackIndex]->getPidTrait()->getPid(pion)) {
	constAngle = mListOfStRichTracks[trackIndex]->getPidTrait()->getPid(pion)->getTruncatedAzimuth();
      }

      vector<StMcRichHit*> tempHits = richMcTrack->getGeantPhotons();
      float wave1,psi1,z1;
      float wave2,psi2,z2;
      float wave1save,wave2save;
      float psi1save,psi2save;
      float z1save,z2save;
      float x1,y1,x2,y2;

      for (int i=0;i<tempHits.size();i++) {
	for (int j=i+1;j<tempHits.size();j++) {

	  getGeantPhotonInfo(richMcTrack,tempHits[i]->parentTrack(),wave1,psi1,z1);
	  getGeantPhotonInfo(richMcTrack,tempHits[j]->parentTrack(),wave2,psi2,z2);

	  if (fabs(psi1)>constAngle && fabs(psi2)>constAngle)  {
	
	    if (psi1<0) psi1 = psi1 + 2.0*M_PI;
	    if (psi2<0) psi2 = psi2 + 2.0*M_PI;
	    if ( fabs(psi1-psi2) < fabs(psi1save-psi2save)) {
	      psi1save=psi1;
	      psi2save=psi2;
	      wave1save=wave1;
	      wave2save=wave2;
	      z1save = z1;
	      z2save = z2;
	      x1 = tempHits[i]->position().x();
	      y1 = tempHits[i]->position().y();
	      x2 = tempHits[j]->position().x();
	      y2 = tempHits[j]->position().y();
	    }  
	  }
	}
      }
      
      float array[13];
      array[0] = richMcTrack->getMomentum().mag();
      array[1] = richMcTrack->getTheta()/degree;
      array[2] = wave1save/nanometer;
      array[3] = wave2save/nanometer;
      array[4] = psi1save/degree;
      array[5] = psi2save/degree;
      array[6] = z1save;
      array[7] = z2save;
      array[8] = x1;
      array[9] = y1;
      array[10] = x2;
      array[11] = y2;
    double totArea = 0;
      array[12] = constAngle/degree;
      geantCloseHitNtuple->Fill(array);      
    }
  }  
}
#endif

#ifdef myRICH_WITH_MC
      if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
	TPC_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}
					   const StSPtrVecRichHit* richHits) {
  
  if (!mcevent || !richHits || !clusters) return;
      if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
	GEANT_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}
    if(!dsGeant || !dsGeant->GetList()) { return;}
  }

  St_DataSetIter geantDstI(dsGeant);
  St_g2t_track   *g2t_track = (St_g2t_track *) geantDstI("g2t_track"); 
  
  StRichMCTrack*        richMcTrack   = 0;
  StParticleDefinition* geantParticle = 0;
  
  StRichMCHit* monteCarloRichHit      = 0;
  StMcTrack*   theHitsStMcTrack       = 0;
  StMcTrack*   theHitsParentStMcTrack = 0;

  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {
    
    richMcTrack    = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);    
    geantParticle  = richMcTrack->getStMcTrack()->particleDefinition();
 
    if (richMcTrack && geantParticle) {
      
      richMcTrack->useTPCInfo();
      StRichRingCalculator* TPC_RingCalc = new StRichRingCalculator(richMcTrack,geantParticle);
      TPC_RingCalc->calculateArea();
      
      //if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
      //TPC_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}
      
      richMcTrack->useGeantInfo();
      StRichRingCalculator* GEANT_RingCalc = new StRichRingCalculator(richMcTrack,geantParticle);	
      GEANT_RingCalc->calculateArea();

      //if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
      //GEANT_RingCalc->calculateConstantArea(mPionSaturatedArea,true,totArea);}

      richMcTrack->useTPCInfo();  
      
      float defaultValue = -999.0;
      float mcWave,mcPsi,mcZ;
      int   signalPhoton;

      // loop over hits
      StSPtrVecRichHitConstIterator iter;
      for (iter = richHits->begin();iter != richHits->end(); ++iter) {
	
	monteCarloRichHit = dynamic_cast<StRichMCHit*>(*iter); 
	theHitsStMcTrack  = getStMcTrack(monteCarloRichHit, mcevent, g2t_track);
	
	if (monteCarloRichHit && theHitsStMcTrack) {
	
	  if (theHitsStMcTrack->geantId() == 50)  {theHitsParentStMcTrack = theHitsStMcTrack->parent();}
	  else {theHitsParentStMcTrack = theHitsStMcTrack;}
	  
	  // determine if photon's parent == StRichTrack
	  signalPhoton = 0;
	  if (monteCarloRichHit->getMCInfo().process()==ePhoton  && 
	      theHitsParentStMcTrack == richMcTrack->getStMcTrack() 
	      && theHitsStMcTrack->geantId()==50) {
	    signalPhoton=1;    
	  }
	  
	  mcWave = defaultValue; mcPsi = defaultValue; mcZ = defaultValue;
	  getGeantPhotonInfo(richMcTrack,theHitsStMcTrack,mcWave,mcPsi,mcZ);
	  StThreeVectorF geantRichHit = getTheGeantHitOnPadPlane(theHitsStMcTrack,monteCarloRichHit->local()); 
	  
	  Float_t photonArray[52];
	  
	  photonArray[0] = richMcTrack->getStTrack()->geometry()->charge();
	  photonArray[1] = richMcTrack->getMomentum().x();
	  photonArray[2] = richMcTrack->getMomentum().y();
	  photonArray[3] = richMcTrack->getMomentum().z();
	  photonArray[4] = richMcTrack->getImpactPoint().x();
	  
	  photonArray[5] = richMcTrack->getImpactPoint().y();    
	  photonArray[6] = richMcTrack->getGeantImpactPointAtRadiator().x();
	  photonArray[7] = richMcTrack->getGeantImpactPointAtRadiator().y();
	  photonArray[8] = richMcTrack->getProjectedMIP().x();
	  photonArray[9] = richMcTrack->getProjectedMIP().y();
	  
	  StRichMCHit* tempHit = dynamic_cast<StRichMCHit*>(richMcTrack->getAssociatedMIP());
	  if (tempHit) {
	   
	    photonArray[10] = tempHit->getMCInfo().id();   
	    photonArray[11] = tempHit->getMCInfo().process();    
	    photonArray[12] = tempHit->charge();    
	    photonArray[13] = tempHit->local().x();
	    photonArray[14] = tempHit->local().y();
	    photonArray[15] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
	  }
	  
	  tempHit = richMcTrack->getGeantRecoMIP();
	  if (tempHit) { 
	    photonArray[16] = tempHit->getMCInfo().id();
	    photonArray[17] = tempHit->getMCInfo().process();
	    photonArray[18] = tempHit->charge();
	    photonArray[19] = tempHit->local().x();
	    photonArray[20] = tempHit->local().y();
	    photonArray[21] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
	  }
	  tempHit=0;
	  
	  photonArray[22] = richMcTrack->getGeantMIP().x();
	  photonArray[23] = richMcTrack->getGeantMIP().y();
	  photonArray[24] = richMcTrack->getGeantMomentumAtRadiator().x();
	  photonArray[25] = richMcTrack->getGeantMomentumAtRadiator().y();
	  photonArray[26] = richMcTrack->getGeantMomentumAtRadiator().z();
	  
	  photonArray[27] = richMcTrack->getTheta()/degree;
	  photonArray[28] = richMcTrack->getGeantThetaAtRadiator()/degree;
	  photonArray[29] = richMcTrack->getPhi()/degree;
	  photonArray[30] = richMcTrack->getGeantPhiAtRadiator()/degree;
	  photonArray[31] = richMcTrack->getStMcTrack()->geantId();
          
	  photonArray[32] = signalPhoton;
	  photonArray[33] = mcWave;
	  photonArray[34] = mcPsi/degree;
	  photonArray[35] = mcZ;
	  photonArray[36] = monteCarloRichHit->getMCInfo().process();
	  
	  photonArray[37] = monteCarloRichHit->local().x();
	  photonArray[38] = monteCarloRichHit->local().y();
	  photonArray[39] = geantRichHit.x();
	  photonArray[40] = geantRichHit.y();
	  photonArray[41] = geantParticle->mass();
	  
	  bool kWriteTheNtuple = false;

	  richMcTrack->useTPCInfo();
	  if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {
	    photonArray[42] = TPC_RingCalc->getConstantAreaAngle();
	    this->hitFilter(TPC_RingCalc, monteCarloRichHit->local(),photonArray[43],photonArray[44]);
	    this->hitFilter(TPC_RingCalc,geantRichHit,photonArray[45],photonArray[46]);
	    if (fabs(photonArray[44])<5 ||   fabs(photonArray[46])<5) kWriteTheNtuple=true;
	  }    
	  else {
	    photonArray[42]=defaultValue;
	    photonArray[43]=defaultValue;
	    photonArray[44]=defaultValue;
	    photonArray[45]=defaultValue;
	    photonArray[46]=defaultValue;
	  }
	  
	  
	
	  richMcTrack->useGeantInfo();
	  if (richMcTrack->fastEnough(geantParticle) && richMcTrack->isGood(geantParticle)) {   
	    photonArray[47] = GEANT_RingCalc->getConstantAreaAngle();
	    this->hitFilter(GEANT_RingCalc,monteCarloRichHit->local(),photonArray[48],photonArray[49]);
	    this->hitFilter(GEANT_RingCalc,geantRichHit,photonArray[50],photonArray[51]);
	    if (fabs(photonArray[49])<5 ||   fabs(photonArray[51])<5) kWriteTheNtuple=true;
	  }
	  else {
	    photonArray[47]=defaultValue;
	    photonArray[48]=defaultValue;
	    photonArray[49]=defaultValue;
	    photonArray[50]=defaultValue;
	    photonArray[51]=defaultValue;
	  }
	  
	  richMcTrack->useTPCInfo();
	  if (kWriteTheNtuple) geantPhotonNtuple->Fill(photonArray);
	}
      }
      delete TPC_RingCalc;
      delete GEANT_RingCalc;
      TPC_RingCalc   = 0;
      GEANT_RingCalc = 0;
    }
  }
}
#endif


#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillMcTrackNtuple(const StSPtrVecRichCluster* clusters) {

  for (size_t trackIndex=0;trackIndex<mListOfStRichTracks.size();trackIndex++) {    
    StRichMCTrack* track = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);
    if (!track) {
      cout << "trying to send a StTrack to the monte carlo ntuple! " << endl;
      abort();
    }    
    
    StRichPidTraits* pidTrait = track->getPidTrait();
    if (!pidTrait) {
      
      StRichPid* pionPid   = pidTrait->getPid(pion);
      StRichPid* kaonPid   = pidTrait->getPid(kaon);
      StRichPid* protonPid = pidTrait->getPid(proton);

      double PionTotalArea         = 0;
      double PionConstantArea      = 0;
      double PionTotalAreaAngle    = 0;
      double PionConstantAreaAngle = 0;
      int    PionTotalHits         = 0;
      int    PionConstantHits      = 0;
      double PionFactor            = 0;
      
      if (pionPid) {
	PionTotalArea         = pionPid->getTotalArea();
	PionConstantArea      = pionPid->getTruncatedArea();
	PionTotalAreaAngle    = pionPid->getTotalAzimuth();
	PionConstantAreaAngle = pionPid->getTruncatedAzimuth();
	PionTotalHits         = pionPid->getTotalHits();
	PionConstantHits      = pionPid->getTruncatedHits();
      }
      
      
      double KaonTotalArea         = 0;
      double KaonConstantArea      = 0;
      double KaonTotalAreaAngle    = 0;
      double KaonConstantAreaAngle = 0;
      int    KaonTotalHits         = 0;
      int    KaonConstantHits      = 0;
      double KaonFactor            = 0;
      
      if (kaonPid) {
	KaonTotalArea         = kaonPid->getTotalArea();
	KaonConstantArea      = kaonPid->getTruncatedArea();
	KaonTotalAreaAngle    = kaonPid->getTotalAzimuth();
	KaonConstantAreaAngle = kaonPid->getTruncatedAzimuth();
	KaonTotalHits         = kaonPid->getTotalHits();
	KaonConstantHits      = kaonPid->getTruncatedHits();
      }
      
      
      double ProtonTotalArea         = 0;
      double ProtonConstantArea      = 0;
      double ProtonTotalAreaAngle    = 0;
      double ProtonConstantAreaAngle = 0;
      double ProtonFactor            = 0;
      int ProtonTotalHits            = 0;
      int ProtonConstantHits         = 0;

      if (protonPid) {
	ProtonTotalArea         = protonPid->getTotalArea();
	ProtonConstantArea      = protonPid->getTruncatedArea();
	ProtonTotalAreaAngle    = protonPid->getTotalAzimuth();
	ProtonConstantAreaAngle = protonPid->getTruncatedAzimuth();
	ProtonTotalHits         = protonPid->getTotalHits();
	ProtonConstantHits      = protonPid->getTruncatedHits();
      }

      
      

    StThreeVectorF  globalp(track->getStTrack()->geometry()->momentum());
    int   counter=0;
    float defaultValue = -999;
    int const entries=88;
    float trackArray[entries];
    /////////

    trackArray[counter++] = mEventN;                
    trackArray[counter++] = mNumberOfPrimaries;
    trackArray[counter++] = mNegativePrimaries;
    trackArray[counter++] = mVertexPos.z();
    trackArray[counter++] = mRichTracks;  

    trackArray[counter++] = globalp.x();          
    trackArray[counter++] = globalp.y();
    trackArray[counter++] = globalp.z();
    trackArray[counter++] = track->getMomentum().x();
    trackArray[counter++] = track->getMomentum().y();   // ---> 10 

    trackArray[counter++] = track->getMomentum().z();   
    trackArray[counter++] = globalp.pseudoRapidity();  
    trackArray[counter++] = track->getStTrack()->geometry()->charge();

    StRichMCHit* tempHit = dynamic_cast<StRichMCHit*>(track->getAssociatedMIP());
    if (tempHit) {    
      trackArray[counter++] = tempHit->getMCInfo().id();   
      trackArray[counter++] = tempHit->getMCInfo().process();    
      trackArray[counter++] = tempHit->charge();    
      trackArray[counter++] = tempHit->local().x();
      trackArray[counter++] = tempHit->local().y();
      trackArray[counter++] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
    }
    trackArray[counter++] = track->getProjectedMIP().x(); 
  

    trackArray[counter++] = track->getProjectedMIP().y();
  
    tempHit = track->getGeantRecoMIP();
    if (tempHit) { 
      trackArray[counter++] = tempHit->getMCInfo().id();
      trackArray[counter++] = tempHit->getMCInfo().process();
      trackArray[counter++] = tempHit->charge();
      trackArray[counter++] = tempHit->local().x();
      trackArray[counter++] = tempHit->local().y();
      trackArray[counter++] = (*clusters)[tempHit->clusterNumber()]->numberOfPads();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue; 
    }
  

    trackArray[counter++] = track->getImpactPoint().x(); // ----> 10 + 20 == 30  
    trackArray[counter++] = track->getImpactPoint().y();
    trackArray[counter++] = track->getUnCorrectedImpactPoint().x();  
    trackArray[counter++] = track->getUnCorrectedImpactPoint().y();
    trackArray[counter++] = track->getUnCorrectedMomentum().x();        

    trackArray[counter++] = track->getUnCorrectedMomentum().y();

    trackArray[counter++] = track->getUnCorrectedMomentum().z();
    trackArray[counter++] = track->getFirstRow();
    trackArray[counter++] = track->getLastRow();  
    trackArray[counter++] = track->getLastHit().x();
    trackArray[counter++] = track->getLastHit().y();      

    trackArray[counter++] = track->getLastHit().z(); 
    trackArray[counter++] = track->getLastHitDCA();   
    trackArray[counter++] = track->getPathLength();
    trackArray[counter++] = track->getMaxChain();
    trackArray[counter++] = track->getMaxGap();      

    trackArray[counter++] = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
    trackArray[counter++] = track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);
    trackArray[counter++] = mMaterialDb->innerWavelength()/nanometer;
    trackArray[counter++] = mMaterialDb->outerWavelength()/nanometer;
    trackArray[counter++] = track->getGeantMomentumAtRadiator().x();     

    trackArray[counter++] = track->getGeantMomentumAtRadiator().y(); // 30 + 20 = 50
    trackArray[counter++] = track->getGeantMomentumAtRadiator().z();  
    trackArray[counter++] = track->getGeantImpactPointAtRadiator().x();
    trackArray[counter++] = track->getGeantImpactPointAtRadiator().y();  
    trackArray[counter++] = track->getGeantMIP().x();   

    trackArray[counter++] = track->getGeantMIP().y(); 
    if (track->getStMcTrack() && track->getStMcTrack()->stopVertex()) {
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->position().x();
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->position().y();
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->position().z();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
    }
    trackArray[counter++] = track->getGeantPhotons().size();
    trackArray[counter++] = track->getRecoGeantPhotons().size(); 

    trackArray[counter++] = track->getNumberOfGeantHitsInRadiator(); // 50 + 10 = 60
    trackArray[counter++] = track->getCommonTpcHits(); 
    if (track->getStMcTrack()) {
      trackArray[counter++] = track->getStMcTrack()->momentum().x();
      trackArray[counter++] = track->getStMcTrack()->momentum().y();
      trackArray[counter++] = track->getStMcTrack()->momentum().z();
    }
    else {
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;
      trackArray[counter++] = defaultValue;    
    }
 
     
    if (track->getStMcTrack()) { trackArray[counter++] = track->getStMcTrack()->geantId();}
    else { trackArray[counter++] = defaultValue;}
  
    if (track->getStMcTrack() && track->getStMcTrack()->stopVertex()) {
      trackArray[counter++] = track->getStMcTrack()->stopVertex()->geantProcess();}
    else { trackArray[counter++] = defaultValue;}

    trackArray[counter++] = track->getNumberOfPartners();
    trackArray[counter++] = PionFactor;
    trackArray[counter++] = PionTotalArea;  

    trackArray[counter++] = PionConstantArea;  // 60 + 10 = 70 
    trackArray[counter++] = PionTotalAreaAngle;
    trackArray[counter++] = PionConstantAreaAngle;
    trackArray[counter++] = PionTotalHits;  
    trackArray[counter++] = PionConstantHits; 

    trackArray[counter++] = KaonFactor;
    trackArray[counter++] = KaonTotalArea;  
      StRichPid* pionPid = pidTrait->getPid(pion);
      StRichPid* kaonPid = pidTrait->getPid(kaon);
      StRichPid* protonPid = pidTrait->getPid(proton);
    
    if (counter != entries) {cout << "StRichPIDMaker::fillMcPidNtuple  wrong counter. abort." << endl; abort();}
    geantTrackNtuple->Fill(trackArray);
    }
  } 
}
#endif

void 
StRichPIDMaker::fillPIDNtuple() {

  util = new TpcHitVecUtilities();
  
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {

    StRichTrack* track = mListOfStRichTracks[trackIndex];
    StRichPidTraits* pidTrait = track->getPidTrait();

    if (pidTrait) {
    
      StRichPid* pionPid = 0;
      StRichPid* kaonPid = 0;
      StRichPid* protonPid = 0;

      //
      // need to get the last entry in the list
      //
      StSPtrVecRichPid thepids = pidTrait->getAllPids();
      for (size_t pidcounter=0;pidcounter<thepids.size();pidcounter++) {
	if (thepids[pidcounter]->getRingType()==pion)   pionPid   = thepids[pidcounter]; 
	if (thepids[pidcounter]->getRingType()==kaon)   kaonPid   = thepids[pidcounter]; 
	if (thepids[pidcounter]->getRingType()==proton) protonPid = thepids[pidcounter]; 
      }
	  if (track->getStTrack() && 
	      fabs(pionHits[i]->getAngle())>PionConstantAreaAngle &&
	      track->getLastHit().perp()>160.0*centimeter) {

      util->clear();
      util->setTrack(track->getStTrack());
      util->findHits();
      util->sortTpcHitVecZ();
      
      double posZ = util->numberOfHitsInZTrack(0,200);
      double negZ = util->numberOfHitsInZTrack(-200,0);
      
      double PionTotalArea         = 0;
      double PionConstantArea      = 0;
      double PionTotalAreaAngle    = 0;
      double PionConstantAreaAngle = 0;
      int    PionTotalHits         = 0;
      int    PionConstantHits      = 0;
      double PionFactor            = 0;
	    if (fabs(mVertexPos.z())<30*centimeter) {
	      //cout << "30:: vertex = " << mVertexPosition.z() << endl;
	      pionResid->Fill(p,resid,dist);
	      pionResid_x->Fill(p,xresid,dist);
	      pionResid_y->Fill(p,yresid,dist);

	      if (p>1) {
		pionTheta->Fill(tan_theta,resid,dist);
		pionTheta_x->Fill(tan_thetax,xresid,dist);
		pionTheta_y->Fill(tan_thetay,yresid,dist);
		
		if (resid<0.3) {
		  pionPsi->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
		}
	      } 
	    }

	    if (fabs(mVertexPos.z())>30*centimeter  &&
		fabs(mVertexPos.z())<60*centimeter) {
		//cout << "30  60:: vertex = " << mVertexPosition.z() << endl;
		pionResidb->Fill(p,resid,dist);
		pionResid_xb->Fill(p,xresid,dist);
		pionResid_yb->Fill(p,yresid,dist);
	      
	      if (p>1) {
		pionThetab->Fill(tan_theta,resid,dist);
		pionTheta_xb->Fill(tan_thetax,xresid,dist);
		pionTheta_yb->Fill(tan_thetay,yresid,dist);
		
		if (resid<0.3) {
		  pionPsib->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
		}
	      } 
	    }
	
	vector<StRichRingHit*> pionHits = track->getRingHits(pion);
	for (size_t i=0;i<pionHits.size();i++) {
	  if (track->getStTrack() && fabs(pionHits[i]->getAngle())>PionConstantAreaAngle) {
	    
	    StThreeVectorF residual(-999,-999,-999);
	    if (track->getAssociatedMIP()) {
	      residual = (track->getProjectedMIP() - track->getAssociatedMIP()->local());
	    }
	    
	    double p = track->getStTrack()->geometry()->momentum().mag();
	    double resid =  residual.perp();
	    double xresid = residual.x();
	    double yresid = residual.y();
	    double dist   = pionHits[i]->getDist();
	    
	    double tan_theta = track->getMomentum().perp()/track->getMomentum().z();
	    double tan_thetax = track->getMomentum().x()/track->getMomentum().z();
	    double tan_thetay = track->getMomentum().y()/track->getMomentum().z();
	
	    
	    pionResid->Fill(p,resid,dist);
	    pionResid_x->Fill(p,xresid,dist);
	    pionResid_y->Fill(p,yresid,dist);
	    
	    if (p>1) {
	      pionTheta->Fill(tan_theta,resid,dist);
	      pionTheta_x->Fill(tan_thetax,xresid,dist);
	      pionTheta_y->Fill(tan_thetay,yresid,dist);
	      
	      if (resid<0.3) {
		pionPsi->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
	      }
	    } 
	    
	    
	  }
	}
      }
      
      
      double KaonTotalArea      = 0;
      double KaonConstantArea   = 0;
      double KaonTotalAreaAngle    = 0;
      double KaonConstantAreaAngle = 0;
      int    KaonTotalHits    = 0;
      int    KaonConstantHits = 0;
      double KaonFactor=0;
      
      if (kaonPid) {
	KaonTotalArea         = kaonPid->getTotalArea();
	KaonConstantArea      = kaonPid->getTruncatedArea();
	KaonTotalAreaAngle    = kaonPid->getTotalAzimuth();
	KaonConstantAreaAngle = kaonPid->getTruncatedAzimuth();
	KaonTotalHits         = kaonPid->getTotalHits();
	KaonConstantHits      = kaonPid->getTruncatedHits();
      }
      
      
      double ProtonTotalArea      = 0;
      double ProtonConstantArea   = 0;
      double ProtonTotalAreaAngle = 0;
      double ProtonConstantAreaAngle = 0;
      double ProtonFactor = 0;
      int ProtonTotalHits = 0;
      int ProtonConstantHits = 0;

      if (protonPid) {
	ProtonTotalArea         = protonPid->getTotalArea();
	ProtonConstantArea      = protonPid->getTruncatedArea();
	ProtonTotalAreaAngle    = protonPid->getTotalAzimuth();
	ProtonConstantAreaAngle = protonPid->getTruncatedAzimuth();
	ProtonTotalHits         = protonPid->getTotalHits();
	ProtonConstantHits      = protonPid->getTruncatedHits();
      }

      
      
      const Int_t entries=67;
      float trackArray[entries];
  
      StThreeVectorF  globalp(track->getStTrack()->geometry()->momentum());
      int counter=0;
      float defaultValue = -999;

      trackArray[counter++] = mEventN;
      trackArray[counter++] = mNumberOfPrimaries;
      trackArray[counter++] = mNegativePrimaries;
      trackArray[counter++] = posZ;
      trackArray[counter++] = negZ;

      trackArray[counter++] = mVertexPos.z();
      trackArray[counter++] = mRichTracks;  
      trackArray[counter++] = globalp.mag();  
      trackArray[counter++] = globalp.perp();
      trackArray[counter++] = globalp.x();

      trackArray[counter++] = globalp.y();
      trackArray[counter++] = globalp.z();
      trackArray[counter++] = track->getMomentum().x();
      trackArray[counter++] = track->getMomentum().y();
      trackArray[counter++] = track->getMomentum().z();
  
      trackArray[counter++] = globalp.pseudoRapidity();
      if (track->getStTrack() && track->getStTrack()->geometry()) {
	trackArray[counter++] = track->getStTrack()->geometry()->charge();
      }
      else {
	trackArray[counter++] = defaultValue;
      }

      if (track->getAssociatedMIP()) {
	trackArray[counter++] = track->getAssociatedMIP()->charge();    
	trackArray[counter++] = track->getAssociatedMIP()->local().x();
	trackArray[counter++] = track->getAssociatedMIP()->local().y();
      }
      else {
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue;  // ---------------> 20 
      }
  
      trackArray[counter++] = track->getProjectedMIP().x();
      trackArray[counter++] = track->getProjectedMIP().y();
      trackArray[counter++] = track->getImpactPoint().x();  
      trackArray[counter++] = track->getImpactPoint().y();
      trackArray[counter++] = track->getUnCorrectedImpactPoint().x();
  
      trackArray[counter++] = track->getUnCorrectedImpactPoint().y();
      trackArray[counter++] = track->getUnCorrectedTheta()/degree;  
      trackArray[counter++] = track->getUnCorrectedPhi()/degree;
      trackArray[counter++] = track->getProjectedCTBPoint().x();
      trackArray[counter++] = track->getProjectedCTBPoint().y();

      trackArray[counter++] = track->getProjectedCTBPoint().z();
      trackArray[counter++] = track->getFirstRow();
      trackArray[counter++] = track->getLastRow();  
      trackArray[counter++] = track->getLastHit().x();
      trackArray[counter++] = track->getLastHit().y();

      trackArray[counter++] = track->getLastHit().z();
      trackArray[counter++] = track->getLastHitDCA();
      trackArray[counter++] = track->getPathLength();
      trackArray[counter++] = track->getMaxChain();
      trackArray[counter++] = track->getMaxGap();   // --------> 20 + 20 = 40

      trackArray[counter++] = track->getTheta()/degree;
      trackArray[counter++] = track->getPhi()/degree;
      trackArray[counter++] = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
      trackArray[counter++] = track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);
      trackArray[counter++] = PionFactor;
void 
StRichPIDMaker::fillCorrectedNtuple() {
  
  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size();trackIndex++) {

    StRichTrack* track = mListOfStRichTracks[trackIndex];
    StRichPidTraits* pidTrait = track->getPidTrait();

    int pioncHits = 0;
    int pionctHits = 0;
    
    int kaoncHits = 0;
    int kaonctHits = 0;
    
    int protoncHits = 0;
    int protonctHits = 0;
 

      

    if (pidTrait) {
    
      StRichPid* pionPid = pidTrait->getPid(pion);
      StRichPid* kaonPid = pidTrait->getPid(kaon);
      StRichPid* protonPid = pidTrait->getPid(proton);

      
      double PionTotalArea         = 0;
      double PionConstantArea      = 0;
      double PionTotalAreaAngle    = 0;
      double PionConstantAreaAngle = 0;
      int    PionTotalHits         = 0;
      int    PionConstantHits      = 0;
      double PionFactor            = 0;
      
      if (pionPid) {
	PionTotalArea         = pionPid->getTotalArea();
	PionConstantArea      = pionPid->getTruncatedArea();
	PionTotalAreaAngle    = pionPid->getTotalAzimuth();
	PionConstantAreaAngle = pionPid->getTruncatedAzimuth();
	PionTotalHits         = pionPid->getTotalHits();
	PionConstantHits      = pionPid->getTruncatedHits();
      }

      
      if (track->fastEnough(pion) && track->isGood(pion)) {
	
	PionFactor = track->getExpectedNPhots(pion);
	
	vector<StRichRingHit*> pionHits = track->getRingHits(pion);
	for (size_t i=0;i<pionHits.size();i++) {
	  if (track->getStTrack() && 
	      track->getStTrack()->geometry() &&
	      fabs(pionHits[i]->getAngle())>PionConstantAreaAngle &&
	      fabs(track->getStTrack()->geometry()->momentum().pseudoRapidity()) < 0.5 &&
	      track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId)>40 &&
	      track->getLastHit().perp()>160.0*centimeter && 
	      track->getAssociatedMIP() && 
	      fabs(track->getAssociatedMIP()->local().x()) > 2.0*centimeter &&
	      fabs(track->getAssociatedMIP()->local().y()) > 2.0*centimeter) {
	    
	    StThreeVectorF residual(track->getProjectedMIP() - track->getAssociatedMIP()->local());
	    
	    double p = track->getStTrack()->geometry()->momentum().mag();
	    double resid =  residual.perp();
	    double xresid = residual.x();
	    double yresid = residual.y();
	    double dist   = pionHits[i]->getDist();
	    
	    double tan_theta = track->getMomentum().perp()/track->getMomentum().z();
	    double tan_thetax = track->getMomentum().x()/track->getMomentum().z();
	    double tan_thetay = track->getMomentum().y()/track->getMomentum().z();
	    
	    

	    if (fabs(mVertexPos.z())<30*centimeter) {
	      //cout << "correct  30:: vertex = " << mVertexPosition.z() << endl;
	      pionCorrectedResid->Fill(p,resid,dist);
	      pionCorrectedResid_x->Fill(p,xresid,dist);
	      pionCorrectedResid_y->Fill(p,yresid,dist);  
	      if (p>1) {
		pionCorrectedTheta->Fill(tan_theta,resid,dist);
		pionCorrectedTheta_x->Fill(tan_thetax,xresid,dist);
		pionCorrectedTheta_y->Fill(tan_thetay,yresid,dist);
		
		if (resid<0.3) {
		  pionCorrectedPsi->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
		}
	      }
	    }



	    if (fabs(mVertexPos.z()) > 30*centimeter &&
		fabs(mVertexPos.z())<60*centimeter) {
	      //cout << "corrected 30  60:: vertex = " << mVertexPosition.z() << endl;
	      pionCorrectedResidb->Fill(p,resid,dist);
	      pionCorrectedResid_xb->Fill(p,xresid,dist);
	      pionCorrectedResid_yb->Fill(p,yresid,dist);  
	      if (p>1) {
		pionCorrectedThetab->Fill(tan_theta,resid,dist);
		pionCorrectedTheta_xb->Fill(tan_thetax,xresid,dist);
		pionCorrectedTheta_yb->Fill(tan_thetay,yresid,dist);
		
		if (resid<0.3) {
		  pionCorrectedPsib->Fill(tan_theta,pionHits[i]->getAngle()/degree,dist); 
		}
	      }
	    }
	    
	    
	  }
	}
      }
      
      
      double KaonTotalArea      = 0;
      double KaonConstantArea   = 0;
      double KaonTotalAreaAngle    = 0;
      double KaonConstantAreaAngle = 0;
      int    KaonTotalHits    = 0;
      int    KaonConstantHits = 0;
      double KaonFactor=0;
      
      if (kaonPid) {
	KaonTotalArea         = kaonPid->getTotalArea();
	KaonConstantArea      = kaonPid->getTruncatedArea();
	KaonTotalAreaAngle    = kaonPid->getTotalAzimuth();
	KaonConstantAreaAngle = kaonPid->getTruncatedAzimuth();
	KaonTotalHits         = kaonPid->getTotalHits();
	KaonConstantHits      = kaonPid->getTruncatedHits();
      }
      
      
      double ProtonTotalArea      = 0;
      double ProtonConstantArea   = 0;
      double ProtonTotalAreaAngle = 0;
      double ProtonConstantAreaAngle = 0;
      double ProtonFactor = 0;
      int ProtonTotalHits = 0;
      int ProtonConstantHits = 0;

      if (protonPid) {
	ProtonTotalArea         = protonPid->getTotalArea();
	ProtonConstantArea      = protonPid->getTruncatedArea();
	ProtonTotalAreaAngle    = protonPid->getTotalAzimuth();
	ProtonConstantAreaAngle = protonPid->getTruncatedAzimuth();
	ProtonTotalHits         = protonPid->getTotalHits();
	ProtonConstantHits      = protonPid->getTruncatedHits();
      }




    vector<StRichRingHit*> temppionHits = track->getRingHits(pion);
    for (size_t i=0;i<temppionHits.size();i++) {
      if (temppionHits[i]->getDist()>0 && temppionHits[i]->getDist() < 1) {
	pioncHits++;
	if (fabs(temppionHits[i]->getAngle())>PionConstantAreaAngle) {pionctHits++;} 
      }
    }

    vector<StRichRingHit*> tempkaonHits = track->getRingHits(kaon);
    for (size_t i=0;i<tempkaonHits.size();i++) {
      if (tempkaonHits[i]->getDist()>0 && tempkaonHits[i]->getDist() < 1) {
	kaoncHits++;
	if (fabs(tempkaonHits[i]->getAngle())>KaonConstantAreaAngle) {kaonctHits++;} 
      }
    }

    vector<StRichRingHit*> tempproHits = track->getRingHits(proton);
    for (size_t i=0;i<tempproHits.size();i++) {
      if (tempproHits[i]->getDist()>0 && tempproHits[i]->getDist() < 1) {
	protoncHits++;
	if (fabs(tempproHits[i]->getAngle())>ProtonConstantAreaAngle) {protonctHits++;} 
      }
    }

      
      
      const Int_t entries=57;
      float trackArray[entries];
  
      StThreeVectorF  globalp(track->getStTrack()->geometry()->momentum());
      int counter=0;
      float defaultValue = -999;

      trackArray[counter++] = mVertexPos.z();

      trackArray[counter++] = globalp.x();
      trackArray[counter++] = globalp.y();
      trackArray[counter++] = globalp.z();

      trackArray[counter++] = track->getMomentum().x();
      trackArray[counter++] = track->getMomentum().y();
      trackArray[counter++] = track->getMomentum().z();

      trackArray[counter++] = track->getUnCorrectedMomentum().x();
      trackArray[counter++] = track->getUnCorrectedMomentum().y();
      trackArray[counter++] = track->getUnCorrectedMomentum().z(); // 10
  
      trackArray[counter++] = globalp.pseudoRapidity();
      if (track->getStTrack() && track->getStTrack()->geometry()) {trackArray[counter++] = track->getStTrack()->geometry()->charge();}
      else {trackArray[counter++] = defaultValue;}

      if (track->getAssociatedMIP()) {
	trackArray[counter++] = track->getAssociatedMIP()->charge();    
	trackArray[counter++] = track->getAssociatedMIP()->local().x();
	trackArray[counter++] = track->getAssociatedMIP()->local().y();
      }
      else {
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue;
	trackArray[counter++] = defaultValue; 
      }
  
      trackArray[counter++] = track->getProjectedMIP().x();
      trackArray[counter++] = track->getProjectedMIP().y();
      
      trackArray[counter++] = track->getImpactPoint().x();  
      trackArray[counter++] = track->getImpactPoint().y();

      trackArray[counter++] = track->getUnCorrectedImpactPoint().x();  // 20
      trackArray[counter++] = track->getUnCorrectedImpactPoint().y();

      trackArray[counter++] = track->getUnCorrectedProjectedMIP().x();
      trackArray[counter++] = track->getUnCorrectedProjectedMIP().y();

      trackArray[counter++] = track->getLastHit().x();
      trackArray[counter++] = track->getLastHit().y();
      trackArray[counter++] = track->getLastHit().z();

      trackArray[counter++] = track->getStTrack()->detectorInfo()->numberOfPoints(kTpcId);
      trackArray[counter++] = track->getStTrack()->fitTraits().numberOfFitPoints(kTpcId);

      trackArray[counter++] = PionFactor; 

      trackArray[counter++] = PionTotalArea; // 30
      trackArray[counter++] = PionConstantArea;  
      trackArray[counter++] = PionTotalAreaAngle;
      trackArray[counter++] = PionConstantAreaAngle;
      trackArray[counter++] = PionTotalHits;  

      trackArray[counter++] = PionConstantHits;
      trackArray[counter++] = KaonFactor;
      trackArray[counter++] = KaonTotalArea;  
      trackArray[counter++] = KaonConstantArea;
      trackArray[counter++] = KaonTotalAreaAngle; 

      trackArray[counter++] = KaonConstantAreaAngle; // 40  
      trackArray[counter++] = KaonTotalHits;
      trackArray[counter++] = KaonConstantHits;
      trackArray[counter++] = ProtonFactor; 
      trackArray[counter++] = ProtonTotalArea;  

      trackArray[counter++] = ProtonConstantArea;
      trackArray[counter++] = ProtonTotalAreaAngle;  
      trackArray[counter++] = ProtonConstantAreaAngle;
      trackArray[counter++] = ProtonTotalHits;
      trackArray[counter++] = ProtonConstantHits; 

      trackArray[counter++] = pioncHits; // 50
      trackArray[counter++] = pionctHits;

      trackArray[counter++] = kaoncHits;
      trackArray[counter++] = kaonctHits;

      trackArray[counter++] = protoncHits;
      trackArray[counter++] = protonctHits;

      trackArray[counter++] = mMaterialDb->innerWavelength()/nanometer;
      trackArray[counter++] = mMaterialDb->outerWavelength()/nanometer; // 57 
      if (counter != entries) {
	cout<< "StRichPIDMaker:: fillPIDNtuple. counter = " << counter << "   --> abort." << endl; abort();} 
    file = new TFile("/star/rcf/scratch/lasiuk/distance.root","RECREATE");
    }
  }
    distup = new TNtuple("dist","b","xi:yi:xo:yo:si:ld:d:oldd:oldsig:phx:phy:x:y:px:py:pz:theta:evt:numb:resx:resy:res:ring:cos:d2siline:p:q:vtx");
      trackArray[counter++] = PionTotalArea;
      trackArray[counter++] = PionConstantArea;  
      trackArray[counter++] = PionTotalAreaAngle;
      trackArray[counter++] = PionConstantAreaAngle;
      trackArray[counter++] = PionTotalHits;  

      trackArray[counter++] = PionConstantHits;
      trackArray[counter++] = KaonFactor;
      trackArray[counter++] = KaonTotalArea;  
      trackArray[counter++] = KaonConstantArea;
      trackArray[counter++] = KaonTotalAreaAngle;

    trackCorrectedNtuple = new TNtuple("trackCorrectedNtuple","","vz:globalpx:globalpy:globalpz:localpx:localpy:localpz:olocalpx:olocalpy:olocalpz:eta:q:amipq:amipx:amipy:pmipx:pmipy:radx:rady:oradx:orady:opmipx:opmipy:lasthitx:lasthity:lasthitz:tpchits:tpcfits:pionf:piontotarea:pionconstarea:piontotang:pionconstang:piontothits:pionconsthits:kaonf:kaontotarea:kaonconstarea:kaontotang:kaonconstang:kaontothits:kaonconsthits:prof:prototarea:proconstarea:prototang:proconstang:protothits:proconsthits:pionchits:pioncthits:kaonchits:kaoncthits:prochits:procthits:wave1:wave2");
      trackArray[counter++] = KaonTotalHits;
      trackArray[counter++] = KaonConstantHits;
      trackArray[counter++] = ProtonFactor; 
      trackArray[counter++] = ProtonTotalArea;  // ------> 40 + 20 = 60

      trackArray[counter++] = ProtonConstantArea;
      trackArray[counter++] = ProtonTotalAreaAngle;  
      trackArray[counter++] = ProtonConstantAreaAngle;
      trackArray[counter++] = ProtonTotalHits;
      trackArray[counter++] = ProtonConstantHits;

      trackArray[counter++] = mMaterialDb->innerWavelength()/nanometer;
      trackArray[counter++] = mMaterialDb->outerWavelength()/nanometer; // ---> 67
      if (counter != entries) {
	cout<< "StRichPIDMaker:: fillPIDNtuple. counter = " << counter << "   --> abort." << endl; abort();} 
      trackNtuple->Fill(trackArray);
    }
  }
}


    file = new TFile("/star/rcf/scratch/lasiuk/test.root","RECREATE");
void StRichPIDMaker::initNtuples() {
#ifdef  myRICH_WITH_NTUPLE
//     char finalname[200];
//     sprintf(finalname,"%s.root",mySaveDirectory);
//     file = new TFile(finalname,"RECREATE");

    file = new TFile("/star/rcf/scratch/lasiuk/exb/dtuple.root","RECREATE");
    file->SetFormat(1);

    distup = new TNtuple("dist","b","xi:yi:xo:yo:si:ld:d:oldd:oldsig:oldsi:phx:phy:x:y:px:py:pz:theta:evt:numb:resx:resy:res:ring:cos:d2siline:p:q:vtx");



    trackNtuple = new TNtuple("trackNtuple","trackwise tuple","evtn:nprimaries:nnegprimaries:posz:negz:vz:nrichtracks:globalp:globalpt:globalpx:globalpy:globalpz:localpx:localpy:localpz:eta:q:amipq:amipx:amipy:pmipx:pmipy:radx:rady:oradx:orady:otheta:ophi:ctbx:ctby:ctbz:firstrow:lastrow:lasthitx:lasthity:lasthitz:lasthitdca:pathlength:maxchain:maxgap:theta:phi:tpchits:tpcfitpoints:pionfactor:piontotalarea:pionconstarea:piontotalangle:pionconstangle:piontotalhits:pionconsthits:kaonfactor:kaontotalarea:kaonconstarea:kaontotalangle:kaonconstangle:kaontotalhits:kaonconsthits:protonfactor:protontotalarea:protonconstarea:protontotalangle:protonconstangle:protontotalhits:protonconsthits:innerwave:outerwave");

#ifdef myRICH_WITH_MC
    geantTrackNtuple = new TNtuple("geantTrackNtuple","geant trackwise tuple",
				 "evtn:nprimaries:nnegprimaries:vz:nrichtracks:globalpx:globalpy:globalpz:localpx:localpy:localpz:eta:q:amipid:amipproc:amipq:amipx:amipy:amipnpads:pmipx:pmipy:gmipid:gmipproc:gmipq:gmipx:gmipy:gmipnpads:radx:rady:oradx:orady:olocalpx:olocalpy:olocalpz:firstrow:lastrow:lasthitx:lasthity:lasthitz:lasthitdca:pathlength:maxchain:maxgap:tpchits:tpcfitpoints:innerwave:outerwave:glocalpx:glocalpy:glocalpz:gradx:grady:geantmipx:geantmipy:gstopvertx:gstopverty:gstopvertz:gphots:grecophots:gradhits:gtpccommonhits:gglobalpx:gglobalpy:gglobalpz:gid:gstopproc:gnpartners:pionfactor:piontotalarea:pionconstarea:piontotalangle:pionconstangle:piontotalhits:pionconsthits:kaonfactor:kaontotalarea:kaonconstarea:kaontotalangle:kaonconstangle:kaontotalhits:kaonconsthits:protonfactor:protontotalarea:protonconstarea:protontotalangle:protonconstangle:protontotalhits:protonconsthits");

    geantPhotonNtuple = new TNtuple("geantPhotonNtuple","geant photon wise tnuple","q:localpx:localpy:localpz:radx:rady:gradx:grady:pmipx:pmipy:amipid:amipproc:amipq:amipx:amipy:amipnpads:gamipid:gamipproc:gamipq:gamipx:gamipy:gamipnpads:gmipx:gmipy:glocalpx:glocalpy:glocalpz:theta:gtheta:phi:gphi:gid:signal:gwave:gpsi:gz:gproc:x:y:gx:gy:gmass:constangle:trdist:trang:tgdist:tgang:gconstangle:grdist:grang:ggdist:ggang");
  
    geantPixelNtuple = new TNtuple("geantPixelNtuple","pixels","adc:n:gid0:gid1:gid2:gid3:q0:q1:q2:q3:proc0:proc1:proc2:proc3");
    geantCloseHitNtuple = new TNtuple("geantHitNtuple","pixels","p:theta:w1:w2:psi1:psi2:z1:z2:x1:y1:x2:y2:constAngle");
#endif
  
    pionResid   = new TH3F("pionResid","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionResid_x = new TH3F("pionResid_x","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionResid_y = new TH3F("pionResid_y","hit distribution",16,0,4,40,-2,2,100,-5,5);
    
    pionTheta   = new TH3F("pionTheta","hit distribution",100,-1,1,20,0,2,100,-5,5);
    pionTheta_x = new TH3F("pionTheta_x","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    pionTheta_y = new TH3F("pionTheta_y","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    
    pionPsi     = new TH3F("pionPsi","hit dist",100,-1,1,180,-180,180,100,-5,5);

    //
    //
    pionResidb   = new TH3F("pionResidb","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionResid_xb = new TH3F("pionResid_xb","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionResid_yb = new TH3F("pionResid_yb","hit distribution",16,0,4,40,-2,2,100,-5,5);
    
    pionThetab   = new TH3F("pionThetab","hit distribution",100,-1,1,20,0,2,100,-5,5);
    pionTheta_xb = new TH3F("pionTheta_xb","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    pionTheta_yb = new TH3F("pionTheta_yb","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    
    pionPsib     = new TH3F("pionPsib","hit dist",100,-1,1,180,-180,180,100,-5,5);

    pionCorrectedResid   = new TH3F("pionCorrectedResid","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionCorrectedResid_x = new TH3F("pionCorrectedResid_x","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionCorrectedResid_y = new TH3F("pionCorrectedResid_y","hit distribution",16,0,4,40,-2,2,100,-5,5);
    
    pionCorrectedTheta   = new TH3F("pionCorrectedTheta","hit distribution",100,-1,1,20,0,2,100,-5,5);
    pionCorrectedTheta_x = new TH3F("pionCorrectedTheta_x","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    pionCorrectedTheta_y = new TH3F("pionCorrectedTheta_y","hit distribution",100,-1,1,40,-2,2,100,-5,5);

    pionCorrectedPsi     = new TH3F("pionCorrectedPsi","hit dist",100,-1,1,180,-180,180,100,-5,5);

    pionCorrectedResidb   = new TH3F("pionCorrectedResidb","hit distribution",16,0,4,20,0,2,100,-5,5);
    pionCorrectedResid_xb = new TH3F("pionCorrectedResid_xb","hit distribution",16,0,4,40,-2,2,100,-5,5);
    pionCorrectedResid_yb = new TH3F("pionCorrectedResid_yb","hit distribution",16,0,4,40,-2,2,100,-5,5);

    pionCorrectedThetab   = new TH3F("pionCorrectedThetab","hit distribution",100,-1,1,20,0,2,100,-5,5);
    pionCorrectedTheta_xb = new TH3F("pionCorrectedTheta_xb","hit distribution",100,-1,1,40,-2,2,100,-5,5);
    pionCorrectedTheta_yb = new TH3F("pionCorrectedTheta_yb","hit distribution",100,-1,1,40,-2,2,100,-5,5);

    pionCorrectedPsib     = new TH3F("pionCorrectedPsib","hit dist",100,-1,1,180,-180,180,100,-5,5);

    //
    //
    pionRadius  = new TH3F("pionRadius","hit dist",16,0,4,180,0,18,100,-5,5);
  
    hitNtup = new TNtuple("hitNtup","hitwise ","npads:adcsum:trackmip:x:y:ring:p:resid:tpchits:maxamp:residx:residy:theta:quartz:rad:angle");
  
    closeHitNtup = new TNtuple("closeHitNtup","hitwise ","p:theta:n:constAngle:psi1:psi2:x1:y1:x2:y2");
    
    overLap = new TNtuple("overLap","hit distribution","p:theta:dist:pionhit:kaonhit:angle");
    
    evtNtup = new TNtuple("evtNtup","evtwise","rtracks:rtracks1gev:pri:npri:vx:vy:vz:hits:pixels");
    
    psiFixNtuple = new TNtuple("psiFixNtuple","","dist:sigma:hitangle:constangle:area:charge:px:py:pz:mass:x:y");
#endif
}

#ifdef myRICH_WITH_MC
bool StRichPIDMaker::makeTrackAssociations(StMcEvent* temp_mEvent, const StSPtrVecRichHit* hits ) {

  if (!temp_mEvent || !hits) {
    cout << "No StMcEvent/rich hits!" << endl;
    return false;}
  
  // StAssociationMaker
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  if (!assoc) {
    cout << "No association maker!" << endl;
    return false;} 
  
  rcTrackMapType*  theTrackMap = 0;
  theTrackMap = assoc->rcTrackMap();
  if (!theTrackMap) {
    cout << "no track map!" << endl;
    return false;} 

  for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) { // track    

    unsigned int maxCommonTpcHits = 0;
    unsigned int numberOfPartners = 0;

    StMcTrack*      mcPartner   = 0;
    StRichMCTrack*  richMcTrack = dynamic_cast<StRichMCTrack*>(mListOfStRichTracks[trackIndex]);
    cout << "rich mc track = " << richMcTrack << endl;
    if (richMcTrack && richMcTrack->getStTrack() && 
	richMcTrack->getStTrack()->node() &&
	richMcTrack->getStTrack()->node()->track(global)) {
      
      StGlobalTrack* globaltrack =
	  dynamic_cast<StGlobalTrack*>(richMcTrack->getStTrack()->node()->track(global));
      cout << "global track pointer= " << globaltrack << endl;

      if (globaltrack) { 
	pair<rcTrackMapIter,rcTrackMapIter> trackBounds = theTrackMap->equal_range(globaltrack);
	cout << "about to loop over track bounds " << endl;
	
	for (rcTrackMapIter rcIt = trackBounds.first; rcIt != trackBounds.second; ++rcIt) {
	  numberOfPartners++;  
	  if ((*rcIt).second->commonTpcHits() >  maxCommonTpcHits) {
	    mcPartner        = (*rcIt).second->partnerMcTrack();
	    maxCommonTpcHits = (*rcIt).second->commonTpcHits();
	  }
	}
	
	
	// need to get the g2t info  
	St_DataSet* dsGeant = GetDataSet("geant");
	if(!dsGeant || !dsGeant->GetList()) {
	  gMessMgr->Warning() << "Could not find dataset geant" << endm;
	  dsGeant = GetDataSet("event/geant/Event");
	  if(!dsGeant || !dsGeant->GetList()) {  // Try direct output from Geant
	    gMessMgr->Warning() << "Could not find dataset event/geant/Event" << endm;
	    return false;
	  }
	}
       
	// Now the Iterator is set up, and this allows us to access the tables
	// This is done like so:
	// TableClass *instanceOfTableClassPointer = 
	// cast to TableClassPointer instanceOfDataSetIter("actual name of table in data set");
	St_DataSetIter geantDstI(dsGeant);
	St_g2t_track   *g2t_track = (St_g2t_track   *) geantDstI("g2t_track");

	richMcTrack->setStMcTrack(mcPartner);
	richMcTrack->setGeantPhotons(temp_mEvent);	
	richMcTrack->setCommonTpcHits(maxCommonTpcHits);
	richMcTrack->setNumberOfPartners(numberOfPartners);
	richMcTrack->setGeantRecoMIP(hits, temp_mEvent, g2t_track);
	richMcTrack->setRecoGeantPhotons(hits,temp_mEvent,g2t_track);	
      }
    }
  }
  return true;
}
#endif


#ifdef  myRICH_WITH_MC
StMcTrack* StRichPIDMaker::getStMcTrack(StRichMCHit* hit, StMcEvent* mcevt, St_g2t_track* geantTracks) {
    
  StMcTrack* mcTrack=0;
  if (!hit || !mcevt || !geantTracks) return mcTrack;
  g2t_track_st *trackList = geantTracks->GetTable();

  unsigned int hitIndex = 0;
  if (hit->getMCInfo().process()==ePhoton)       hitIndex = hit->getMCInfo().id();
  else if (hit->getMCInfo().process()==eCharged) hitIndex = hit->getMCInfo().trackp();
  else { return 0;}
  
  float  start=0;
  float  end = mcevt->tracks().size()-1;
  int    index = hitIndex;
  int    counter=0;
  bool   searching=true;
  while (searching) {
    if (index >= mcevt->tracks().size()-1) return 0;
    if (trackList[hitIndex].id == mcevt->tracks()[index]->key()) {return mcevt->tracks()[index];}
    if (trackList[hitIndex].id > mcevt->tracks()[index]->key()) {start=index;}
    else {end=index;}
    index=(end-start)/2 + start;
    counter++;
    if (counter>mcevt->tracks().size()-1) searching=false; 
  }
  return mcTrack;  
}
#endif

#ifdef  myRICH_WITH_MC
void StRichPIDMaker::getGeantPhotonInfo(StRichMCTrack* richTrack, StMcTrack* photon, 
				     float& wave, float& gpsi, float& z) { 

  wave = -999;
  gpsi = -999;
  z = -999;
  if (!richTrack || !photon || photon->geantId()!=50) { return;}
   
  StRichLocalCoordinate localStartVert(-999,-999,-999);
  if (photon->startVertex()) {
    StGlobalCoordinate globalStartVert(photon->startVertex()->position().x(),
				       photon->startVertex()->position().y(),
				       photon->startVertex()->position().z());
    (*mCoordinateTransformation)(globalStartVert,localStartVert);
  }
  z = localStartVert.position().z();


  // get photons momentum at emission pt
 StThreeVector<double> globalMomentum(photon->momentum().x(),
				      photon->momentum().y(),
				      photon->momentum().z());
  
  // get geant photons azimuthal angle of emission
  // need to normalize vector (mag of photon vector ~ 1.0e-9)
  if (globalMomentum.mag()==0) {return;}
  globalMomentum.setMag(1.0);
  StThreeVector<double> localMomentum;
  mMomentumTransformation->localMomentum(globalMomentum,localMomentum);

  StThreeVectorF trackLocalMomentum = richTrack->getGeantMomentumAtRadiator();  
  if (trackLocalMomentum.mag() == 0) {return;}
  trackLocalMomentum.setMag(1.0);

  StThreeVectorF vect(0.0,0.0,1.0); 
  double rotationTheta = acos(vect.dot(trackLocalMomentum));
  double rotationPhi   = trackLocalMomentum.phi();
  
  // do rotation
  StThreeVectorF 
    rotatedTrackMomentum(cos(rotationTheta)*cos(rotationPhi)*trackLocalMomentum.x() +
			 cos(rotationTheta)*sin(rotationPhi)*trackLocalMomentum.y() -
			 sin(rotationTheta)*trackLocalMomentum.z(),
			 
			 -sin(rotationPhi)*trackLocalMomentum.x() +
			 cos(rotationPhi)*trackLocalMomentum.y(),
			 
			 cos(rotationPhi)*sin(rotationTheta)*trackLocalMomentum.x() +
			 sin(rotationPhi)*sin(rotationTheta)*trackLocalMomentum.y() +
			 cos(rotationTheta)*trackLocalMomentum.z());
  
  StThreeVectorF 
    photonRotatedMomentum(cos(rotationTheta)*cos(rotationPhi)*localMomentum.x() +
			  cos(rotationTheta)*sin(rotationPhi)*localMomentum.y() -
			  sin(rotationTheta)*localMomentum.z(),
			  
			  -sin(rotationPhi)*localMomentum.x() +
			  cos(rotationPhi)*localMomentum.y(),
			  
			  cos(rotationPhi)*sin(rotationTheta)*localMomentum.x() +
			  sin(rotationPhi)*sin(rotationTheta)*localMomentum.y() +
			  cos(rotationTheta)*localMomentum.z());
  
  gpsi = atan2( rotatedTrackMomentum.x() - photonRotatedMomentum.x(),
		rotatedTrackMomentum.y() - photonRotatedMomentum.y());
  
  double constantPhaseDifference = M_PI/2.0;
  gpsi = gpsi - constantPhaseDifference;
  if (gpsi < -M_PI) {gpsi = gpsi + 2.0*M_PI;}
  
  // get photons wavelength (nm)
  wave = ( ( (h_Planck/eV)*c_light)/(photon->energy()/eV) )/nanometer; 
}
#endif


#ifdef  myRICH_WITH_MC
StThreeVectorF StRichPIDMaker::getTheGeantHitOnPadPlane(StMcTrack* mcTrack, StThreeVectorF& inputHit) { 
  
  
  double anodeDistanceToPadPlane = mGeometryDb->anodeToPadSpacing();
  float  defaultValue = -999.9;
  StThreeVectorF hit(defaultValue,defaultValue,defaultValue);
  StThreeVectorF tempHit(defaultValue,defaultValue,defaultValue);
  if (!mcTrack) return hit;

  // photon case  --> get point on pad plane
  if (mcTrack->geantId()==50) {

    StGlobalCoordinate testGlobal(mcTrack->richHits()[0]->position().x(),
				  mcTrack->richHits()[0]->position().y(),
				  mcTrack->richHits()[0]->position().z());
    
    StRichLocalCoordinate testLocalPoint(0,0,0);
    (*mCoordinateTransformation)(testGlobal,testLocalPoint);
    
    hit.setX(testLocalPoint.position().x());
    hit.setY(testLocalPoint.position().y());
    hit.setZ(testLocalPoint.position().z());
  }

  // charged particle case    ---> get point on anode wire
  else {
    for (size_t i=0;i<mcTrack->richHits().size();i++) {

       StGlobalCoordinate testGlobal(mcTrack->richHits()[i]->position().x(),
				     mcTrack->richHits()[i]->position().y(),
				     mcTrack->richHits()[i]->position().z());
      
      StRichLocalCoordinate testLocalPoint(0,0,0);
      (*mCoordinateTransformation)(testGlobal,testLocalPoint);

      tempHit.setX(testLocalPoint.position().x());
      tempHit.setY(testLocalPoint.position().y());
      tempHit.setZ(testLocalPoint.position().z());
  
      // geant mip at anode wire position      
      if ( fabs(tempHit.z()) > 0.00 && fabs(tempHit.z()) < 2.0*anodeDistanceToPadPlane ) {
	if ( (tempHit-inputHit).perp() < (hit-inputHit).perp() ) {hit = tempHit;}
      } 
    }  
  }

  return hit;
}

#endif

#ifdef RICH_WITH_L3_TRACKS
double StRichPIDMaker::findL3ZVertex(globalTrack * trackVec,int nTracks){

    TH1D temp("temp","vertex",500,-100,100);
    for(int i = 0;i<nTracks;i++){
	double currentVertex;
	currentVertex = trackVec[i].z0-trackVec[i].tanl*trackVec[i].r0*cos(trackVec[i].psi-trackVec[i].phi0);
	temp.Fill(currentVertex);
    }

    return temp.GetBinCenter(temp.GetMaximumBin());    
}
#endif




ClassImp(StRichPIDMaker)
  
    








