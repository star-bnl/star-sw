/******************************************************
 * $Id: StRichPIDMaker.cxx,v 2.22 2000/11/28 19:21:01 lasiuk Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 2.22  2000/11/28 19:21:01  lasiuk
 * correct memory leak in writing to StEvent
 * add additional WARNING for tracks without assigned MIP
 *
 * Revision 2.22  2000/11/28 19:21:01  lasiuk
 * correct memory leak in writing to StEvent
 * add additional WARNING for tracks without assigned MIP
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

// StChain, etc...
#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

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
#endif


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
#include "StEventTypes.h"
#include "StRichPid.h"
#include "StRichPhotonInfo.h"
#include "StRichPidTraits.h"
#include "StTpcDedxPidAlgorithm.h"

#include "StEventMaker/StRootEventManager.hh"

// StRrsMaker, StRchMaker
#include "StRrsMaker/StRichGeometryDb.h"
#include "StRrsMaker/StRichCoordinateTransform.h"
#include "StRrsMaker/StRichEnumeratedTypes.h"
#include "StRchMaker/StRichSimpleHitCollection.h"

// StRichPIDmaker
#include "StRichRingCalculator.h"
#include "StRichRingDefinition.h"
#include "StRichTrack.h"
#include "StRichMCTrack.h"

#ifdef myRICH_WITH_MC
// StMCEvent
#include "StMcEvent/StMcEventTypes.hh"
#include "StMcEventMaker/StMcEventMaker.h"

// StAssociationMaker
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

// g2t tables
#include "tables/St_g2t_track_Table.h"
#endif

// magnetic field map
//#include "StarCallf77.h"
//#define gufld  F77_NAME(gufld,GUFLD)
//extern "C" {void gufld(Float_t *, Float_t *);}

static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 2.22 2000/11/28 19:21:01 lasiuk Exp $";

StRichPIDMaker::StRichPIDMaker(const Char_t *name, bool writeNtuple) : StMaker(name) {
  drawinit = kFALSE;
  fileName = 0;
  kWriteNtuple=writeNtuple;

  //
  // initialize cuts, values and parameters for processing
  //
  
  this->initCutParameters();
  
}

StRichPIDMaker::~StRichPIDMaker() {}

void StRichPIDMaker::initCutParameters() {
    //
    // Event Level
    //
    mVertexWindow = 200.*centimeter;

    //
    // Hit Level (Depracated)
    //
    mAdcCut = 300; 

    //
    // Track Level
    //
    mPtCut = 0.0*GeV; // GeV/c
    mEtaCut = 0.5; 
    mLastHitCut = 160.0*centimeter;
    mDcaCut = 3.0*centimeter;
    mFitPointsCut = 20;
    mPathCut = 500*centimeter;
    mPadPlaneCut = 2.0*centimeter;
    mRadiatorCut = 2.0*centimeter;

    mThresholdMomentum=.3*GeV;

    //
    // Convergence parameter for psi determination
    //
    mPrecision = 100*micrometer;

    //
    // GK, November 2, 2000
    // bins are:
    //   0       1       2      3     4     5
    // <-10  -10<t<-5 -5<t<0  0<t<5 5<t<10 10<t
    // helix h- = 0
    // helix h+ = 1
    //
    // negatives
    //
    meanD[0][0] = .702; sigmaD[0][0] = .209;
    meanD[1][0] = .666; sigmaD[1][0] = .188;
    meanD[2][0] = .640; sigmaD[2][0] = .235;
    meanD[3][0] = .739; sigmaD[3][0] = .166;
    meanD[4][0] = .737; sigmaD[4][0] = .259;
    meanD[5][0] = .657; sigmaD[5][0] = .183;

    //
    // positives
    //
    meanD[0][1] = .595; sigmaD[0][1] = .486;
    meanD[1][1] = .669; sigmaD[1][1] = .253;
    meanD[2][1] = .753; sigmaD[2][1] = .271;
    meanD[3][1] = .764; sigmaD[3][1] = .275;
    meanD[4][1] = .763; sigmaD[4][1] = .234;
    meanD[5][1] = .801; sigmaD[5][1] = .388;
    
}

Int_t StRichPIDMaker::Init() {

  //
  // instantiate StParticleDefinition's and store in vector
  //
  pion    = StPionMinus::instance();
  kaon    = StKaonMinus::instance();
  proton  = StAntiProton::instance();
    
  mListOfParticles.clear();
  mListOfParticles.resize(3);
  mListOfParticles[0]   = pion;
  mListOfParticles[1]   = kaon;
  mListOfParticles[2]   = proton;
  
  //
  // get data bases
  //
  mMaterialDb = StRichMaterialsDb::getDb();
  mGeometryDb = StRichGeometryDb::getDb(); 
  mCoordinateTransformation = StRichCoordinateTransform::getTransform(mGeometryDb);
  mMomentumTransformation = StRichMomentumTransform::getTransform(mGeometryDb);
  
  //
  // set pad plane display pointer to zero
  //
  mPadMonitor = 0;
  
  //
  // default for area calculation 
  //
  mDoGapCorrection      = true;
    
  
  //
  // hard coded values for now!
  // this->setWaveLenght(small,large)
  // can be used to change wavelenghts
  //
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
	cout << "\tSkip Event" << endl;
	return kStWarn;
    }

    const StSPtrVecRichHit*     pRichHits      = 0;
    const StSPtrVecRichCluster* pRichClusters  = 0;
    const StSPtrVecRichPixel*   pRichPixels    = 0;
    
    int myRichHits   = 0;
    int myRichPixels = 0;
    
    if (richCollection->pixelsPresent()) {
	myRichPixels = richCollection->getRichPixels().size();
	const StSPtrVecRichPixel& richPixels =
	    richCollection->getRichPixels();
	pRichPixels = &richPixels;
#ifdef  myRICH_WITH_NTUPLE
	int adcsum       = 0;
	StSPtrVecRichPixelConstIterator i;
	for (i=pRichPixels->begin(); i != pRichPixels->end(); ++i) {
	    adcsum += (*i)->adc();
	}
#endif
    }
    
    if (richCollection->clustersPresent()) {
	const StSPtrVecRichCluster& richClusters =
	    richCollection->getRichClusters();
	pRichClusters = &richClusters;
    }
    
    if (richCollection->hitsPresent()) {
	const StSPtrVecRichHit& richHits =
	    richCollection->getRichHits();
	pRichHits = &richHits;
	if(pRichHits)
	    myRichHits = pRichHits->size();
    }
    
    
    //
    // LOAD TRACKS intersecting RICH
    // 
    mNumberOfPrimaries  = this->fillTrackList(rEvent,pRichHits);
    mRichTracks = mListOfStRichTracks.size();


    //
    // Check if the tpcHitCollection Exists
    // If it does, be sure the hits which
    // are associated with the StTrack are there,
    // otherwise, put them there
    //
    size_t ii;

    if(!rEvent->tpcHitCollection()) {
	//cout << "StRichPIDMaker:Make()\n";
	//cout << "\tNo StTpcHitCollection\n";
	//cout << "\tTry make one" << endl;
	StTpcHitCollection* theTpcHitCollection = new StTpcHitCollection();
	rEvent->setTpcHitCollection(theTpcHitCollection);

	//cout << "\tMake an StRootEventManager" << endl;
	StRootEventManager* theEvtManager = new StRootEventManager();
	theEvtManager->setMaker(this);

	//cout << "\tQuery to the .dst" << endl;
	int status = theEvtManager->openEvent("dst");
	if(status == oocError) {
	    cout << "StRichPIDMaker::Make()\n";
	    cout << "\tWARNING:\n";
	    cout << "\tCannot Open dataset \"dst\"";
	    cout << "\tCannot make the StTpcHitCollection\n" << endl;
	}
	else {
	    long nrows;
	    dst_point_st* dstPoints = theEvtManager->returnTable_dst_point(nrows);

	    if(!dstPoints) {
		cout << "StRichPIDMaker::Make()\n";
		cout << "\tWARNING: Cannot get the dstPoints\n";
		cout << "\tContinuing..." << endl;
	    }
	    else {
		for(ii=0; ii<mListOfStRichTracks.size(); ii++) {
		    StTrack* currentTrack = mListOfStRichTracks[ii]->getStTrack();
		    if(!currentTrack->detectorInfo()) {
			cout << "StRichPIDMaker::Make()\n";
			cout << "\tWARNING: No detectorInfo()\n";
			cout << "\tassocciated with the track.  Continuing..." << endl;
			continue;
		    }
		    
		    unsigned short trackKey = currentTrack->key(); 
		    for(int i=0; i<nrows; i++) { //nrows
			if(dstPoints[i].id_track != trackKey) continue;
			StTpcHit* tpcHit = new StTpcHit(dstPoints[i]);
			
			//
			// add the hit to the tpcHitCollection which
			// will own and manage the memory for the hit
			// while we must also add it to the detectorInfo()
			// of the current track, as this is how we will
			// access it later
			//
			if(theTpcHitCollection->addHit(tpcHit)) {
			    currentTrack->detectorInfo()->addHit(tpcHit);
			}
			
		    } // loop over the hits

		} // loop over the Rich Tracks
			
		delete theEvtManager;
		theEvtManager=0;
	    } // else Got 'em
	} // else 
    } // check on the tpcHitCollection

    //
    // The StTpcHitCollection should exist at this
    // point whether it is created by the PIDMaker
    // or not

    
    // 
    // This is the beginning of the main PID loop
    //
    


#ifdef RICH_WITH_PAD_MONITOR 

    //
    // if pad plane display monitor is active
    // get pointer and clear display
    //
    mPadMonitor=StRichPadMonitor::getInstance(mGeometryDb);
    mPadMonitor->clearMisc();
#endif


    StPtrVecHit tpcHits;
    StRichRingCalculator* ringCalc;
  
    for (ii=0; ii<mListOfStRichTracks.size(); ii++) {     
	StRichTrack* richTrack = mListOfStRichTracks[ii];

	if(richTrack->getStTrack()->detectorInfo())
	    tpcHits = richTrack->getStTrack()->detectorInfo()->hits(kTpcId);

	//
	// This is the place for the track refit to be
	// done using the tpcHits
	//
//  	unsigned short numberOfTpcHits = tpcHits.size();
//  	cout << "We have " << numberOfTpcHits << " TPC hits to work with" << endl;
// 	for(size_t zz=0; zz<numberOfTpcHits; zz++)
// 	    cout << "\t" << zz << " " << tpcHits[zz]->position()
// 		 << " row " << dynamic_cast<StTpcHit*>(tpcHits[zz])->padrow() << endl;
	//
	// Loop over the particle hypothesis
	//
	for (size_t iParticle=0; iParticle<mListOfParticles.size(); iParticle++) {
	    
	    //
	    // check if track is above Cherenkov threshold for this species
	    // and if the back portion of the ring is not refracted away
	    //
	    if ( richTrack->fastEnough(mListOfParticles[iParticle]) &&
		 richTrack->isGood(mListOfParticles[iParticle]) ) {
	    
		ringCalc = new StRichRingCalculator(richTrack,mListOfParticles[iParticle]);
		
		this->hitFilter(pRichHits,ringCalc);

		//
		// calculate all areas, angles using default input parameters
		// gap correction on, no angle cut (but the constant area is still ok) and 
		// number of points used in calculation == 3600
		//
		ringCalc->calculateArea();
		this->fillPIDTraits(ringCalc);
	    
		delete ringCalc;  
	    
		ringCalc = 0;
	    }
	} // loop over particles

    } // loop over all tracks


    //
    // now that we have looked at all the photons for all
    // tracks we can set the bit flags properly
    //

    for (ii=0; ii<mListOfStRichTracks.size(); ii++) {     
	StRichTrack* richTrack = mListOfStRichTracks[ii];

	StRichPidTraits* theTraits = richTrack->getPidTrait();

	if(!theTraits) {
	    cout << "StRichPIDMaker::Make()\n";
	    cout << "\tERROR Processing the PIDTraits\n";
	    cout << "\tContinuing..." << endl;
	    continue;
	}

    	if(!this->reprocessTheTraits(theTraits)) {
	    //
	    // if necessary I can take action here
	    //
	}

	//
	// fill the StTrack's StRichPidTrait with RICH PID info
	//
	StTrack* track = richTrack->getStTrack();
	if (track && richTrack->getPidTrait()) {
	    track->addPidTraits(richTrack->getPidTrait());
	}
	else {
	    cout << "StRichPIDMaker::Make()\n";
	    cout << "\tCannot fill the PidTrait to the StTrack" << endl;
	    cout << "\tContinuing..." << endl;
	}
	richTrack = 0;
    }

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
#endif

	    if(this->checkTrack(tempTrack)) {

		//
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
	if (tempEvent->primaryVertex()->daughter(ii)->geometry()->charge() < 0) {
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
    
    return mNumberOfPrimaries;
}


void StRichPIDMaker::hitFilter(const StSPtrVecRichHit* richHits,
			       StRichRingCalculator* ringCalculator) {

//      ofstream os("data.txt",ios::app);
//      os << "**PDG " <<  ringCalculator->
//  	getRing(eInnerRing)->getParticleType()->pdgEncoding() << endl;

    //
    // make sure the hits exist
    //
    if(!richHits) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tERROR no hits..." << endl;
	return;
    }

    StThreeVectorF pointOnRing;

    StRichTrack* currentTrack = ringCalculator->getRing(eInnerRing)->getTrack();
    if(!currentTrack) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tERROR no track" << endl;
	return;
    }

    StThreeVectorF trackMomentum = currentTrack->getMomentum();
//      os << "Track Momentum: " << ( abs(trackMomentum) ) << endl;

//     PR( abs(trackMomentum) );
//     PR(mThresholdMomentum);
    
    if(!checkTrackMomentum(abs(trackMomentum))) return;
    
    StRichHit* centralHit = currentTrack->getAssociatedMIP();
    if(!centralHit) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tERROR no associated MIP" << endl;
	return;
    }

    //
    // IF the 
    if( !centralHit->isSet(eMip) ) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tWARNING!\n";
	cout << "\tCentral Hit is not classified as\n";
	cout << "\ta MIP (" << centralHit->charge() << ")\n";
	cout << "\tContinuing...for now..." << endl;
	//return;
    }
    
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


//      os << "Drawing central " << central << endl;
#ifdef RICH_WITH_PAD_MONITOR 
    mPadMonitor->drawMarker(central);
#endif
    
//      os << "LOOP OVER HITS start" << endl;

#ifdef myRICH_WITH_NTUPLE
    float distHits[29];
#endif

    //
    // loop over hits
    //
    int photonNumber=0;
    const int maxIterForInitialPsi = 40;
    const int maxIterForRingPsi    = 50;
    
    StSPtrVecRichHitConstIterator hitIter;
    for (hitIter = richHits->begin();
	 hitIter != richHits->end(); hitIter++) {

	if( (*hitIter) == centralHit) continue;
	if ( !(*hitIter)->isSet(ePhotoElectron) ) continue;

	ringCalculator->clear();  
	StThreeVectorF hit = (*hitIter)->local();

	//
	// we are waiting for the database
	//
	if( hit.x()<-65 ) continue;

	//
	// OLD VARIABLE CALCULATION
	//
	innerDistance = ringCalculator->getInnerDistance(hit,innerAngle);
	outerDistance = ringCalculator->getOuterDistance(hit,outerAngle);
	meanDistance  = ringCalculator->getMeanDistance(hit,meanAngle);
	ringWidth     = ringCalculator->getRingWidth();
	double quartzPath    = ringCalculator->getMeanPathInQuartz();
	double radPath       = ringCalculator->getMeanPathInRadiator();
	
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
//  	os << "referenceLine " << referenceLine << endl;
#ifdef RICH_WITH_PAD_MONITOR 
	//mPadMonitor->drawLine(central,hit);
#endif
	//
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
//  	os << "pointOnRing" << pointOnRing << endl;

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
	//               AxBy - AyBx
	//

	int signOfTheta =
	    sign(ringPointLine.x()*referenceLine.y() - ringPointLine.y()*referenceLine.x());
	double sineTheta = abs( (ringPointLine.unit()).cross(referenceLine.unit()) ); 
//  	os << "sineTheta* (" << signOfTheta << ") " << sineTheta << endl;

	float minDistToRefLine =
	    fabs(ringPointLine.mag())* sineTheta;
// 	os << "minDistToRefLine " << minDistToRefLine << endl;

		    
	StThreeVectorF newPointOnRing;
	bool anotherIteration = true;
	bool convergence      = false;

	double step      = 1.*degree;
	double maxChange = 5.*degree;

	//
	// take the initial step according to the sign of theta
	//
	
	if(signOfTheta<0) step *= -1.;
//  	os << "initial step " << step << endl;

	int ctr = 0;
	while (anotherIteration && ctr<maxIterForInitialPsi) {

	    //
	    // make sure you are in range
	    //
// 		    if(psi>0 && psi>M_PI) {
// 			os << "Adjustment (-1)" << endl;
// 			psi -= (2.*M_PI);
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
//  		os << "StRichPIDMaker::hitFilter()\n";
//  		os << "REFRACTED AWAY" << endl;
		psi -= step;
		step *= 0.5;
		continue;
	    }

	    ringPointLine = newPointOnRing - central;

	    int signOfNewTheta =
		sign(ringPointLine.x()*referenceLine.y() -
		     ringPointLine.y()*referenceLine.x());
	    sineTheta = abs( (ringPointLine.unit()).cross(referenceLine.unit()) ); 
//  	    os << "sineNewTheta (" << signOfNewTheta << ") " << sineTheta << endl;
		    
	    double newDistToRefLine = 
		abs(ringPointLine)* sineTheta;

//  	    os << "newDistToRefLine " << newDistToRefLine << endl;
//  	    os << "sineTheta " << sineTheta << endl;

	    if(ctr > maxIterForRingPsi)
		anotherIteration = false;

	    if (newDistToRefLine<mPrecision) {
		convergence = true;
		break;
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
//  		    os << "BRAKES step " << step << endl;
		    step = (step>0) ?
			min(1.*degree,step) : max(-1.*degree,step);
		}

	    } // else
//  	    os << "step " << step << endl;

	    minDistToRefLine = newDistToRefLine;
		    
	};

	if(!convergence) continue;

	//
	// assign the point on the ring
	//
	StThreeVectorF innerRingPoint = newPointOnRing;
//  	os << "Drawing innerRingPoint " << innerRingPoint << endl;
#ifdef RICH_WITH_PAD_MONITOR 
	//mPadMonitor->drawMarker(innerRingPoint,22);
#endif
	//
	// Given an initial guess for psi, find the "line of constant psi"
	// which crosses both rings and intersects with the hit
	//
//  	os << "current best guess for psi " << psi << endl;
		
	//
	// determine the outerRingPoint with the corresponding Psi
	//
	StThreeVectorF outerRingPoint;

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

	if( (fabs(outerRingPoint.x()  > mPadPlaneDimension.x())) ||
	    (fabs(outerRingPoint.y()) > mPadPlaneDimension.y())  ||
	    (fabs(innerRingPoint.x()  > mPadPlaneDimension.x())) ||
	    (fabs(innerRingPoint.y()) > mPadPlaneDimension.y())  ) {
//  	    os << "ERROR: PANIC BAD POINT...continue" << endl;
	    continue;
	}


	//
	// the constant Psi line between the inner and outer rings
	//
	
	StThreeVectorF consPsiVector = outerRingPoint - innerRingPoint; 
//  	os << "consPsiVector " << consPsiVector << endl;
#ifdef RICH_WITH_PAD_MONITOR 
//  	mPadMonitor->drawLine(innerRingPoint,outerRingPoint);
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
	
	signOfTheta =
	    sign(consPsiRefLine.x()*consPsiVector.y() -
		 consPsiRefLine.y()*consPsiVector.x());
	sineTheta = abs( (consPsiRefLine.unit()).cross(consPsiVector.unit()) ); 
//  	os << "sineTheta2 (" << signOfTheta << ") " << sineTheta << endl;
		
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
	    // make sure you are in range
	    //
// 		    if(psi>0 && psi>M_PI) {
// 			os << "Adjustment (-2)  psi " << psi << endl;
// 			psi -= (2.*M_PI);
// 			step *= -1.;
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
		    
	    consPsiRefLine = hit - newInnerPointOnRing;
	    consPsiVector  = newOuterPointOnRing - newInnerPointOnRing; 

	    //
	    //        ^   ^
	    // | A |  A x B
	    //
	    // where:
	    //        A = consPsiRefLine
	    //        B = consPsiVector
	    
	    int signOfNewTheta =
		sign(consPsiRefLine.x()*consPsiVector.y() -
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
		break;
	    }

	    if( (signOfTheta != signOfNewTheta) ||
		( (signOfTheta == signOfNewTheta) &&
		  (newMinDistToConsPsiRefLine > minDistToConsPsiRefLine) ) ) {
		//wrong way
		signOfTheta = signOfNewTheta;
		step *= -0.5;
	    }
	    else {
		// make the step size bigger if we are moving
		// in the right direction
		step = (step>0) ?
		    min(maxChange,(step*1.2)) : max(-1.*maxChange,step*1.2);

		if(newMinDistToConsPsiRefLine < 3.*centimeter) {
//  		    os << "BRAKES(2) " << endl;
		    step = (step>0) ?
			min(1.*degree,step) : max(-1.*degree,step);
		}
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
	
	//
	// ring width
	//
	StThreeVectorF lengthOfD = newOuterPointOnRing - newInnerPointOnRing;
//  	os << "photonNumber " << photonNumber << endl;
//  	os << "abs(lengthOfD) " << abs(lengthOfD) << endl;
//  	os << "consPsiVector " << abs(consPsiVector) << endl;

	//
	// distance to hit at constant psi
	//

	StThreeVectorF hitDVector = hit - newInnerPointOnRing;

//  	os << "abs(hitDVector) " << abs(hitDVector) << endl;
//  	os << "consPsiRefLine " << consPsiRefLine << endl;
		
	int signOfD = sign(lengthOfD*hitDVector);
		
	float cosineOfD = lengthOfD.unit()*hitDVector.unit();
//  	os << "cosineOfD " << cosineOfD << endl;

	double normalizedD = signOfD * (abs(hitDVector)/abs(lengthOfD));		
//  	os << "normalizedD " <<  normalizedD << endl;

#ifdef RICH_WITH_PAD_MONITOR
	if(normalizedD>0 && normalizedD<1)
	    mPadMonitor->drawMarker(hit,29,1.2,3);
#endif


#ifdef myRICH_WITH_NTUPLE
	distHits[0] = innerRingPoint.x();
	distHits[1] = innerRingPoint.y();
	distHits[2] = outerRingPoint.x();
	distHits[3] = outerRingPoint.y();
	distHits[4] = psi;
	distHits[5] = abs(lengthOfD);
	distHits[6] = normalizedD;

	// OLD labels
	distHits[7] = olddist;
	distHits[8] = oldsigma;
	distHits[9] = meanAngle; // the old psi
	//
	// the photon
	distHits[10]  = hit.x();
	distHits[11] = hit.y();
	//
	// the MIP
	distHits[12] = central.x();
	distHits[13] = central.y();
	
	distHits[14] = trackMomentum.x();
	distHits[15] = trackMomentum.y();
	distHits[16] = trackMomentum.z();

	// Track incident angle
	//theta = acos(-pz/sqrt(px**2+py**2+pz**2))
	distHits[17] = ringCalculator->getRing(eInnerRing)->getTrack()->getTheta()/degree;
	distHits[18] = mEventN;
	
	distHits[19] = photonNumber;
	
	distHits[20] = mipResidual.x();
	distHits[21] = mipResidual.y();
	distHits[22] = abs(mipResidual);
	
	distHits[23] = particleType;
	distHits[24] = cosineOfD;
	distHits[25] = minDistToConsPsiRefLine;
	
	distHits[26] = abs(trackMomentum);
	distHits[27] = currentTrack->getStTrack()->geometry()->helix().h();
	distHits[28] = mVertexPos.z();

	distup->Fill(distHits);
#endif

	//
	// Take only photons with -1 < d < 3
	//
	//
	double sigmaOfD;
	double meanOfD;

	// First Attempt
// 	double sigmaOfD = .96;
// 	double meanOfD  = .65;

	//
	// dipAngle of the track (in degrees)
	//
// 	double trackDipAngle = atan(trackMomentum.x()/trackMomentum.z())/degree;
// 	PR(trackDipAngle);

// 	int charge, bin;
	// For helix h>0
// 	if(trackDipAngle<-10)
// 	    bin = 0;
// 	else if(trackDipAngle>-10 && trackDipAngle<-5)
// 	    bin = 1;
// 	else if(trackDipAngle>-5 && trackDipAngle<0)
// 	    bin = 2;
// 	else if(trackDipAngle>0 && trackDipAngle<5)
// 	    bin = 3;
// 	else if(trackDipAngle>5 && trackDipAngle<10)
// 	    bin = 4;
// 	else if(trackDipAngle>10)
// 	    bin = 5;

// 	if(currentTrack->getStTrack()->geometry()->helix().h() > 0)
// 	    charge = 1; // positive
// 	else
// 	    charge =0; // negative

// 	meanOfD = meanD[bin][charge];
// 	sigmaOfD = sigmaD[bin][charge];

	//
	// constant area
	//
	meanOfD  = 0.70;
 	sigmaOfD = 0.30;
	
	if( normalizedD<-1. || normalizedD>3. ) continue;

	bool inArea = false;
	if(normalizedD>=0 && normalizedD<=1 )
	    inArea = true;
	
	double sigma = (normalizedD - meanOfD)/sigmaOfD;
	//
	//**************** hitfilter *******************
	//              setting the flags
	//

	if(particleType == -211   ||   // pion
	   particleType == -321   ||   // kaon
	   particleType == -2212  ) {  // proton

	    ringCalculator->
		getRing(eInnerRing)->
		getTrack()->
		addHit((*hitIter),normalizedD,sigma,psi,radPath,quartzPath,particle);
	}
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
		if ( checkTrack(tempTrack) )  {
		    StThreeVectorF trackPredictor = tempTrack->getProjectedMIP();
		    StThreeVectorF testThisResidual;
		    StRichHit* mAssociatedMIP = 0;
		    float smallestResidual = 3.0*centimeter;
		    float adcCut = 300;

		    StSPtrVecRichHitIterator hitIter;
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
		delete tempTrack;
	    }
	}
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
//     cout << "event run id   = " << mEventN        << endl;

    mMagField    = 2.49117;    
    if (event->summary()) {
	mMagField  = event->summary()->magneticField();
	cout << "  B field = " << mMagField << endl;
    } 
    else {
	cout << "StRichPIDMaker::checkEvent().\n";
	cout << "\tWARNING!\n";
	cout << "\tCannot get B field from event->summary(). \n";
	cout << "\tUse B= " << mMagField << endl;
    } 
    	
    return true;
}

bool StRichPIDMaker::checkTrack(StTrack* track) {

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
	 track->getLastHit().perp()>mLastHitCut) {
	return true;
    }
    
    return false;
}

bool StRichPIDMaker::checkTrackMomentum(float mag) {
    if(mag<mThresholdMomentum)
	return false;

    return true;
}

bool StRichPIDMaker::checkHit(StRichHit* hit) {

    // should use mip/hit flag
    if (hit && hit->charge()<mAdcCut) {
	return true;
    }

    return false;
}

void StRichPIDMaker::fillPIDTraits(StRichRingCalculator* ringCalc) {

    //cout << "StRichPIDMaker::fillPIDTraits()" << endl;

    //
    // Preliminary checks to make sure the pointer are all there
    //


    if (!ringCalc || !ringCalc->getRing(eInnerRing)) {
	cout << "StRichPIDMaker::fillPIDTraits()\n";
	cout << "\tRingCalculator Pointer is lost\n";
	cout << "\tReturning" << endl;
	return;
    }

    //
    // grab track, particle type from Ring Calculator
    //
    StRichTrack* richTrack = ringCalc->getRing(eInnerRing)->getTrack();
    if(!richTrack) {
      cout << "StRichPIDMaker::fillPidTraits()\n";
      cout << "\tWARNING Cannot get StRichTrack:\n";
      cout << "\tReturning..." << endl;
      return;
    }

    if(!richTrack->getStTrack()) {
      cout << "StRichPIDMaker::fillPidTraits()\n";
      cout << "\tWARNING Cannot get StTrack from StRichTrack\n";
      cout << "\tReturning..." << endl;
      return;
    }

    if(!richTrack->getStTrack()->detectorInfo()) {
      cout << "StRichPIDMaker::fillPidTraits()\n";
      cout << "\tWARNING Cannot get DetectorInfo from StTrack\n";
      cout << "\tReturning..." << endl;
      return;
    }
    
    StParticleDefinition* part =
	ringCalc->getRing(eInnerRing)->getParticleType();  
    
    // used to be here
    //ringCalc->calculateArea();
      
      
    unsigned int totalHitsInArea    = 0;
    unsigned int hitsInConstantAngle = 0;
    unsigned int hitsInConstantArea = 0;
    unsigned int sig1    = 0;
    unsigned int sig2 = 0;

    //
    // if the track meets minimum criteria,
    // create and fill an StRichPid structure.
    // It is kept in a ROOT-STL container so
    // it is by pointer
    //

    if (richTrack->fastEnough(part) && richTrack->isGood(part)) {
	StRichPid* pid = new StRichPid();
	pid->setRingType(part);
//  	cout << "PARTICLE " <<  part->pdgEncoding() << endl;
// 	cout << "\tp= " << richTrack->getStTrack()->geometry()->momentum().mag() << endl;

	//
	// set the constant area of the Ring
	//
	//

	pid->setTotalArea(ringCalc->getTotalConstantArea());
	pid->setTotalAzimuth(ringCalc->getTotalConstantAngle());

	//
	// set the constant area on the active portion of pad plane
	// takes into account the edge and gap
	//
	pid->setTruncatedArea(ringCalc->getTotalConstantAreaOnActivePadPlane());
	pid->setTruncatedAzimuth(ringCalc->getTotalConstantAngleOnActivePadPlane());	
  
	
	vector<StRichRingHit*> hits = richTrack->getRingHits(part);
	
	//
	// Add the StRichHit information to the StRichPid
	// remember we keep a reference to the hit in
	// the StTrackDetectorInfo structure as well.
	//
	for (size_t i=0; i<hits.size(); i++) {

	    StRichHit* theCurrentHit = hits[i]->getHit();
	    pid->addHit(theCurrentHit);
	    richTrack->getStTrack()->detectorInfo()->addHit(theCurrentHit);

	    float normalizedD = hits[i]->getDist();
	    float sigma       = hits[i]->getNSigma();
	    float psi         = hits[i]->getAngle();

//  	    cout << "d s si csi "
//  		 << normalizedD << '\t'
//  		 << sigma << '\t'
//  		 << psi << '\t'
//  		 << ringCalc->getConstantAreaAngle() << " ";
	    
	    pid->addPhotonInfo(new StRichPhotonInfo(normalizedD, sigma, psi));

	    //
	    // boolean flags
	    //
	    bool inArea          = false;
	    bool inConstantAngle = false;
	    bool inConstantArea  = false;

	    if ( (normalizedD >= 0) && (normalizedD <= 1) ) {
		inArea = true;
		totalHitsInArea++;
	    }	    

	    pid->setConstantAreaCut(ringCalc->getConstantAreaAngle());
	    
	    if ( fabs(psi) > ringCalc->getConstantAreaAngle()) {
		inConstantAngle = true;
		hitsInConstantAngle++;
	    }

	    if(inArea && inConstantAngle) {
		inConstantArea = true;
		hitsInConstantArea++;
	    }

	    if(part == pion) {
		if( theCurrentHit->isSet(eInMultipleCAreaPi) ) continue;
		
		if( theCurrentHit->isSet(eInAreaPi) ||
		    theCurrentHit->isSet(eInConstantAnglePi) ) {
		    //
		    // it was touched by another pion ring
		    theCurrentHit->setBit(eMultiplyAssigned);
		}

		if(inArea) {
		    if( theCurrentHit->isSet(eInAreaPi) ) {
			theCurrentHit->setBit(eInMultipleAreaPi);
		    }
		    else {
			theCurrentHit->setBit(eInAreaPi);
		    }
		}

		if(inConstantAngle) {
		    if( theCurrentHit->isSet(eInConstantAnglePi) ) {
			theCurrentHit->setBit(eInMultipleCAnglePi);
		    }
		    else {
			theCurrentHit->setBit(eInConstantAnglePi);
		    }
		}

		if( inConstantAngle && !theCurrentHit->isSet(eInMultipleCAnglePi) ) {
		    if( fabs(sigma)<1 ) {theCurrentHit->setBit(e1SigmaPi);sig1++;}
		    if( fabs(sigma)<2 ) {theCurrentHit->setBit(e2SigmaPi);sig2++;}
		}
	    }

	    if(inConstantArea) {
		if( theCurrentHit->isSet(eInConstantAreaPi) ) {
		    theCurrentHit->setBit(eInMultipleCAreaPi);
		}
		else {
		    theCurrentHit->setBit(eInConstantAreaPi);
		}
	    }
	    
	    if(part == kaon) {
		if( theCurrentHit->isSet(eInMultipleCAreaK) ) continue;

		if( theCurrentHit->isSet(eInAreaK)          ||
		    theCurrentHit->isSet(eInConstantAngleK) ) {
		    //
		    // it was touched by another kaon ring
		    theCurrentHit->setBit(eMultiplyAssigned);
		}

		if(inArea) {
		    if( theCurrentHit->isSet(eInAreaK) ) {
			theCurrentHit->setBit(eInMultipleAreaK);
		    }
		    else {
			theCurrentHit->setBit(eInAreaK);
		    }
		}

		if(inConstantAngle) {
		    if( theCurrentHit->isSet(eInConstantAngleK) ) {
			theCurrentHit->setBit(eInMultipleCAngleK);
		    }
		    else {
			theCurrentHit->setBit(eInConstantAngleK);
		    }
		}
		
		if( inConstantAngle && !theCurrentHit->isSet(eInMultipleCAngleK) ) {
		    if( fabs(sigma)<1 ) {theCurrentHit->setBit(e1SigmaK);sig1++;}
		    if( fabs(sigma)<2 ) {theCurrentHit->setBit(e2SigmaK);sig2++;}
		}

		if(inConstantArea) {
		    if( theCurrentHit->isSet(eInConstantAreaK) ) {
			theCurrentHit->setBit(eInMultipleCAreaK);
		    }
		    else {
			theCurrentHit->setBit(eInConstantAreaK);
		    }
		}
		
	    }
	    
	    if(part == proton) {
		if( theCurrentHit->isSet(eInMultipleCAreap) ) continue;

		if( theCurrentHit->isSet(eInAreap)          ||
		    theCurrentHit->isSet(eInConstantAnglep) ) {

		    //
		    // it was touched by another proton ring
		    theCurrentHit->setBit(eMultiplyAssigned);
		}

		if(inArea) {
		    if( theCurrentHit->isSet(eInAreap) ) {
			theCurrentHit->setBit(eInMultipleAreap);
		    }
		    else {
			theCurrentHit->setBit(eInAreap);
		    }
		}

		if(inConstantAngle) {
		    if( theCurrentHit->isSet(eInConstantAnglep) ) {
			theCurrentHit->setBit(eInMultipleCAnglep);
		    }
		    else {
			theCurrentHit->setBit(eInConstantAnglep);
		    }
		}

		if(inConstantAngle && !theCurrentHit->isSet(eInMultipleCAnglep) ) {
		    if( fabs(sigma)<1 ) {theCurrentHit->setBit(e1Sigmap);sig1++;}
		    if( fabs(sigma)<2 ) {theCurrentHit->setBit(e2Sigmap);sig2++;}
		}

		if(inConstantArea) {
		    if( theCurrentHit->isSet(eInConstantAreap) ) {
			theCurrentHit->setBit(eInMultipleCAreap);
		    }
		    else {
			theCurrentHit->setBit(eInConstantAreap);
		    }
		}

	    }
		
// 	    if(part==pion) {
// 		cout << "pi "
// 		     << theCurrentHit->isSet(eInAreaPi)
// 		     << theCurrentHit->isSet(eInConstantAnglePi)
// 		     << theCurrentHit->isSet(eInConstantAreaPi)
// 		     << theCurrentHit->isSet(e1SigmaPi)
// 		     << theCurrentHit->isSet(e2SigmaPi) << " "
// 		     << theCurrentHit->isSet(eInMultipleAreaPi)
// 		     << theCurrentHit->isSet(eInMultipleCAnglePi)
// 		     << theCurrentHit->isSet(eInMultipleCAreaPi);
// 	    }
// 	    else if(part==kaon) {
// 		cout << "K "
// 		     << theCurrentHit->isSet(eInAreaK)
// 		     << theCurrentHit->isSet(eInConstantAngleK)
// 		     << theCurrentHit->isSet(eInConstantAreaK)
// 		     << theCurrentHit->isSet(e1SigmaK)
// 		     << theCurrentHit->isSet(e2SigmaK) << " "
// 		     << theCurrentHit->isSet(eInMultipleAreaK)
// 		     << theCurrentHit->isSet(eInMultipleCAngleK)
// 		     << theCurrentHit->isSet(eInMultipleCAreaK);
// 	    }
// 	    else if(part==proton) {
// 		cout << "p "
// 		     << theCurrentHit->isSet(eInAreap)
// 		     << theCurrentHit->isSet(eInConstantAnglep)
// 		     << theCurrentHit->isSet(eInConstantAreap)
// 		     << theCurrentHit->isSet(e1Sigmap)
// 		     << theCurrentHit->isSet(e2Sigmap) << " "
// 		     << theCurrentHit->isSet(eInMultipleAreap)
// 		     << theCurrentHit->isSet(eInMultipleCAnglep)
// 		     << theCurrentHit->isSet(eInMultipleCAreap);
// 	    }
// 	    cout << " " << theCurrentHit << endl;
	}

	pid->setTotalDensity(hitsInConstantArea/pid->getTotalArea());
	pid->setTruncatedDensity(hitsInConstantArea/pid->getTruncatedArea());
	
// 	cout << "totalHitsInArea= " << totalHitsInArea
// 	     << " hitsInConstantAngle= " << hitsInConstantAngle
// 	     << " hitsInConstantArea= "  << hitsInConstantArea << endl;
// 	cout << "sig1hit= " << sig1 << " sig2hit= " << sig2 << endl;
	
	//if (part==kaon && constantHits>2 && fabs(mVertexPos.z())<30) {
	//        if (fabs(mVertexPos.z())<30) {
	//  	  mPrintThisEvent=true;
	//        }

	pid->setTotalHits(totalHitsInArea);
	pid->setTruncatedHits(hitsInConstantArea);
 
	StThreeVectorD residual(-999.,-999.,-999.);
	if (!richTrack->getAssociatedMIP()) {
	    cout << "StRichPIDMaker::fillPIDTraits()\n";
	    cout << "\tWARNING Rich Track has no AssociatedMIP\n";
	    cout << "\tp= " << richTrack->getStTrack()->geometry()->momentum().mag() << endl;
	}
	else {
	    residual.setX(richTrack->getProjectedMIP().x()-richTrack->getAssociatedMIP()->local().x());
	    residual.setY(richTrack->getProjectedMIP().y()-richTrack->getAssociatedMIP()->local().y());
	    residual.setZ(richTrack->getProjectedMIP().z()-richTrack->getAssociatedMIP()->local().z());
	}
	
	pid->setMipResidual(residual);
	
	//
	// assign the pid to the StRichTrack
	//

	richTrack->getPidTrait()->addPid(pid);
   } 
}

bool StRichPIDMaker::reprocessTheTraits(StRichPidTraits* traits)
{ 
    //
    // get the hypothesis
    //
    const StSPtrVecRichPid& allThePids = traits->getAllPids();
    for(size_t ii=0; ii<allThePids.size(); ii++) { // thepids
	StRichPid* pid = allThePids[ii];
	if(!pid) {
	    cout << "StRichPIDMaker::reprocessTheTraits()\n";
	    cout << "\tERROR cannot get the StRichPid\n";
	    cout << "\tcontinuing..." << endl;
	    continue;
	}
	
	const StPtrVecRichHit& hit = pid->getAssociatedRichHits();

	unsigned int hitsInConstantArea = 0;
	for(size_t jj=0; jj<hit.size(); jj++) {
	    switch(pid->getParticleNumber()) {
	    case -211:
// 		cout << "pi "
// 		     << hit[jj]->isSet(eInAreaPi)
// 		     << hit[jj]->isSet(eInConstantAnglePi)
// 		     << hit[jj]->isSet(eInConstantAreaPi)
// 		     << hit[jj]->isSet(e1SigmaPi)
// 		     << hit[jj]->isSet(e2SigmaPi) << " "
// 		     << hit[jj]->isSet(eInMultipleAreaPi)
// 		     << hit[jj]->isSet(eInMultipleCAnglePi)
// 		     << hit[jj]->isSet(eInMultipleCAreaPi);
				
		if( hit[jj]->isSet(eInMultipleCAreaPi) ) continue;
		if( hit[jj]->isSet(eInConstantAreaPi) ) {
		    hitsInConstantArea++;
		}
		break;
		
	    case -321:
// 		cout << "K "
// 		     << hit[jj]->isSet(eInAreaK)
// 		     << hit[jj]->isSet(eInConstantAngleK)
// 		     << hit[jj]->isSet(eInConstantAreaK)
// 		     << hit[jj]->isSet(e1SigmaK)
// 		     << hit[jj]->isSet(e2SigmaK) << " "
// 		     << hit[jj]->isSet(eInMultipleAreaK)
// 		     << hit[jj]->isSet(eInMultipleCAngleK)
// 		     << hit[jj]->isSet(eInMultipleCAreaK);
				
		if( hit[jj]->isSet(eInMultipleCAreaK) ) continue;
		if( hit[jj]->isSet(eInConstantAreaK) ) {
		    hitsInConstantArea++;
		}
		break;

	    case -2212:
// 		cout << "p "
// 		     << hit[jj]->isSet(eInAreap)
// 		     << hit[jj]->isSet(eInConstantAnglep)
// 		     << hit[jj]->isSet(eInConstantAreap)
// 		     << hit[jj]->isSet(e1Sigmap)
// 		     << hit[jj]->isSet(e2Sigmap) << " "
// 		     << hit[jj]->isSet(eInMultipleAreap)
// 		     << hit[jj]->isSet(eInMultipleCAnglep)
// 		     << hit[jj]->isSet(eInMultipleCAreap);
				
		if( hit[jj]->isSet(eInMultipleCAreap) ) continue;
		if( hit[jj]->isSet(eInConstantAreap) ) {
		    hitsInConstantArea++;
		}
		break;

	    default:
		cout << "StRichPIDMaker::reprocessTheTraits()\n";
		cout << "\tERROR Unknown Particle Type\n";
		cout << "\tContinuing..." << endl;
	    }
// 	    cout << " " << hit[jj] << endl;
	} // loop over the hits

	if(pid->getTruncatedHits() != hitsInConstantArea) {
// 	    cout << "adjusted < (old) " << hitsInConstantArea << "(" << pid->getTruncatedHits() << ")" << endl;
	    if(hitsInConstantArea > pid->getTruncatedHits()) {
		//cout << "StRichPIDMaker::reprocessTheTraits()\n";
		//cout << "\tCANNOT HAVE MORE HITS!!!!\n";
		//cout << "\tAnother Track has touched this..." << endl;
		break;
	    }
	    
	    pid->setTruncatedHits(hitsInConstantArea);
	    pid->setTruncatedDensity(hitsInConstantArea/pid->getTruncatedArea());
	}
    } // loop over the pids

    //
    // This is the place an ID should be assigned and
    // a probability written out
    // ....in progress from the StRichSpectraMaker
    //
    return true;
}

void StRichPIDMaker::fillRichSoftwareMonitor(StEvent* evnt) {
  
  StSoftwareMonitor* monitor = evnt->softwareMonitor();
  if (monitor) {
    StRichSoftwareMonitor* richMonitor = monitor->rich();
    if (richMonitor) {
      richMonitor->setNumberOfTracksCrossing(mRichTracks);
      richMonitor->setNumberOfTracksAbove1Gev(mNumberOf1GeV);
      richMonitor->setNumberOfHitsInRings(mNumberOfRingHits);
    }
  }
}

void StRichPIDMaker::drawPadPlane(StEvent* rEvent, bool kCreatePsFile) {

#ifdef RICH_WITH_PAD_MONITOR 
  
//   StThreeVectorF VertexPos(-999,-999,-999);
//   if (rEvent->primaryVertex()) {
//     VertexPos = rEvent->primaryVertex()->position();
//   } 

   mPadMonitor = StRichPadMonitor::getInstance(mGeometryDb);
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
   //   mPadMonitor->hiLiteHits(e2SigmaK);
   //   mPadMonitor->hiLiteHits(e2Sigmap);
   mPadMonitor->drawRingInfo();
   mPadMonitor->update();  
   if (kCreatePsFile) {
     cout << "print it...." << endl;
     mPadMonitor->printCanvas("~anyDirectory",fileName,rEvent->id());
   }
#endif 
}

//
// THE Cuts
// Cut values can be set at the macro level here
// Make sure to respect units
//

void StRichPIDMaker::printCutParameters(ostream& os) const
{
    os << "==============================================" << endl;
    os << "StRichPIDMaker::printCutParameters()" << endl;
    os << "----------------------------------------------" << endl;
    os << "Event Level:" << endl;
    os << "\tVertexWindow =  " << (mVertexWindow/centimeter)  << " cm"    << endl;
    os << "Hit Level:" << endl;
    os << "\tAdcCut =        " << (mAdcCut)                               << endl; 
    os << "Track Level:" << endl;
    os << "\tPtCut =         " << (mPtCut/GeV)                << " GeV/c" << endl;
    os << "\tEtaCut =        " << mEtaCut                                 << endl;
    os << "\tLastHitCut =    " << (mLastHitCut/centimeter)    << " cm"    << endl;
    os << "\tDcaCut =        " << (mDcaCut/centimeter)        << " cm"    << endl;
    os << "\tFitPointsCut =  " << mFitPointsCut                           << endl;
    os << "\tPathCut =       " << (mPathCut/centimeter)       << " cm"    << endl;
    os << "\tPadPlaneCut =   " << (mPadPlaneCut/centimeter)   << " cm"    << endl;
    os << "\tRadiatorCut =   " << (mRadiatorCut/centimeter)   << " cm"    << endl;
    os << "\tThreshMom =     " << (mThresholdMomentum/GeV)    << " GeV/c" << endl;
    os << "----------------------------------------------" << endl;
    os << "++++++++++++++++++++++++++++++++++++++++++++++" << endl;
    os << "Convergence Precision:" << endl;
    os << "\tPrecision =     " << (mPrecision/micrometer)     << " um"    << endl;
    os << "----------------------------------------------" << endl;

//     os << "<d>  "     << "\t" << "sigma_d"    << endl;
//     os << "Negatives" << endl;
//     os << meanD[0][0] << "\t" << sigmaD[0][0] << endl;
//     os << meanD[1][0] << "\t" << sigmaD[1][0] << endl;
//     os << meanD[2][0] << "\t" << sigmaD[2][0] << endl;
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
  array[0] = mListOfStRichTracks.size();
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
	    }
	    
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
	    
	    float dist  = ((outerDistance/ringWidth > 1 && (innerDistance/ringWidth < outerDistance/ringWidth )) ? 
			   (-1.0):(1.0))*innerDistance/ringWidth;
	    
	    if (fabs(dist)<5 && fabs(outerAngle/degree)>90) { overLap->Fill(p,theta,dist,pionTag,kaonTag,innerAngle/degree);} 
	    
	  }
	}
      }

      delete pionCalculator; pionCalculator = 0;
      delete kaonCalculator; kaonCalculator = 0;
    }
  }
  return;
}



void StRichPIDMaker::fillHitNtuple(const StSPtrVecRichHit* hits,
				   const StSPtrVecRichCluster* clusters) {
  
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
      array[12] = constAngle/degree;
      geantCloseHitNtuple->Fill(array);      
    }
  }  
}
#endif


#ifdef myRICH_WITH_MC
void StRichPIDMaker::fillMcPhotonNtuple(StMcEvent* mcevent,
					   const StSPtrVecRichCluster* clusters, 
					   const StSPtrVecRichHit* richHits) {
  
  if (!mcevent || !richHits || !clusters) return;

  // need to get the g2t info  
  St_DataSet* dsGeant = GetDataSet("geant");
  if(!dsGeant || !dsGeant->GetList()) {
    dsGeant = GetDataSet("event/geant/Event");
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
    trackArray[counter++] = KaonConstantArea;
    trackArray[counter++] = KaonTotalAreaAngle;
    trackArray[counter++] = KaonConstantAreaAngle;
  
    trackArray[counter++] = KaonTotalHits;
    trackArray[counter++] = KaonConstantHits;
    trackArray[counter++] = ProtonFactor;  
    trackArray[counter++] = ProtonTotalArea;
    trackArray[counter++] = ProtonConstantArea; 

    trackArray[counter++] = ProtonTotalAreaAngle;  
    trackArray[counter++] = ProtonConstantAreaAngle;
    trackArray[counter++] = ProtonTotalHits;
    trackArray[counter++] = ProtonConstantHits; // 18 + 70 = 88
    
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

      trackArray[counter++] = KaonConstantAreaAngle;  
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
  
    








