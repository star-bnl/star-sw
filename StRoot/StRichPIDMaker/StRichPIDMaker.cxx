/******************************************************
 * $Id: StRichPIDMaker.cxx,v 2.43 2001/05/29 22:04:08 dunlop Exp $
 * 
 * Description:
 *  Implementation of the Maker main module.
 *
 * $Log: StRichPIDMaker.cxx,v $
 * Revision 2.43  2001/05/29 22:04:08  dunlop
 * Made sure to clear mListOfRichTracks, even if event doesn't pass cuts.
 * Removes segvio
 *
 * Revision 2.42  2001/05/16 20:05:05  dunlop
 * Modified to also write into StEvent high pt hits.
 *
 * Revision 2.41  2001/04/25 00:31:40  lasiuk
 * HP changes.  removal of reprocessTraits()
 *
 * Revision 2.40  2001/04/17 18:21:05  horsley
 * updated default index of refraction values, made correction to hit filter
 *
 * Revision 2.39  2001/02/22 21:10:39  lasiuk
 * remove debug output (dca)
 *
 * Revision 2.38  2001/02/22 21:06:05  lasiuk
 * fill the new StEvent structures in PidTraits, and richCollection
 * dca code now included
 *
 * Revision 2.37  2001/02/07 15:58:31  lasiuk
 * update for production (production version and StEvent changes)
 * refit and momentum loss are default behavior (Nikolai's parameterization)
 * richCollection kept as data member
 * reprocess the traits is default behavior
 * creation of PIDTraits is done earlier
 *
 * Revision 2.36  2001/02/01 17:55:29  horsley
 * set energy loss in CTB at 20 MeV (default)
 * ifdef'd out the TrackEntryClass
 * StRichTrack::fastEnough() has materialsDB input for wavelenght's
 *
 * Revision 2.35  2001/01/31 13:25:23  horsley
 * minimum  number of fitpoints = 25
 *
 * Revision 2.34  2001/01/30 22:13:10  horsley
 * trajectory correction now default, added trajectory correction comments for log file
 *
 * Revision 2.33  2001/01/30 16:38:43  horsley
 * updated PID maker for next production run, included new class for TTree
 *
 * Revision 2.32  2000/12/15 01:05:49  horsley
 * corrected distHits entry number in hitFilter
 *
 * Revision 2.31  2000/12/15 00:05:18  horsley
 * added associated Mip's charge to dist ntuple
 *
 * Revision 2.30  2000/12/14 21:09:06  horsley
 * fixed  error in filling ntuple with StTrack's momentum
 *
 * Revision 2.29  2000/12/14 19:20:48  horsley
 * added event run id to dist ntuple,
 *
 * added flag to bit mask in dist ntuple to indictae which checkTrack
 * check made entry to ntuple,
 *
 * dist ntuple has global p_vec and not local p_vec
 *
 * commented out StRichTrack pathlength check in constructor
 *
 * Revision 2.28  2000/12/08 20:09:31  horsley
 * updated monte carlo ntuples, member functions in StRichMCTrack, StRichPIDMaker
 * changed monte carlo double xCorrection = 0 in StRichTrack to xCorrection = 0
 * with no declaration of the double
 *
 * Revision 2.27  2000/12/08 14:59:41  horsley
 * thepids is now passed by reference
 * fillCorrectedNtuple now uses the particledefinition->pdgEncoding()
 * TpcHitVectorUtilites function commented out due to micro dst tpc hit
 * problem
 *
 * Revision 2.26  2000/12/08 06:32:48  lasiuk
 * fillcorrectedNtuple (ifdefs)
 *
 * Revision 2.25  2000/12/08 05:12:25  lasiuk
 * correct SUN iostream problem
 *
 * Revision 2.24  2000/12/08 04:54:56  lasiuk
 * hit filter changed for refit
 * fillCorrectedNTuple
 * energy loss
 * modify distup for PID
 *
 * Revision 2.23  2000/11/30 23:29:28  lasiuk
 * rectify constant area (pion flags)
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

#include <map>
#include <algorithm>
#ifndef ST_NO_NAMESPACES
using std::min;
using std::max;
using std::map;
using std::pair;
using std::make_pair;
using std::less;


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



#include "TObject.h"
#include "TClonesArray.h"

#ifdef myPrivateVersion 
#include "TreeEntryClasses.h"
#endif

// magnetic field map
//#include "StarCallf77.h"
//#define gufld  F77_NAME(gufld,GUFLD)
//extern "C" {void gufld(Float_t *, Float_t *);}

static const char rcsid[] = "$Id: StRichPIDMaker.cxx,v 2.43 2001/05/29 22:04:08 dunlop Exp $";

StRichPIDMaker::StRichPIDMaker(const Char_t *name, bool writeNtuple) : StMaker(name) {
  drawinit = kFALSE;
  fileName = 0;
  kWriteNtuple=writeNtuple;

  mTotalEvents = 0;
  mGoodEvents = 0;

  //
  // default version
  mProductionVersion = -999;

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
    // Track Level
    //
    mPtCut        = 0.5*GeV; // GeV/c
    mEtaCut       = 0.5; 
    mLastHitCut   = 140.0*centimeter;
    mDcaCut       = 3.0*centimeter;
    mFitPointsCut = 24;
    mPathCut      = 500*centimeter;
    mPadPlaneCut  = 1.0*centimeter;
    mRadiatorCut  = 1.0*centimeter;

    mThresholdMomentum = 0.5*GeV;

    //
    // Convergence parameter for psi determination
    //
    mPrecision = 100*micrometer;
    
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
  //
  //
  cout << "StRichPIDMaker::init()   trajectory correction turned on." << endl;
  mTrackRefit = true;


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
  mDefaultShortWave = 174.633e-7;
  mDefaultLongWave  = 217.039e-7;
  
  if ( (mDefaultShortWave != mShortWave) &&
       (mDefaultLongWave  != mLongWave) ) {
    mMaterialDb->setWavelengthRange(mDefaultShortWave,mDefaultLongWave);
  }
  
  mPadPlaneDimension = mGeometryDb->padPlaneDimension();
  mRadiatorDimension = mGeometryDb->radiatorDimension();
  
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
    
    cout << "StRichPIDMaker::Make() production= ";
    cout << this->productionVersion() << endl;

    mPrintThisEvent = false;
    mNumberOfRingHits=0;
    mTotalEvents++;


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
    // JCD 5/16/01 moved bail to later.
    bool goodRichEvent = this->checkEvent(rEvent);
    // but bail if no StEvent or vertex
    if (!rEvent) {
	cout << "StRichPIDMaker::Make()\n";
	cout << "\tWarning: No StEvent" << endl;
	return kStWarn;
    }
    if (!rEvent->primaryVertex()) {
	cout << "StRichPIDMaker::Make()\n";
	cout << "\tWarning: No vertex" << endl;
	return kStWarn;
    }


    if (goodRichEvent) {
	mGoodEvents++;
    }

    //
    // Initialize Parameters
    //

    mRichTracks        = 0;
    mNumberOfPrimaries = 0;
    mNegativePrimaries = 0;

    //
    // get hits, clusters, pixels from StEvent
    //
    //const
    const StSPtrVecRichHit*     pRichHits      = 0;
    const StSPtrVecRichCluster* pRichClusters  = 0;
    const StSPtrVecRichPixel*   pRichPixels    = 0;
    
    int myRichHits   = 0;
    int myRichPixels = 0;
    // Reorganized JCD 5/16/01.  Used to be that you would bail.
    StRichCollection* richCollection=0;
    
    if (goodRichEvent) {
	richCollection = rEvent->richCollection();
	mRichCollection = richCollection;
	if (richCollection) {
	    if (richCollection->pixelsPresent()) {
		myRichPixels = richCollection->getRichPixels().size();
		const StSPtrVecRichPixel& richPixels =
		    richCollection->getRichPixels();
		pRichPixels = &richPixels;
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
	}
    }
    else { // need to clear the list: persistent across events
	this->clearTrackList();
    }
    

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
                // Begin changes for high pt writing.  JCD 5/16/01.
		// Use a map to avoid duplication of keys.  Avoids having to sort on pointer.
#ifndef ST_NO_TEMPLATE_DEF_ARGS
		typedef map < unsigned short, StTrack*> trackKeyToTrackMapType;
#else
		typedef map < unsigned short, StTrack*, less<unsigned short>, allocator < OS_PAIR<unsigned short, StTrack* > > trackKeyToTrackMapType;
#endif
		trackKeyToTrackMapType trackKeyToTrack;

		if (goodRichEvent) {
		    
		    for (ii=0; ii<mListOfStRichTracks.size(); ++ii) {
			StTrackNode* theTrackNode = mListOfStRichTracks[ii]->getStTrack()->node();
			for (size_t jj=0; jj<theTrackNode->entries(); ++jj) {
			    
			    StTrack* currentTrack = theTrackNode->track(jj);
			    
			    if(!currentTrack->detectorInfo()) {
				cout << "StRichPIDMaker::Make()\n";
				cout << "\tWARNING: No detectorInfo()\n";
			    cout << "\tassocciated with the track.  Continuing..." << endl;
			    continue;
			    }
			    trackKeyToTrack.insert(make_pair(currentTrack->key(),currentTrack));
			    
			    break;
			}
		    }
		}
		
		// Now the global tracks that high pt wants.
		StSPtrVecTrackNode& theNodes = rEvent->trackNodes();
		for (size_t nodeIndex = 0; nodeIndex<theNodes.size(); ++nodeIndex) {
		    StTrackNode* theTrackNode = theNodes[nodeIndex];
		    for (size_t globalIndex=0; globalIndex<theTrackNode->entries(global);++globalIndex) {
			StTrack* currentTrack = theTrackNode->track(global,globalIndex);
			// Hard coded cuts on global pt and eta
			if (currentTrack->geometry()->momentum().perp()>=1.5 
			    && 
			    fabs(currentTrack->geometry()->momentum().pseudoRapidity())<1.
			    &&
			    currentTrack->flag()>=0) {
			    if(!currentTrack->detectorInfo()) {
				cout << "StRichPIDMaker::Make()\n";
				cout << "\tWARNING: No detectorInfo()\n";
				    cout << "\tassocciated with the track.  Continuing..." << endl;
				    continue;
			    }
			    trackKeyToTrack.insert(make_pair(currentTrack->key(),currentTrack));
			
			    break;
			}
		    }
		}
		
		cout << "StRichPIDMaker::Make(): Adding hits to " << trackKeyToTrack.size() << " tracks" << endl;
		cout << "\t " << mListOfStRichTracks.size() << " Rich Tracks " << endl;
		

// This can be made more efficient if necessary with multimaps.
		
		for(trackKeyToTrackMapType::const_iterator titer= trackKeyToTrack.begin(); 
		    titer!=trackKeyToTrack.end(); ++titer) {
		    
		    StTrack* currentTrack = (*titer).second;
		    
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
		} // loop over the map entries
		delete theEvtManager;
		theEvtManager=0;
	    } // else Got 'em
	} // else 
    } // check on the tpcHitCollection

    //
    // The StTpcHitCollection should exist at this
    // point whether it is created by the PIDMaker
    // or not
 
   // Now bail.  JCD 5/16/2001
    if (!goodRichEvent) return kStWarn;
    if (!richCollection) {
	cout << "StRichPIDMaker::Make()\n";
	cout << "\tERROR: Cannot get richCollection from StEvent\n";
	cout << "\tSkip Event" << endl;
	return kStWarn;
    }
    
       
    
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

	if (mTrackRefit) richTrack->correctTrajectory();

	//
	// for the refit assign the residual.  Note
	// there is no energy loss associated with
	// this residual.  It would require us to
	// assume a PID.  This will be done later in
	// the StRichPid structure
	//
	
	StThreeVectorF refitResidual(-999.,-999.,-999.);

	if(richTrack->getAssociatedMIP()) {
	    refitResidual = richTrack->getProjectedMIP() - richTrack->getAssociatedMIP()->local();
	}
	richTrack->getPidTrait()->setRefitResidual(refitResidual);
	
	if(richTrack->getStTrack()->detectorInfo())
	    tpcHits = richTrack->getStTrack()->detectorInfo()->hits(kTpcId);

	//
	// This is the place for the track refit to be
	// done using the tpcHits
	//
	//  unsigned short numberOfTpcHits = tpcHits.size();
	//  cout << "We have " << numberOfTpcHits 
	// << " TPC hits to work with" << endl;
	//  for(size_t zz=0; zz<numberOfTpcHits; zz++)
	//   cout << "\t" << zz << " " << tpcHits[zz]->position()
	// 	   << " row " << dynamic_cast<StTpcHit*>(tpcHits[zz])->padrow() 
	//          << endl;

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
	    
	    //
	    // calculate all areas, angles using default input parameters
	    // gap correction on, no angle cut 
	    // (but the constant area is still ok) and 
	    // number of points used in calculation == 3600
	    //
	    ringCalc->calculateArea();

	    
	    //
	    // assign the points to the pi/K/p rings
	    //
	    this->hitFilter(pRichHits,ringCalc);


	    //
	    // fill PID traits
	    //
	    
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
    
    
    //
    // Original reprocssing of the hit flags to
    // make sure the multiply used bit is set properly
    // and uniformly
    // For the refit it is not needed (or desired)
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
      
      
      //
      // reprocessTheTraits is commented out for now
      //
//       if(!this->reprocessTheTraits(theTraits)) {
// 	  //
// 	  // if necessary I can take action here
// 	  //
// 	  cout << "StRichPIDMaker::Make()\n";
// 	  cout << "\treprocessTheTraits() failed." << endl;
//        }
      
      //
      // fill the StTrack's StRichPidTrait with RICH PID info
      //
      StTrack* track = richTrack->getStTrack();
      if (track) {

	//
	// add pid
	//
	track->addPidTraits(richTrack->getPidTrait());


#ifdef myPrivateVersion	
	//
	// here i will fill the tree
	//
	m_Track->addTrackInfo(richTrack,rEvent,mMaterialDb,
			      mListOfStRichTracks.size(),mNegativePrimaries);
	
	// pion
	vector<StRichRingHit*> piHits = richTrack->getRingHits(pion);
	for (int i=0;i<piHits.size();i++) {
	  HitEntry tempHit(piHits[i]->getHit()->local().x(),piHits[i]->getHit()->local().y(),
			   piHits[i]->getDist(), piHits[i]->getNSigma(),
			   piHits[i]->getAngle()/degree,
			   pion->pdgEncoding(),piHits[i]->getHit()->charge(),
		       (*pRichClusters)[piHits[i]->getHit()->clusterNumber()]->numberOfPads(),
			   piHits.size());
	  m_Track->addHit(tempHit);
	}
	
	// kaon
	vector<StRichRingHit*> kaHits = richTrack->getRingHits(kaon);
	for (int i=0;i<kaHits.size();i++) {
	  HitEntry tempHit(kaHits[i]->getHit()->local().x(),kaHits[i]->getHit()->local().y(),
			   kaHits[i]->getDist(), kaHits[i]->getNSigma(),
			   kaHits[i]->getAngle()/degree,
			   kaon->pdgEncoding(),kaHits[i]->getHit()->charge(),
		       (*pRichClusters)[kaHits[i]->getHit()->clusterNumber()]->numberOfPads(),
			   kaHits.size());
	  m_Track->addHit(tempHit);
	}
	
	
	// proton
	vector<StRichRingHit*> prHits = richTrack->getRingHits(proton);
	for (int i=0;i<prHits.size();i++) {
	  HitEntry tempHit(prHits[i]->getHit()->local().x(),prHits[i]->getHit()->local().y(),
			   prHits[i]->getDist(), prHits[i]->getNSigma(),
			   prHits[i]->getAngle()/degree,
			   proton->pdgEncoding(),prHits[i]->getHit()->charge(),
		      (*pRichClusters)[prHits[i]->getHit()->clusterNumber()]->numberOfPads(),
			   prHits.size());
	  m_Track->addHit(tempHit);
	}
	
	myTree->Fill();
	m_Track->clear();
#endif		
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
#ifdef RICH_WITH_PAD_MONITOR    
    this->drawPadPlane(rEvent,mPrintThisEvent);
#endif
    
    this->fillRichSoftwareMonitor(rEvent);
    
    
#ifdef myRICH_WITH_MC
    if (kWriteNtuple) this->fillMcTrackNtuple(pRichClusters);
    if (kWriteNtuple) this->fillMcPixelNtuple(pRichPixels);
    if (kWriteNtuple) this->fillMcPhotonNtuple(mEvent,pRichClusters,pRichHits);
    if (kWriteNtuple) this->fillGeantHitNtuple();
#endif
    
    return kStOK;
}


Int_t StRichPIDMaker::Finish() {
    
    cout << "StRichPIDMaker::Finish()" << endl;
    this->printCutParameters();

#ifdef  myRICH_WITH_NTUPLE 
    if(file) {
      cout << "StRichPIDMaker::Finish()  writing file ...." << endl;
      file->Write();
      file->Close();
      delete file;
      file = 0;
      cout << "StRichPIDMaker::Finish()  done writing file." << endl;
    }
#endif
    
    cout << "Total Number of Events = " << mTotalEvents << endl;
    cout << "Total Number of Events that passed cuts  = " << mGoodEvents << endl;
    
    return kStOK;
}

void StRichPIDMaker::setTrackRefit(bool refit) {mTrackRefit = refit;}

void StRichPIDMaker::clearTrackList() {
    
    for (size_t trackIndex=0; trackIndex<mListOfStRichTracks.size(); trackIndex++) {
	delete mListOfStRichTracks[trackIndex];
	mListOfStRichTracks[trackIndex] = 0;
    }
    mListOfStRichTracks.clear();
    mListOfStRichTracks.resize(0);
}

Int_t StRichPIDMaker::fillTrackList(StEvent* tempEvent, 
				    const StSPtrVecRichHit* richHits) {
  
    mNumberOf1GeV=0;
    this->clearTrackList();
    
    mNumberOfPrimaries = tempEvent->primaryVertex()->numberOfDaughters();  
    
    for (int ii=0; ii<mNumberOfPrimaries; ii++)  {

	StTrack* track = tempEvent->primaryVertex()->daughter(ii);

	if (this->checkTrack(track)) {
	    
#ifdef myRICH_WITH_MC    
	    StRichMCTrack* tempTrack = new StRichMCTrack(track,mMagField);
	    cout << "StRichPIDMaker::fillTrackList()  -->  creating StMcTrack!" 
		 << endl;
#else 
	    StRichTrack* tempTrack   = new StRichTrack(track,mMagField);
#endif
	    //
	    // we need theGlobalTrack to calculate the signed dca
	    //
	    StTrack* theGlobalTrack = track->node()->track(global);
	    
	    if(this->checkTrack(tempTrack)) {
		
		//
		// set the eAssociatedMIP flag
		//
		tempTrack->assignMIP(richHits);
		
		if (tempTrack->getMomentum().mag()>1)
		    mNumberOf1GeV++;	
		
		//
		// Create an StRichPidTrait and fill the information
		// - productionNumber
		// - associatedMip Pointer
		// - associatedMip Residual
		//
		
		StRichPidTraits* theRichPidTraits = new StRichPidTraits();
		
		StRichHit*       theAssociatedMip = 0;
		StThreeVectorD   theMipResidual(-999.,-999.,-999.);
		
		if (!tempTrack->getAssociatedMIP()) {
		    cout << "StRichPIDMaker::fillTrackList()\n";
		    cout << "\tWARNING Rich Track has no AssociatedMIP\n";
		    cout << "\tp= "
			 << tempTrack->getStTrack()->geometry()->momentum().mag()
			 << endl;
		}
		else {
		    theAssociatedMip = tempTrack->getAssociatedMIP();
		    theMipResidual =
			tempTrack->getProjectedMIP() - theAssociatedMip->local();
		}

		theRichPidTraits->setProductionVersion(mProductionVersion);

		theRichPidTraits->setAssociatedMip(theAssociatedMip);
		theRichPidTraits->setMipResidual(theMipResidual);

		double signed3dDca = -999.;
		double signed2dDca = -999.;
		
		if(theGlobalTrack) {
		    signed3dDca = this->calculateSignedDca(theGlobalTrack, &signed2dDca);
		}
// 		PR(signed2dDca);
// 		PR(signed3dDca);
		
		theRichPidTraits->setSignedDca2d(signed2dDca);
		theRichPidTraits->setSignedDca3d(signed3dDca);
		
		mRichCollection->addTrack(tempTrack->getStTrack());

		//
		// Add the StRichPidTrait to the StRichTrack
		// ...this will be passed to StEvent
		// (NO RESPONSIBILITY FOR MEMORY)
		// This will show up as a memory leak
		//
		
		tempTrack->addPidTrait(theRichPidTraits);

		//
		// energy loss for CTB
		//
		tempTrack->setMomentumLoss();

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

    cout << "StRichPIDMaker::trackList=" << mListOfStRichTracks.size() << endl;
    return mNumberOfPrimaries;
}


Float_t StRichPIDMaker::calculateSignedDca(const StTrack* track, double* dca2d)  {

    //
    // Calculation of the signed dca to the vertex
    // -- the 3d value is the function return value
    // -- the 2d value is returned by pointer
    //
    
    StHelixD aHelix = track->geometry()->helix();

    double dca3d = aHelix.distance(mVertexPos);

    //
    // now determine the sign
    //
    StThreeVectorD circleCenter(aHelix.xcenter(), aHelix.ycenter(), 0.);

    StThreeVectorD vertex2d(mVertexPos.x(), mVertexPos.y(), 0.);

    double distanceToCircleCenterFromVertex =
	abs(circleCenter - vertex2d);
	
    *dca2d =
	(1./aHelix.curvature() - distanceToCircleCenterFromVertex);

    double signOfDca = sign((*dca2d));

    return (signOfDca * dca3d);
}

void StRichPIDMaker::hitFilter(const StSPtrVecRichHit* richHits,
			       StRichRingCalculator* ringCalculator) {

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
    
    if(!checkTrackMomentum(abs(trackMomentum))) return;
    
    StRichHit* centralHit = currentTrack->getAssociatedMIP();
    if(!centralHit) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tERROR no associated MIP" << endl;
	return;
    }

    //
    // Possible to take action if the central
    // hit is not classified as a MIP
    //
    if( !centralHit->isSet(eMip) ) {
	cout << "StRichPIDMaker::hitFilter()\n";
	cout << "\tWARNING!\n";
	cout << "\tCentral Hit is not classified as\n";
	cout << "\ta MIP q=(" << centralHit->charge() << ")\n";
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
	( central - currentTrack->getProjectedMIP() ); 

    StParticleDefinition* particle =
	ringCalculator->getRing(eInnerRing)->getParticleType();
    int particleType = particle->pdgEncoding();
//      os << "particleType " << particleType << endl;


//      os << "Drawing central " << central << endl;
#ifdef RICH_WITH_PAD_MONITOR 
    mPadMonitor->drawMarker(central);
#endif
    
//      os << "LOOP OVER HITS start" << endl;

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
	// Disregard any hit that is in the first column (FENCE POST)
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
        // os << "current best guess for psi " << psi << endl;
		
	//
	// determine the outerRingPoint with the corresponding Psi
	//
	StThreeVectorF outerRingPoint;

	int iterationNumber = 0;
	double littleStep   = 1.*degree;
	double modifiedPsi = psi;
	if(modifiedPsi < 0) littleStep *= -1.;
	
	while (iterationNumber<maxIterForInitialPsi) {
	  iterationNumber++;
	  ringCalculator->getRing(eOuterRing)->getPoint(modifiedPsi,outerRingPoint);

	  // ---------> fix brackets	  
	  if ( (outerRingPoint.x() == FLT_MAX) ||
	       ( 
		fabs(outerRingPoint.x()) > mGeometryDb->padPlaneDimension().x() ||
		fabs(outerRingPoint.y()) > mGeometryDb->padPlaneDimension().y() 
		) 
	       ) {
	    //  os << "An OuterRingPoint DOES NOT EXIST with this psi..( "
	    //      << iterationNumber << ")" << endl;
	    modifiedPsi+=littleStep;
	  }
	  else {
	    // we found a point
	    break;
	  }
	}
	
	//  	os << "old psi " << psi << endl;
	psi = modifiedPsi;
//  	os << "new (starting) psi " << psi << endl;

	ringCalculator->getRing(eInnerRing)->getPoint(psi,innerRingPoint);
	ringCalculator->getRing(eOuterRing)->getPoint(psi,outerRingPoint);



	// ----------->  fix brackets
	if ( (fabs(outerRingPoint.x()) > mPadPlaneDimension.x()) || 
	     (fabs(outerRingPoint.y()) > mPadPlaneDimension.y())  ||
	     (fabs(innerRingPoint.x()) > mPadPlaneDimension.x()) ||
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
	meanOfD  = 0.5;
 	sigmaOfD = 0.25;
	
	if( normalizedD<-3. || normalizedD>6. ) continue;

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
    mEventRunId = event->runId();


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

// bool StRichPIDMaker::checkTrack(StTrack* track) {

//     if (track &&
// 	track->flag()>=0 &&
// 	track->geometry() && 
// 	(track->geometry()->helix().distance(mVertexPos)<mDcaCut) &&
// 	(track->fitTraits().numberOfFitPoints(kTpcId) >= mFitPointsCut) &&
// 	(fabs(track->geometry()->momentum().pseudoRapidity()) < mEtaCut) &&
// 	(track->geometry()->momentum().perp() > mPtCut)) {
  
// 	return true;
//     }
  
//     return false;
// }

bool StRichPIDMaker::checkTrack(StTrack* track) {

    bool status = true;
    float bitmask = 0;
    if (track->flag()<0) {
	status = false;
	bitmask += pow(2.,0);
    }
    
    if (!track->geometry()) {
	status = false;
	bitmask += pow(2.,1);
    }
    
    if (track->geometry()->helix().distance(mVertexPos) > mDcaCut) {
	status = false;
	bitmask += pow(2.,2);
    }
    
    if (track->fitTraits().numberOfFitPoints(kTpcId) < mFitPointsCut) {
	status = false;
	bitmask += pow(2.,3);
    }
    
    if (fabs(track->geometry()->momentum().pseudoRapidity()) > mEtaCut) {
	status = false;
	bitmask += pow(2.,4);
    }
    
    if (track->geometry()->momentum().perp() < mPtCut) {
	status = false;
	bitmask += pow(2.,5);
    }
    //cout << "StRichPIDMaker:checkTrack()\n";
    //cout << "\tbitmask = " << bitmask << endl;

#ifdef myRICH_WITH_NTUPLE


#ifdef myRICH_WITH_MC
  float distHits[35];
#else
  float distHits[34];
#endif

  distHits[0] = 0;
  distHits[1] = 0;
  distHits[2] = 0;
  distHits[3] = 0;
  distHits[4] = 0;
  distHits[5] = 0;
  distHits[6] = 0;
  
  // OLD labels
  distHits[7] = 0;
  distHits[8] = 0;
  distHits[9] = 0;
  //
  // the photon
  distHits[10] = 0;
  distHits[11] = 0;
  //
  // the MIP
  distHits[12] = 0;
  distHits[13] = 0;
  

  distHits[14] = track->geometry()->momentum().x();
  distHits[15] = track->geometry()->momentum().y();
  distHits[16] = track->geometry()->momentum().z();
  
  // Track incident angle
  //theta = acos(-pz/sqrt(px**2+py**2+pz**2))
  distHits[17] = 0;
  distHits[18] = mEventN;
  
  distHits[19] = -2.0;
  
  distHits[20] = 0;
  distHits[21] = 0;
  distHits[22] = bitmask;
  
  distHits[23] = 0;
  distHits[24] = 0;
  distHits[25] = 0;
  
  distHits[26] = track->geometry()->momentum().mag();
  distHits[27] = track->geometry()->charge();
  distHits[28] = mVertexPos.z();
  distHits[29] = 0;
  distHits[30] = 0;
  distHits[31] = 0;
  distHits[32] = mEventRunId;
  distHits[33] = 0;
#ifdef myRICH_WITH_MC
  distHits[34] = 0;
#endif  

  distup->Fill(distHits);

#endif
  
  return status;
}

bool StRichPIDMaker::checkTrack(StRichTrack* track) {

    bool status = true;
    StThreeVectorF extrapolatedPosition = track->getProjectedMIP();
    StThreeVectorF impactPoint          = track->getImpactPoint();
    
    float bitmask = 0;
    if (fabs(extrapolatedPosition.x()) > (mPadPlaneDimension.x() - mPadPlaneCut)) {
	status = false;
	bitmask += pow(2.,6);
    }

    if ( fabs(extrapolatedPosition.y()) > (mPadPlaneDimension.y() - mPadPlaneCut) ) {
	status = false;
	bitmask += pow(2.,7);
    }
  
    if (fabs(extrapolatedPosition.x()) < (mPadPlaneCut) ) {
	status = false;
	bitmask += pow(2.,8);
    }

    if (fabs(extrapolatedPosition.y()) < (mPadPlaneCut) ) {
	status = false;
	bitmask += pow(2.,9);
    }
  
    if (fabs(impactPoint.x()) > (mRadiatorDimension.x() - mRadiatorCut) ) {
	status = false;
	bitmask += pow(2.,10);
    }

    if (fabs(impactPoint.y()) > (mRadiatorDimension.y() - mRadiatorCut) ) {
	status = false;
	bitmask += pow(2.,11);
    }

    if (fabs(impactPoint.x()) < (mRadiatorCut)) {
	status = false;
	bitmask += pow(2.,12);
    }

    if (fabs(impactPoint.y()) < (mRadiatorCut)) {
	status = false;
	bitmask += pow(2.,13);
    }

    if (track->getPathLength()<0 || track->getPathLength()>mPathCut/centimeter) {
	status = false;
	bitmask += pow(2.,14);
    }
	 
    if (track->getLastHit().perp()<mLastHitCut) { 
	status = false;
	bitmask += pow(2.,15);
    }

    //cout << "StRichPIDMaker::checkTrack(St)\n";
    //cout << "\tbitmask = " << bitmask << endl;

#ifdef myRICH_WITH_NTUPLE

#ifdef myRICH_WITH_MC
  float distHits[35];
#else
  float distHits[34];
#endif


  float amipq = -999;
  if (track->getAssociatedMIP()) {
    amipq = track->getAssociatedMIP()->charge();
  }
  
  distHits[0] = 0;
  distHits[1] = 0;
  distHits[2] = 0;
  distHits[3] = 0;
  distHits[4] = 0;
  distHits[5] = 0;
  distHits[6] = 0;
  
  // OLD labels
  distHits[7] = 0;
  distHits[8] = 0;
  distHits[9] = 0;
  //
  // the photon
  distHits[10] = 0;
  distHits[11] = 0;
  //
  // the MIP
  distHits[12] = track->getProjectedMIP().x();
  distHits[13] = track->getProjectedMIP().y();
  
  distHits[14] = track->getStTrack()->geometry()->momentum().x();
  distHits[15] = track->getStTrack()->geometry()->momentum().y();
  distHits[16] = track->getStTrack()->geometry()->momentum().z();
  
  // Track incident angle
  //theta = acos(-pz/sqrt(px**2+py**2+pz**2))
  distHits[17] = 0;
  distHits[18] = mEventN;
  
  distHits[19] = -1.0;
  
  distHits[20] = 0;
  distHits[21] = 0;
  distHits[22] = bitmask;
  
  distHits[23] = 0;
  distHits[24] = 0;
  distHits[25] = 0;
  
  distHits[26] = track->getMomentum().mag();
  distHits[27] = track->getStTrack()->geometry()->charge();
  distHits[28] = mVertexPos.z();
  distHits[29] = 0;
  distHits[30] = 0;
  distHits[31] = 0;
  distHits[32] = mEventRunId;
  distHits[33] = amipq;
  
  
#ifdef myRICH_WITH_MC
  distHits[34] = 0;
#endif


  distup->Fill(distHits);
#endif


  return status;
  //
  // Old code to be reimplemented when
  // the above histogram is removed
  //
//     //
//     // values for the pad plane and radiator dimension should
//     // be kept as data members to avoid the dereferencing
//     //
//     if ( fabs(extrapolatedPosition.x()) < (mGeometryDb->padPlaneDimension().x() - mPadPlaneCut) &&
// 	 fabs(extrapolatedPosition.y()) < (mGeometryDb->padPlaneDimension().y() - mPadPlaneCut) &&
// 	 fabs(extrapolatedPosition.x()) > (mPadPlaneCut) &&
// 	 fabs(extrapolatedPosition.y()) > (mPadPlaneCut) &&
// 	 fabs(impactPoint.x()) < (mGeometryDb->radiatorDimension().x() - mRadiatorCut) &&
// 	 fabs(impactPoint.y()) < (mGeometryDb->radiatorDimension().y() - mRadiatorCut) &&
// 	 fabs(impactPoint.x()) > (mRadiatorCut) &&
// 	 fabs(impactPoint.y()) > (mRadiatorCut) &&
// 	 (track->getPathLength()>0 && track->getPathLength()<mPathCut) &&
// 	 track->getLastHit().perp()>mLastHitCut) {
// 	return true;
//     }
    
//     return false;
}

bool StRichPIDMaker::checkTrackMomentum(float mag) {
    if(mag<mThresholdMomentum)
      return false;
    
    return true;
}


bool StRichPIDMaker::checkHit(StRichHit* hit) {
  //
  // should use mip/hit flag
  //
  if (hit && hit->charge()<mAdcCut) {return true;}
  return false;
}




void StRichPIDMaker::fillPIDTraits(StRichRingCalculator* ringCalc) {

    //cout << "StRichPIDMaker::fillPIDTraits()" << endl;

    //
    // Preliminary checks to make sure the pointer are all there
    // 
    if (!ringCalc) {
	cout << "StRichPIDMaker::fillPIDTraits()\n";
	cout << "\tRingCalculator Pointer is lost\n";
	cout << "\tReturning" << endl;
	return;
    }
    
    if (!ringCalc->getRing(eInnerRing)) {
	cout << "StRichPIDMaker::fillPIDTraits()\n";
	cout << "\tRingCalculator lost inner ring\n";
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
    if ( richTrack->fastEnough(part) &&
	 richTrack->isGood(part) )      {
	
	StRichPid* pid = new StRichPid();
	pid->setRingType(part);
	
	//
	// set the constant area of the Ring
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
	for (size_t i=0; i<hits.size(); i++) { // loop over track's  hits 
	    
	    StRichHit* theCurrentHit = hits[i]->getHit();
	    pid->addHit(theCurrentHit);
	    richTrack->getStTrack()->detectorInfo()->addHit(theCurrentHit);
	    
	    float normalizedD = hits[i]->getDist();
	    float sigma       = hits[i]->getNSigma();
	    float psi         = hits[i]->getAngle();
	    //
	    // do we need the hit's adc, npads, (x,y) ?
	    //
	    
	    pid->addPhotonInfo(new StRichPhotonInfo(normalizedD, sigma, psi));
	    
	    //
	    // boolean flags
	    //
	    bool inArea             = false;
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
	    
	    if(part == pion) {  // pion
		
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
		
		
		if(inConstantArea) {
		    if( theCurrentHit->isSet(eInConstantAreaPi) ) {
			theCurrentHit->setBit(eInMultipleCAreaPi);
		    }
		    else {
			theCurrentHit->setBit(eInConstantAreaPi);
		    }
		}
		
	    } // end of pion
	    
	    
	    if(part == kaon) { // kaon
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
		
	    } // end of kaon
	    
	    if(part == proton) {  // proton
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
		
	    } // end of proton
	    
	    
	    
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
    
	} // end of hits loop



	pid->setTotalDensity(hitsInConstantArea/pid->getTotalArea());
	pid->setTruncatedDensity(hitsInConstantArea/pid->getTruncatedArea());
      
	pid->setTotalHits(totalHitsInArea);
	pid->setTruncatedHits(hitsInConstantArea);
      
	StThreeVectorD residual(-999.,-999.,-999.);
	if (!richTrack->getAssociatedMIP()) {
	    cout << "StRichPIDMaker::fillPIDTraits()\n";
	    cout << "\tWARNING Rich Track has no AssociatedMIP*\n";
	  cout << "\tp= " << richTrack->getStTrack()->geometry()->momentum().mag() << endl;
	}
	else {
	    residual = richTrack->getProjectedMIP() - richTrack->getAssociatedMIP()->local();
	}
	
	//
	// This residual is specifically for the refit
	// when the particle species energy/momentum loss
	// is used.
	// The "generic" residual is stored in the PidTrait
	// and is the same for all Pids
	//
	pid->setMipResidual(residual);
      
	//
	// assign the pid to the StRichTrack
	//    
	
	richTrack->getPidTrait()->addPid(pid);
	
    } // end of track check 
    
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
	    //cout << " " << hit[jj] << endl;
	} // loop over the hits

	if(pid->getTruncatedHits() != hitsInConstantArea) {
 	    //cout << "adjusted < (old) " << hitsInConstantArea;
	    //cout << "(" << pid->getTruncatedHits() << ")" << endl;
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
   //mPadMonitor->hiLiteHits(eInAreaPi,pion);
   //mPadMonitor->hiLiteHits(eInAreaK,kaon);
   mPadMonitor->hiLiteHits(eInAreap,proton);
   //mPadMonitor->hiLiteHits();
   //mPadMonitor->hiLiteHits(e2SigmaPi,pion);
   //   mPadMonitor->hiLiteHits(e2SigmaK,kaon);
   mPadMonitor->hiLiteHits(e2Sigmap,proton);

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
      float gtheta1,gtheta2;
      float thetap1,thetap2;
      float x1,y1,x2,y2;

      for (int i=0;i<tempHits.size();i++) {
	for (int j=i+1;j<tempHits.size();j++) {

	  getGeantPhotonInfo(richMcTrack,tempHits[i]->parentTrack(),wave1,psi1,z1,gtheta1,thetap1);
	  getGeantPhotonInfo(richMcTrack,tempHits[j]->parentTrack(),wave2,psi2,z2,gtheta2,thetap2);

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
      float mcWave,mcPsi,mcZ,mcTheta,mcPhotTheta;
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
	  
	  mcWave = defaultValue; mcPsi = defaultValue; 
	  mcZ = defaultValue; mcTheta = defaultValue;
	  mcPhotTheta = defaultValue;
	  getGeantPhotonInfo(richMcTrack,theHitsStMcTrack,mcWave,mcPsi,mcZ,mcTheta,mcPhotTheta);
	  StThreeVectorF geantRichHit = getTheGeantHitOnPadPlane(theHitsStMcTrack,monteCarloRichHit->local()); 
	  
	  Float_t photonArray[54];
	  
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
	  
	  photonArray[52]=mcTheta/degree;
	  photonArray[53]=mcPhotTheta/degree;
	   
	  richMcTrack->useTPCInfo();
	  geantPhotonNtuple->Fill(photonArray);
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
    
 
    cout << "geant track ntuple" << endl;
   StRichPidTraits* pidTrait = track->getPidTrait();
    if (pidTrait) {
      
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
    
    //trackArray[counter++] = track->getUnCorrectedImpactPoint().x();  
    //trackArray[counter++] = track->getUnCorrectedImpactPoint().y();


    //
    // changed this dec 3, 2000
    //
    trackArray[counter++] = track->getGeantMomentumAtPadPlane().perp();
    trackArray[counter++] = track->getGeantMomentumAtPadPlane().z();
    
    

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
    trackArray[counter++] = mMaterialDb->indexOfRefractionOfC6F14At(mMaterialDb->innerWavelength());
    trackArray[counter++] = mMaterialDb->indexOfRefractionOfC6F14At(mMaterialDb->outerWavelength());
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
    
    cout << "writing out the ntuple" << endl;
    geantTrackNtuple->Fill(trackArray);
    }
  } 
}
#endif





void StRichPIDMaker::initNtuples() {

#ifdef  myRICH_WITH_NTUPLE
  char finalname[200];
  sprintf(finalname,"%s.root",mySaveDirectory);
  file = new TFile(finalname,"RECREATE");
  file->SetFormat(1);
  file->SetCompressionLevel(9);
  
#ifdef myPrivateVersion	
  cout << "StRichPIDMaker::initNtuples   --> creating mStRichUstTrack. " << endl;
  m_Track = new TrackEntry();  
  myTree = new TTree("myTree","rich pid analysis tree");
  myTree->SetAutoSave(10000000);
  int bufsize = 2.0*64000;
  int split = 1;
  myTree->Branch("TrackBranch","TrackEntry",&m_Track,bufsize,split);
#endif


  
#ifdef myRICH_WITH_MC
    distup = new TNtuple("dist","b","xi:yi:xo:yo:si:ld:d:oldd:oldsig:oldsi:phx:phy:x:y:px:py:pz:theta:evt:numb:resx:resy:res:ring:cos:d2siline:p:q:vtx:refit:constang:energyloss:runid:amipq:gid");
#else
    distup = new TNtuple("dist","b","xi:yi:xo:yo:si:ld:d:oldd:oldsig:oldsi:phx:phy:x:y:px:py:pz:theta:evt:numb:resx:resy:res:ring:cos:d2siline:p:q:vtx:refit:constang:energyloss:runid:amipq");
#endif




#ifdef myRICH_WITH_MC   
    geantTrackNtuple = new TNtuple("geantTrackNtuple","geant trackwise tuple",
				 "evtn:nprimaries:nnegprimaries:vz:nrichtracks:globalpx:globalpy:globalpz:localpx:localpy:localpz:eta:q:amipid:amipproc:amipq:amipx:amipy:amipnpads:pmipx:pmipy:gmipid:gmipproc:gmipq:gmipx:gmipy:gmipnpads:radx:rady:gPtp:gPz:olocalpx:olocalpy:olocalpz:firstrow:lastrow:lasthitx:lasthity:lasthitz:lasthitdca:pathlength:maxchain:maxgap:tpchits:tpcfitpoints:innerwave:outerwave:glocalpx:glocalpy:glocalpz:gradx:grady:geantmipx:geantmipy:gstopvertx:gstopverty:gstopvertz:gphots:grecophots:gradhits:gtpccommonhits:gglobalpx:gglobalpy:gglobalpz:gid:gstopproc:gnpartners:pionfactor:piontotalarea:pionconstarea:piontotalangle:pionconstangle:piontotalhits:pionconsthits:kaonfactor:kaontotalarea:kaonconstarea:kaontotalangle:kaonconstangle:kaontotalhits:kaonconsthits:protonfactor:protontotalarea:protonconstarea:protontotalangle:protonconstangle:protontotalhits:protonconsthits");

    geantPhotonNtuple = new TNtuple("geantPhotonNtuple","geant photon wise tnuple","q:localpx:localpy:localpz:radx:rady:gradx:grady:pmipx:pmipy:amipid:amipproc:amipq:amipx:amipy:amipnpads:gamipid:gamipproc:gamipq:gamipx:gamipy:gamipnpads:gmipx:gmipy:glocalpx:glocalpy:glocalpz:theta:gtheta:phi:gphi:gid:signal:gwave:gpsi:gz:gproc:x:y:gx:gy:gmass:constangle:trdist:trang:tgdist:tgang:gconstangle:grdist:grang:ggdist:ggang:gcher:gphottheta");
  
    geantPixelNtuple = new TNtuple("geantPixelNtuple","pixels","adc:n:gid0:gid1:gid2:gid3:q0:q1:q2:q3:proc0:proc1:proc2:proc3");
    geantCloseHitNtuple = new TNtuple("geantHitNtuple","pixels","p:theta:w1:w2:psi1:psi2:z1:z2:x1:y1:x2:y2:constAngle");
#endif
  
#endif
}



#ifdef myRICH_WITH_MC
bool StRichPIDMaker::makeTrackAssociations(StMcEvent* temp_mEvent, const StSPtrVecRichHit* hits ) {

  cout << "StRichPIDMaker:makeTrackAssociations()\n";
  
  if (!temp_mEvent || !hits) {
    cout << "StRichPIDMaker:makeTrackAssociations()\n";
    cout << "No StMcEvent/rich hits!" << endl;
    return false;}
  
  // StAssociationMaker
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  if (!assoc) {
    cout << "StRichPIDMaker:makeTrackAssociation() \n";
    cout << "No association maker!" << endl;
    return false;} 
  
  rcTrackMapType*  theTrackMap = 0;
  theTrackMap = assoc->rcTrackMap();
  if (!theTrackMap) {
    cout << "StRichPIDMaker:makeTrackAssociation() \n";
    cout << "No track map!" << endl;
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
				     float& wave, float& gpsi, float& z, float& gtheta, float& gphottheta) { 

  wave = -999;
  gpsi = -999;
  z = -999;
  gtheta = -999;
  gphottheta = -999;
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

  gphottheta = acos(-localMomentum.z()/localMomentum.mag());

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
  
  gtheta = acos(rotatedTrackMomentum.dot(photonRotatedMomentum));
 
  //  cout << "chernekov angle of emission = " << gtheta/degree << endl;
  // cout << "angle against normal = " << gphottheta/degree << endl;

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
  
    








