//*-- Author : Victor Perevoztchikov
// 
// $Id: StUndoDistortionMaker.cxx,v 1.1 2002/11/19 18:44:28 dunlop Exp $
// $Log: StUndoDistortionMaker.cxx,v $
// Revision 1.1  2002/11/19 18:44:28  dunlop
// This undoes the distortions and refits.
// Modifications made to Bum's refitter in order to do a "primary" fit.
// Much like non-Kalman primary fit used to be.
//
// Revision 1.15  2002/04/28 01:28:36  jeromel
// Reshaped comments for doxygen. Hopefully, users will propagate this good
// habit.
//
// Revision 1.14  2000/06/23 16:50:07  fisyak
// remove params
//
// Revision 1.13  1999/12/19 16:07:01  perev
// Add README
//
// Revision 1.12  1999/07/15 13:57:44  perev
// cleanup
//
// Revision 1.11  1999/07/10 22:59:16  fine
// Some comments have been introduced to show html docs
//


#include "StUndoDistortionMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
// SCL
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
  
// StEvent
#include "StEventTypes.h"

#include "StGlobalTrackRefitter.h"
#include "StTpcDb/StTpcDb.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StDbUtilities/StMagUtilities.h"
#include "StDbUtilities/StCoordinates.hh"

ClassImp(StUndoDistortionMaker)

//_____________________________________________________________________________
/// TLA constructor
/*!
  const char *name -  the name of this constructor
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  See <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A>

 */
StUndoDistortionMaker::StUndoDistortionMaker(const char *name):StMaker(name){
    //
}


//_____________________________________________________________________________
/// This is TLA destructor
/*!
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  
  The first comment lines after the opening bracket
  ({) of a member function are considered as a member function description 
  see: <A HREF="http://root.cern.ch/root/Documentation.html"> ROOT HTML documentation </A> 

 */
StUndoDistortionMaker::~StUndoDistortionMaker(){
    //
}


//_____________________________________________________________________________
/// Init - is a first method the top level StChain calls to initialize all its makers 
Int_t StUndoDistortionMaker::Init(){
    // Create tables
    // Create Histograms    
    mRefitter = 0;
    
    mRefitter = new StGlobalTrackRefitter();
    mRefitter->setDebug(1);
    
    m_ExB = 0;
    
    return StMaker::Init();
}


//_____________________________________________________________________________
/// Make - this method is called in loop for each event
Int_t StUndoDistortionMaker::Make(){

    cout << "In UndoDistortionMaker::Make() " << endl;
    
    StEvent *event = (StEvent *) GetInputDS("StEvent");
    if (!event) {
	cout << "No Event?" << endl;
	return kStWarn;
    }
    
    if (!mRefitter) 
    {
	cout << "No refitter? " << endl;
	
	return kStWarn;
    }
    
// Get the mag field
    double Bmag = 2.49117*kilogauss;
    if (event->summary()) {
	Bmag = event->summary()->magneticField() * kilogauss;
    }
    mRefitter->setBField(Bmag);
    
    if (!m_ExB) {
//  	float gFactor = Bmag/kilogauss/4.980;
//  	cout << "gFactor == " << gFactor << endl;
	
//  	if (gFactor < -0.8) {
//  	    SetFlavor("FullMagFNegative","tpcGlobalPosition");
//  	}
//  	else if (gFactor < -0.2) {
//  	    SetFlavor("HalfMagFNegative","tpcGlobalPosition");
//  	}
//  	else if (gFactor < 0.2) {
//  	    SetFlavor("ZeroMagF","tpcGlobalPosition");
//  	}
//  	else if (gFactor < 0.8) {
//  	    SetFlavor("HalfMagFPositive","tpcGlobalPosition");
//  	}
//  	else if (gFactor < 1.2) {
//  	    SetFlavor("FullMagFPositive","tpcGlobalPosition");
//  	}
//          StTpcDbMaker *tpcDb = (StTpcDbMaker*) GetMaker("tpcDb");
//  	if (tpcDb) {
//  	    cout << "Updating TPC database" << endl;
	    
//  	    tpcDb->Update_tpg_pad_plane();
//  	    tpcDb->Update_tpg_detector();
//  	}
    
	TDataSet *RunLog = GetDataBase("RunLog");

	m_ExB = new StMagUtilities(gStTpcDb,RunLog,0);

    }
    
    
// Next: make a vector of all global tracks
    
    StPrimaryVertex *theVertex = event->primaryVertex();
    StPrimaryVertex *vert1=0;
    StPrimaryVertex *vert2=0;
    StPrimaryVertex *vert3=0;


    
    if (theVertex) {
	
	vert1 = new StPrimaryVertex();
        vert2 = new StPrimaryVertex();
	vert3 = new StPrimaryVertex();
	vert1->setPosition(theVertex->position());
	vert2->setPosition(theVertex->position());
	vert3->setPosition(theVertex->position());
	
	event->addPrimaryVertex(vert1);
	event->addPrimaryVertex(vert2);
	event->addPrimaryVertex(vert3);
    }
    StThreeVectorF vertErr(0.02,0.02,0.2);
    StThreeVectorF vertPos(theVertex->position());
    

// Set up a vector of all tpc hits for ease
    vector<StTpcHit*> allTpcHits;
    StTpcHitCollection *theCollection = event->tpcHitCollection();
    for (size_t isec = 0; isec < theCollection->numberOfSectors(); ++isec) {
	StTpcSectorHitCollection *theSec = theCollection->sector(isec);
	for (size_t ipad = 0; ipad < theSec->numberOfPadrows(); ++ipad) {
	    StTpcPadrowHitCollection *thePad = theSec->padrow(ipad);
	    StSPtrVecTpcHit & theHits = thePad->hits();
	    for (StSPtrVecTpcHitIterator hiter = theHits.begin();
		 hiter != theHits.end(); ++hiter) {
		allTpcHits.push_back(*hiter);
	    }
	}
    }
    
    cout << "Found " << allTpcHits.size() << "Tpc hits" << endl;
    


// Now deal with the refitter.
// Pass 1 

    StSPtrVecTrackNode& theNodes = event->trackNodes();

    
    // original globals and primaries
    vector<StTrack*> gvec;
    vector<StTrack*> pvec;
    
    for (StSPtrVecTrackNodeIterator niter = theNodes.begin();
	 niter != theNodes.end(); ++niter) {

	// Take the last pid trait available
	StRichPidTraits *theTrait = 0;
	StTrackNode *node = *niter;
	
	for (size_t ientry = 0; ientry < node->entries(); ++ientry) {
	    StTrack * tTrack = node->track(ientry);
	    if (dynamic_cast<StGlobalTrack*>(tTrack)) {
		gvec.push_back(tTrack);
	    }
	    if (dynamic_cast<StPrimaryTrack*>(tTrack)) {
		pvec.push_back(tTrack);
		StSPtrVecTrackPidTraits& theTraits = tTrack->pidTraits();
		for (StSPtrVecTrackPidTraitsIterator riter = theTraits.begin();
		     riter != theTraits.end(); ++riter) {
		    StRichPidTraits *tempTrait = dynamic_cast<StRichPidTraits*>(*riter);
		    if (tempTrait) theTrait = tempTrait;
		}
	    }
	}
// Put the traits on here.  They'll be copied over into the newer files.
	if (theTrait) {
	    
	    for (size_t ientry=0; ientry < node->entries(); ++ientry) {
		StTrack *tTrack = node->track(ientry);
		StRichPidTraits *isTrait = 0;
		StSPtrVecTrackPidTraits& theTraits = tTrack->pidTraits();
		for (StSPtrVecTrackPidTraitsIterator riter = theTraits.begin();
		     riter != theTraits.end(); ++riter) {
		    StRichPidTraits *tempTrait = dynamic_cast<StRichPidTraits*>(*riter);
		    if (tempTrait) isTrait = tempTrait;
		}
		if (!isTrait) {
		    tTrack->addPidTraits(new StRichPidTraits(*theTrait));
		}
	    }
	}
    }
    
    cout << "Found " << gvec.size() << " globals and " << pvec.size() << "primaries" << endl;
    
    for (vector<StTrack*>::iterator titer = gvec.begin();
	 titer != gvec.end(); ++titer) {
	
	StTrack *gTrack = (*titer);
	StTrackNode *node = gTrack->node();
	if (!node) continue;
	if (!gTrack) continue;
	StGlobalTrack *oldg = dynamic_cast<StGlobalTrack*>(gTrack);
	if (!oldg) continue;
	StGlobalTrack *newg = new StGlobalTrack(*oldg);
	if (!newg) continue;
	newg->setDetectorInfo(oldg->detectorInfo());
	newg->setNode(node);
	newg->setEncodedMethod(990);

// We want a new one here, not constant.
	const StPhysicalHelixD *tHelix = mRefitter->refit(gTrack);
	StPhysicalHelixD newHelix(*tHelix);
	
	
	if (newg->detectorInfo()) {
	    double pathFirst = newHelix.pathLength(newg->detectorInfo()->firstPoint());
	    newHelix.moveOrigin(pathFirst);
	}
	// Make a new StTrackGeometry
	short q = ((Bmag * newHelix.h())<0) ? 1 : -1;
	float c = newHelix.curvature();
	float dip = newHelix.dipAngle();
	
	StThreeVectorF o = newHelix.origin();
	
	StThreeVectorF p = newHelix.momentumAt(0,Bmag);
	
	double psi = newHelix.phase()+newHelix.h()*pi/2;
	short h = newHelix.h();
	newg->setGeometry(new StHelixModel(q,psi,c,dip,o,p,h));
	newg->setOuterGeometry(new StHelixModel(q,psi,c,dip,o,p,h));

	
	
	node->addTrack(newg);

    } // loop over globals
    
    if (vert1) {
	
	
    // now deal with the primaries
	for (vector<StTrack*>::iterator titer = pvec.begin();
	     titer != pvec.end(); ++titer) {
	    
	    StTrack *pTrack = (*titer);
	    StTrackNode *node = pTrack->node();
	    if (!node) continue;
	    if (!pTrack) continue;
	    StPrimaryTrack *oldp = dynamic_cast<StPrimaryTrack*>(pTrack);
	    if (!oldp) continue;
	    StPrimaryTrack *newp = new StPrimaryTrack(*oldp);
	    if (!newp) continue;
	    newp->setDetectorInfo(oldp->detectorInfo());
	    newp->setNode(node);
	    newp->setEncodedMethod(990);
	    
// We want a new one here, not constant.
	    const StPhysicalHelixD *tHelix= mRefitter->refit(pTrack);
	    mRefitter->addPoint(vertPos,vertErr);
	    tHelix = mRefitter->refitNoFill(pTrack);
	    
	    if (!tHelix) continue;
	    StPhysicalHelixD newHelix(*tHelix);
	    double pathFirst = newHelix.pathLength(vert1->position());
	    
	    newHelix.moveOrigin(pathFirst);
	    
	    // Make a new StTrackGeometry
	    short q = ((Bmag * newHelix.h())<0) ? 1 : -1;
	    float c = newHelix.curvature();
	    float dip = newHelix.dipAngle();
	    
	    StThreeVectorF o = newHelix.origin();
	    
	    StThreeVectorF p = newHelix.momentumAt(0,Bmag);
	    
	    double psi = newHelix.phase()+newHelix.h()*pi/2;
	    short h = newHelix.h();

	    newp->setGeometry(new StHelixModel(q,psi,c,dip,o,p,h));
	    newp->setOuterGeometry(new StHelixModel(q,psi,c,dip,o,p,h));

	    
	    
	    node->addTrack(newp);
	    vert1->addDaughter(newp);
	    
	} // Primaries
    }
    // Now undo the distortion
    for (vector<StTpcHit*>::iterator hiter = allTpcHits.begin();
	 hiter != allTpcHits.end(); ++hiter) {
	// first convert back into local coordinates
	const StThreeVectorF &spc = (*hiter)->position();
	float x[3],Xprime[3];
	x[0] = spc.x();      x[1] = spc.y();      x[2] = spc.z();
	
	StTpcCoordinateTransform transform(gStTpcDb);
	
	StGlobalCoordinate global(x[0],x[1],x[2]);
	StTpcLocalCoordinate local;
	transform(global,local);
	x[0] = local.position().x();
	x[1] = local.position().y();
	x[2] = local.position().z();
	
	m_ExB->UndoSpaceChargeDistortion(x,Xprime);
	// Need to DO the distortion
	Xprime[0] = 2*x[0] - Xprime[0] ;
	Xprime[1] = 2*x[1] - Xprime[1] ;
	Xprime[2] = 2*x[2] - Xprime[2] ;
	
	local.position().setX(Xprime[0]);
	local.position().setY(Xprime[1]);
	local.position().setZ(Xprime[2]);
	transform(local,global);
	
//	cout 
//  	    << "Moved (" << spc.x() 
//  	    << "," << spc.y() 
//  	    << "," << spc.z() 
//  	    << ") to (" 
//  	    << global.position().x() 
//  	    << "," << global.position().y() 
//  	    << "," << global.position().z() <<
//  	    ") " << endl;
	// Because of the =, this gets deep copied.  So this is fine.
	// Damn.  Need to do something here because of StThreeVectorF vs
	// StThreeVector<double>
	x[0] = global.position().x();
	x[1] = global.position().y();
	x[2] = global.position().z();
	
	StThreeVectorF newPosition(x);
	(*hiter)->setPosition(newPosition);
	
    }

// Now refit
    for (vector<StTrack*>::iterator titer = gvec.begin();
	 titer != gvec.end(); ++titer) {
	
	StTrack *gTrack = (*titer);
	StTrackNode *node = gTrack->node();
	if (!node) continue;
	if (!gTrack) continue;
	StGlobalTrack *oldg = dynamic_cast<StGlobalTrack*>(gTrack);
	if (!oldg) continue;
	StGlobalTrack *newg = new StGlobalTrack(*oldg);
	if (!newg) continue;
	newg->setDetectorInfo(oldg->detectorInfo());
	newg->setNode(node);
	newg->setEncodedMethod(991);

// We want a new one here, not constant.
	const StPhysicalHelixD *tHelix = mRefitter->refit(gTrack);
	StPhysicalHelixD newHelix(*tHelix);
	
	
	if (newg->detectorInfo()) {
	    double pathFirst = newHelix.pathLength(newg->detectorInfo()->firstPoint());
	    newHelix.moveOrigin(pathFirst);
	}
	// Make a new StTrackGeometry
	short q = ((Bmag * newHelix.h())<0) ? 1 : -1;
	float c = newHelix.curvature();
	float dip = newHelix.dipAngle();
	
	StThreeVectorF o = newHelix.origin();
	
	StThreeVectorF p = newHelix.momentumAt(0,Bmag);
	
	double psi = newHelix.phase()+newHelix.h()*pi/2;
	short h = newHelix.h();
	newg->setGeometry(new StHelixModel(q,psi,c,dip,o,p,h));
	newg->setOuterGeometry(new StHelixModel(q,psi,c,dip,o,p,h));

	
	
	node->addTrack(newg);

    } // loop over globals
    
    if (vert2) {
	
	
    // now deal with the primaries
	for (vector<StTrack*>::iterator titer = pvec.begin();
	     titer != pvec.end(); ++titer) {
	    
	    StTrack *pTrack = (*titer);
	    StTrackNode *node = pTrack->node();
	    if (!node) continue;
	    if (!pTrack) continue;
	    StPrimaryTrack *oldp = dynamic_cast<StPrimaryTrack*>(pTrack);
	    if (!oldp) continue;
	    StPrimaryTrack *newp = new StPrimaryTrack(*oldp);
	    if (!newp) continue;
	    newp->setDetectorInfo(oldp->detectorInfo());
	    newp->setNode(node);
	    newp->setEncodedMethod(991);
	    
// We want a new one here, not constant.
	    const StPhysicalHelixD *tHelix= mRefitter->refit(pTrack);
	    mRefitter->addPoint(vertPos,vertErr);
	    tHelix = mRefitter->refitNoFill(pTrack);
	    
	    if (!tHelix) continue;
	    StPhysicalHelixD newHelix(*tHelix);
	    double pathFirst = newHelix.pathLength(vert2->position());
	    
	    newHelix.moveOrigin(pathFirst);
	    
	    // Make a new StTrackGeometry
	    short q = ((Bmag * newHelix.h())<0) ? 1 : -1;
	    float c = newHelix.curvature();
	    float dip = newHelix.dipAngle();
	    
	    StThreeVectorF o = newHelix.origin();
	    
	    StThreeVectorF p = newHelix.momentumAt(0,Bmag);
	    
	    double psi = newHelix.phase()+newHelix.h()*pi/2;
	    short h = newHelix.h();

	    newp->setGeometry(new StHelixModel(q,psi,c,dip,o,p,h));
	    newp->setOuterGeometry(new StHelixModel(q,psi,c,dip,o,p,h));

	    
	    
	    node->addTrack(newp);
	    vert2->addDaughter(newp);
	    
	} // Primaries
    }
    // Now put in the space charge R2 distortion
    for (vector<StTpcHit*>::iterator hiter = allTpcHits.begin();
	 hiter != allTpcHits.end(); ++hiter) {
	// first convert back into local coordinates
	const StThreeVectorF &spc = (*hiter)->position();
	float x[3],Xprime[3];
	x[0] = spc.x();      x[1] = spc.y();      x[2] = spc.z();
	
	StTpcCoordinateTransform transform(gStTpcDb);
	
	StGlobalCoordinate global(x[0],x[1],x[2]);
	StTpcLocalCoordinate local;
	transform(global,local);
	x[0] = local.position().x();
	x[1] = local.position().y();
	x[2] = local.position().z();
	
	m_ExB->UndoSpaceChargeR2Distortion(x,Xprime);
	
	local.position().setX(Xprime[0]);
	local.position().setY(Xprime[1]);
	local.position().setZ(Xprime[2]);
	transform(local,global);
	
//  	cout 
//  	    << "Moved (" << spc.x() 
//  	    << "," << spc.y() 
//  	    << "," << spc.z() 
//  	    << ") to (" 
//  	    << global.position().x() 
//  	    << "," << global.position().y() 
//  	    << "," << global.position().z() <<
//  	    ") " << endl;
	// Because of the =, this gets deep copied.  So this is fine.
	// Damn.  Need to do something here because of StThreeVectorF vs
	// StThreeVector<double>
	x[0] = global.position().x();
	x[1] = global.position().y();
	x[2] = global.position().z();
	
	StThreeVectorF newPosition(x);
	(*hiter)->setPosition(newPosition);
	
    }

// Now refit
    for (vector<StTrack*>::iterator titer = gvec.begin();
	 titer != gvec.end(); ++titer) {
	
	StTrack *gTrack = (*titer);
	StTrackNode *node = gTrack->node();
	if (!node) continue;
	if (!gTrack) continue;
	StGlobalTrack *oldg = dynamic_cast<StGlobalTrack*>(gTrack);
	if (!oldg) continue;
	StGlobalTrack *newg = new StGlobalTrack(*oldg);
	if (!newg) continue;
	newg->setDetectorInfo(oldg->detectorInfo());
	newg->setNode(node);
	newg->setEncodedMethod(992);

// We want a new one here, not constant.
	const StPhysicalHelixD *tHelix = mRefitter->refit(gTrack);
	StPhysicalHelixD newHelix(*tHelix);
	
	
	if (newg->detectorInfo()) {
	    double pathFirst = newHelix.pathLength(newg->detectorInfo()->firstPoint());
	    newHelix.moveOrigin(pathFirst);
	}
	// Make a new StTrackGeometry
	short q = ((Bmag * newHelix.h())<0) ? 1 : -1;
	float c = newHelix.curvature();
	float dip = newHelix.dipAngle();
	
	StThreeVectorF o = newHelix.origin();
	
	StThreeVectorF p = newHelix.momentumAt(0,Bmag);
	
	double psi = newHelix.phase()+newHelix.h()*pi/2;
	short h = newHelix.h();
	newg->setGeometry(new StHelixModel(q,psi,c,dip,o,p,h));
	newg->setOuterGeometry(new StHelixModel(q,psi,c,dip,o,p,h));

	
	
	node->addTrack(newg);

    } // loop over globals
    
    if (vert3) {
	
	
    // now deal with the primaries
	for (vector<StTrack*>::iterator titer = pvec.begin();
	     titer != pvec.end(); ++titer) {
	    
	    StTrack *pTrack = (*titer);
	    StTrackNode *node = pTrack->node();
	    if (!node) continue;
	    if (!pTrack) continue;
	    StPrimaryTrack *oldp = dynamic_cast<StPrimaryTrack*>(pTrack);
	    if (!oldp) continue;
	    StPrimaryTrack *newp = new StPrimaryTrack(*oldp);
	    if (!newp) continue;
	    newp->setDetectorInfo(oldp->detectorInfo());
	    newp->setNode(node);
	    newp->setEncodedMethod(992);
	    
// We want a new one here, not constant.
	    const StPhysicalHelixD *tHelix= mRefitter->refit(pTrack);
	    mRefitter->addPoint(vertPos,vertErr);
	    tHelix = mRefitter->refitNoFill(pTrack);
	    
	    if (!tHelix) continue;
	    StPhysicalHelixD newHelix(*tHelix);
	    double pathFirst = newHelix.pathLength(vert3->position());
	    
	    newHelix.moveOrigin(pathFirst);
	    
	    // Make a new StTrackGeometry
	    short q = ((Bmag * newHelix.h())<0) ? 1 : -1;
	    float c = newHelix.curvature();
	    float dip = newHelix.dipAngle();
	    
	    StThreeVectorF o = newHelix.origin();
	    
	    StThreeVectorF p = newHelix.momentumAt(0,Bmag);
	    
	    double psi = newHelix.phase()+newHelix.h()*pi/2;
	    short h = newHelix.h();

	    newp->setGeometry(new StHelixModel(q,psi,c,dip,o,p,h));
	    newp->setOuterGeometry(new StHelixModel(q,psi,c,dip,o,p,h));

	    
	    
	    node->addTrack(newp);
	    vert3->addDaughter(newp);
	    
	} // Primaries
    }


    // Reset all the pid trait signed dca 2d and 3d
    if (theVertex) {
	
	for (StSPtrVecTrackNodeIterator niter = theNodes.begin();
	     niter != theNodes.end(); ++niter) {
	    for (size_t ientry = 0; ientry < (*niter)->entries(); ++ientry) {
		StTrack *track = (*niter)->track(ientry);
		if (!track) continue;
		
		// Find the pid trait
		// Take the last pid trait available
		StRichPidTraits *theTrait = 0;
		StSPtrVecTrackPidTraits &theTraits = track->pidTraits();
		for (StSPtrVecTrackPidTraitsIterator traititer = theTraits.begin();
		     traititer != theTraits.end(); ++traititer) {
		    StRichPidTraits *tempTrait = dynamic_cast<StRichPidTraits*>(*traititer);
		    if (tempTrait) theTrait = tempTrait;
		}
		
		if (!theTrait) continue;
		
		// Find the associated global
		StTrack *gtrack = 0;
		for (size_t ient2 = 0; ient2 < (*niter)->entries(global); ++ient2) {
		    StTrack *ttrack = (*niter)->track(global,ient2);
		    if (!ttrack) continue;
		    
		    if (ttrack->encodedMethod() == track->encodedMethod()) gtrack = ttrack;
		}
		if (!gtrack) continue;
		
		if (gtrack->geometry()) {
		    double dca2d = gtrack->geometry()->helix().curvatureSignedDistance(theVertex->position().x(),theVertex->position().y());
		    
		    theTrait->setSignedDca2d(dca2d);
		    double dca3d = gtrack->geometry()->helix().curvatureSignedDistance(theVertex->position());
		    
		    theTrait->setSignedDca3d(dca3d);
		}
	    }
	    
	}
    }
    
	
		
					     
    return kStOK;
}


    










