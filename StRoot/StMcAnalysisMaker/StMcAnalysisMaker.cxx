/*************************************************
 *
 * $Id: StMcAnalysisMaker.cxx,v 1.4 1999/07/30 16:19:19 calderon Exp $
 * $Log: StMcAnalysisMaker.cxx,v $
 * Revision 1.4  1999/07/30 16:19:19  calderon
 * Use value_type typedef for inserting pairs in multimaps, Victor corrected iterators on HP in SL99h, Improved use of const for HP compilation
 *
 * Revision 1.3  1999/07/29 15:08:33  calderon
 * Include Mom. Resolution example (Histograms & Ntuple)
 *
 * Revision 1.2  1999/07/28 20:27:30  calderon
 * Version with SL99f libraries
 *
 *
 * Examples that use the structures of
 * StMcEvent and StAssociationMaker
 *
 *************************************************/

#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <math.h>
#include "TStyle.h"
#include "TCanvas.h"
#include "TH1.h"
#include "TH2.h"
#include "TNtuple.h"

#include "StMcAnalysisMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StPhysicalHelixD.hh"

#include "StChain/StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StSubDetector.hh"
#include "StAssociationMaker/StTrackPairInfo.hh"

#define USING_PERSISTENT
#ifndef USING_PERSISTENT
#include "StThreeVector.hh"

#include "StEvent/StEvent.hh"
#include "StEvent/StTpcHit.hh"
#include "StEvent/StVecPtrTpcHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"
#include "StEvent/StVertex.hh"
#include "StEvent/StVertexCollection.hh"
#else
#include "StThreeVectorF.hh"

#include "StEvent/StEvent.h"
#include "StEvent/StTpcHit.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StVertex.h"
#endif

#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcTpcHit.hh"
#include "StMcEvent/StMcTpcHitCollection.hh"
#include "StMcEvent/StMcTrack.hh"
#include "StMcEvent/StMcTrackCollection.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcVertexCollection.hh"

#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"

// Define data Members for the histograms
const Int_t   StMcAnalysisMaker::mNumDeltaX = 50;
const Int_t   StMcAnalysisMaker::mNumDeltaZ = 50;
const Float_t StMcAnalysisMaker::mMinDeltaX = -0.12;
const Float_t StMcAnalysisMaker::mMaxDeltaX =  0.12;
const Float_t StMcAnalysisMaker::mMinDeltaZ = -0.24;
const Float_t StMcAnalysisMaker::mMaxDeltaZ =  0.24;

ClassImp(StMcAnalysisMaker)


//_________________________________________________
StMcAnalysisMaker::StMcAnalysisMaker(const char *name, const char *title):StMaker(name,title)
{
    //  StMcAnalysisMaker Constructor
    // - zero all pointers defined in the header file
    mAssociationCanvas = 0;
    mMomResolution  = 0;
    mHitResolution  = 0;   
    coordRec        = 0;  
    coordMcPartner  = 0;
    mTrackNtuple    = 0;
}

//_________________________________________________
StMcAnalysisMaker::~StMcAnalysisMaker()
{
    //  StMcAnalysisMaker Destructor
    //  delete the histograms
    SafeDelete(mAssociationCanvas);
    SafeDelete(mMomResolution);
    SafeDelete(mHitResolution);
    SafeDelete(coordRec);
    SafeDelete(coordMcPartner);
    SafeDelete(mTrackNtuple);
}

//_________________________________________________
Int_t StMcAnalysisMaker::Finish()
{
    return StMaker::Finish();
}


//_________________________________________________
Int_t StMcAnalysisMaker::Init()
{
    // StMcAnalysisMaker - Init
    
    SetZones();  // This is my method to set the zones for the canvas.

    return StMaker::Init();
}
//_________________________________________________
Int_t StMcAnalysisMaker::Make()
{
    // Get the pointers we need, we have to use the titles we gave them in the
    // macro.  I just used the defaults.
    StEvent* rEvent = 0;
#ifndef USING_PERSISTENT
    rEvent = ((StEventReaderMaker*) gStChain->Maker("events"))->event();
#else
    rEvent = (StEvent*) GetInputDS("StEvent");
#endif

    StMcEvent* mEvent = 0;
    mEvent = ((StMcEventMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
    StAssociationMaker* assoc = 0;
    assoc = (StAssociationMaker*) gStChain->Maker("Associations");

    tpcHitMapType* theHitMap = 0;
    theHitMap = assoc->tpcHitMap();
    trackMapType* theTrackMap = 0;
    theTrackMap = assoc->trackMap();

    // Example: look at the position of the primary vertex
    //          Map is not needed for this, but it's a good check,
    //          tracking will not be good if primary vertex was not well placed.

    cout << "Position of Primary Vertex from StEvent:" << endl;
    cout << rEvent->primaryVertex()->position() << endl;
    cout << "Position of Primary Vertex from StMcEvent:" << endl;
    cout << mEvent->primaryVertex()->position() << endl;
    
    // Example: look at hits associated with 1st REC hit in Tpc Hit collection.

    StTpcHit*     firstHit;
    firstHit = *( rEvent->tpcHitCollection()->begin() );
    cout << "Assigned First Hit: " << endl;
    cout << *firstHit << endl;
    cout << "This hit has " <<  theHitMap->count(firstHit) << " MC Hits associated with it."<< endl;
    // To get the associated hits of the first hit we use equal_range(key), which returns
    // 2 iterators, the lower bound and upper bound, so that then we can loop over them.
    
    cout << "Position of First Rec. Hit and Associated (if any) MC Hit:" << endl;
    pair<tpcHitMapIter,tpcHitMapIter> hitBounds = theHitMap->equal_range(firstHit);

    for (tpcHitMapIter it=hitBounds.first; it!=hitBounds.second; ++it) {
	cout << "[" << (*it).first->position() << ", " << (*it).second->position() << "]" << endl;
    }

    // Example: Make a histogram using the Hit Map.
    
    
    // Delete previous histogram if any, then book histogram
    if (mHitResolution!=0) {
	delete mHitResolution;
	mHitResolution =0;
    }
    mHitResolution = new TH2F("Hit Resolution","Delta X Vs Delta Z for Hits",
			     mNumDeltaX,mMinDeltaX,mMaxDeltaX,mNumDeltaZ,mMinDeltaZ,mMaxDeltaZ);
    mHitResolution->SetXTitle("Delta X (cm)");
    mHitResolution->SetYTitle("Delta Z (cm)");

    // Fill histogram from map

    float DeltaX;
    float DeltaZ;
    
    StTpcHit* keyHit;
    StTpcHitCollection* recHits = rEvent->tpcHitCollection();
     
	    
    // Loop over Rec Hits
    for (StTpcHitIterator iter = recHits->begin(); iter != recHits->end(); iter++) {
	keyHit = *iter;
	pair<tpcHitMapIter,tpcHitMapIter> recBounds = theHitMap->equal_range(keyHit);
	
	for (tpcHitMapIter it2=recBounds.first; it2!=recBounds.second; ++it2){
	    
	    DeltaX = (*it2).first->position().x() - (*it2).second->position().x();
	    DeltaZ = (*it2).first->position().z() - (*it2).second->position().z();
	    
	    mHitResolution->Fill(DeltaX,DeltaZ); 
				
	}//Mc Hits assoc. w/ rec.
    } // Rec hits loop
    
    keyHit = 0;

    // Example: look at the magnitude of the momentum of
    //          the MC track associated with first track in track Collection
    
    StGlobalTrack*     firstTrack;
    firstTrack = *(rEvent->trackCollection()->begin());
    
    pair<trackMapIter,trackMapIter> trackBounds = theTrackMap->equal_range(firstTrack);
    
    // Calculate the momentum of the track at the start vertex and compare it to MC Track
    StPhysicalHelixD& helix = (*trackBounds.first).first->helix();
    double s = helix.pathLength(firstTrack->startVertex()->position()); // path length at start vertex
    double B = 0.5*tesla;  // magnetic field

    cout << "MC Tracks associated with first Track in collection: " << theTrackMap->count(firstTrack) << endl;
    cout << "Momentum of First Track and of first Associated Track:" << endl;
    cout << "[" << abs(helix.momentumAt(s, B)) << ", ";
    cout << abs((*trackBounds.first).second->partnerMcTrack()->momentum()) << "]" << endl;
    cout << "These tracks have " << (*trackBounds.first).second->commonHits() << " hits in common." << endl;


    // Example: Make a histogram of the momentum resolution of the event
    //          Make an Ntuple with rec & monte carlo mom, mean hit difference, and # of common hits
    StGlobalTrack* recTrack;
    StMcTrack*     mcTrack;
    StThreeVectorF p;
    StThreeVectorF pmc;
    float diff =0;
    if (mMomResolution!=0) {
	delete mMomResolution;
	mMomResolution=0;
    }
    mMomResolution = new TH1F("Mom. Resolution","(|p| - |pmc|)/|p|",100,-1.,1.);
    mMomResolution->SetXTitle("Resolution (%)");

    
    
    if (mTrackNtuple!=0) {
	delete mTrackNtuple;
	mTrackNtuple=0;
    }
    char* vars = "px:py:pz:p:pxrec:pyrec:pzrec:prec:commHits:hitDiffX:hitDiffY:hitDiffZ";
    float* values = new float[12];
    mTrackNtuple = new TNtuple("Track Ntuple","Track Pair Info",vars);
    
    cout << "Defined Momentum Res. Histogram & Ntuple" << endl;
    for (trackMapIter tIter=theTrackMap->begin();
	 tIter!=theTrackMap->end(); ++tIter){
	
	recTrack = (*tIter).first;
	if ((*tIter).second->commonHits()<10) continue;
	mcTrack = (*tIter).second->partnerMcTrack();

	pmc = mcTrack->momentum();
	for (int k=0; k<3; k++) values[k] = pmc[k];
	values[3]=pmc.mag();
	s = recTrack->helix().pathLength(recTrack->startVertex()->position());
	p = recTrack->helix().momentumAt(s, B);
	for (int j=0; j<3; j++) values[j+4] = p[j];
	values[7]=p.mag();
	values[8]=(*tIter).second->commonHits();
	// Fill 1d Mom. resolution Histogram
	diff = (p.mag() - pmc.mag())/p.mag();
	mMomResolution->Fill(diff,1.);

	// Loop to get Mean hit position diff.
	StThreeVectorF rHitPos(0,0,0);
	StThreeVectorF mHitPos(0,0,0);
	
	for (StTpcHitIterator hi=recTrack->tpcHits().begin(); hi!=recTrack->tpcHits().end(); hi++) {

	    StTpcHit* rHit = *hi;
	    
	    pair<tpcHitMapIter,tpcHitMapIter> rBounds = theHitMap->equal_range(rHit);
	    for (tpcHitMapIter hIter=rBounds.first; hIter!=rBounds.second; hIter++) {
		const StMcTpcHit* mHit = (*hIter).second;
		if (mHit->parentTrack() != mcTrack) continue;
		rHitPos += rHit->position();
		mHitPos += mHit->position();
	    }// Associated Hits Loop.
	
	} // Hits of rec. Track Loop
	
	rHitPos /=(float) (*tIter).second->commonHits();
	mHitPos /=(float) (*tIter).second->commonHits();
	for (int jj=0; jj<3; jj++) values[9+jj] = rHitPos[jj] - mHitPos[jj];
	mTrackNtuple->Fill(values);

    } // Tracks in Map Loop
    cout << "Finished Track Loop, Made Ntuple" << endl;
    //delete vars;
    delete values;


    // Example: Make 2 Histograms
    // - x and y positions of the hits from the reconstructed track.
    // - x and y positions of the hits from the  Monte Carlo  track.
    
    StMcTrack* partner = (*trackBounds.first).second->partnerMcTrack();
    if (coordRec!=0) {
	delete coordRec;
	coordRec =0;
    }
    coordRec = new TH2F("coords. Rec","X vs Y pos. of Hits", 100, -200, 200, 100, -200, 200);
    coordRec->SetXTitle("X (cm)");
    coordRec->SetYTitle("Y (cm)");
    if (coordMcPartner!=0) {
	delete coordMcPartner;
	coordMcPartner =0;
    }
    coordMcPartner = new TH2F("coords. MC","X vs Y pos. of Hits", 100, -200, 200, 100, -200, 200);
    coordMcPartner->SetXTitle("X (cm)");
    coordMcPartner->SetYTitle("Y (cm)");

    StTpcHitIterator rcHitIt;
    StMcTpcHitIterator mcHitIt;
    const StVecPtrTpcHit& theHits = firstTrack->tpcHits();
    for (rcHitIt  = theHits.begin();
	 rcHitIt != theHits.end();
	 rcHitIt++) coordRec->Fill((*rcHitIt)->position().x(),(*rcHitIt)->position().y());
    for (mcHitIt  = partner->tpcHits()->begin();
	 mcHitIt != partner->tpcHits()->end();
	 mcHitIt++) coordMcPartner->Fill((*mcHitIt)->position().x(),(*mcHitIt)->position().y());
    

    
    
    mAssociationCanvas = new TCanvas("mAssociationCanvas", "Histograms",200,10,900,500);

    return kStOK;
}
