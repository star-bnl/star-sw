/*************************************************
 *
 * StMcAnalysisMaker.cxx
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

#include "StMcAnalysisMaker.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVector.hh"
#include "StPhysicalHelixD.hh"

#include "StChain/StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StSubDetector.hh"
#include "StAssociationMaker/StTrackPairInfo.hh"

#include "StEvent/StEvent.hh"
#include "StEvent/StTpcHit.hh"
#include "StEvent/StVecPtrTpcHit.hh"
#include "StEvent/StGlobalTrack.hh"
#include "StEvent/StTrackCollection.hh"
#include "StEvent/StVertex.hh"
#include "StEvent/StVertexCollection.hh"

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
    mHitResolution  = 0;   
    coordRec        = 0;  
    coordMcPartner  = 0;  
}

//_________________________________________________
StMcAnalysisMaker::~StMcAnalysisMaker()
{
    //  StMcAnalysisMaker Destructor
    //  delete the histograms
    SafeDelete(mAssociationCanvas);
    SafeDelete(mHitResolution);
    SafeDelete(coordRec);
    SafeDelete(coordMcPartner);
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
    rEvent = ((StEventMaker*) gStChain->Maker("events"))->event();
    StMcEvent* mEvent = 0;
    mEvent = ((StMcEventMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
    StAssociationMaker* assoc = 0;
    assoc = (StAssociationMaker*) gStChain->Maker("Associations");

    tpcHitMapType* theHitMap = 0;
    theHitMap = assoc->tpcHitMap();
    trackMapType* theTrackMap = 0;
    theTrackMap = assoc->trackMap();

    
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
    StPhysicalHelix& helix = (*trackBounds.first).first->helix();
    double s = helix.pathLength(firstTrack->startVertex()->position()); // path length at start vertex
    double B = 0.5*tesla;  // magnetic field

    cout << "MC Tracks associated with first Track in collection: " << theTrackMap->count(firstTrack) << endl;
    cout << "Momentum of First Track and of first Associated Track:" << endl;
    cout << "[" << abs(helix.momentumAt(s, B)) << ", ";
    cout << abs((*trackBounds.first).second->partnerMcTrack()->momentum()) << "]" << endl;
    cout << "These tracks have " << (*trackBounds.first).second->commonHits() << " hits in common." << endl;

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

    StVecPtrTpcHitIterator rcHitIt;
    StMcTpcHitIterator mcHitIt;
    const StVecPtrTpcHit& theHits = firstTrack->tpcHits();
    // Again run into the problem that StVecPtrTpcHitConstIterator doesn't exist yet.
//     for (int i =0; i<theHits.size(); i++) coordRec->Fill(theHits[i]->position().x(),theHits[i]->position().y());
    for (rcHitIt  = firstTrack->tpcHits().begin();
	     rcHitIt != firstTrack->tpcHits().end();
	     rcHitIt++) coordRec->Fill((*rcHitIt)->position().x(),(*rcHitIt)->position().y());
    for (mcHitIt  = partner->tpcHits()->begin();
	     mcHitIt != partner->tpcHits()->end();
	     mcHitIt++) coordMcPartner->Fill((*mcHitIt)->position().x(),(*mcHitIt)->position().y());
    

    
    
    mAssociationCanvas = new TCanvas("mAssociationCanvas", "Histograms");
    //mAssociationCanvas->SetLogy();
    //mNumberOfPings->Draw();

    return kStOK;
}
