/*************************************************
 *
 * StAssociationMaker.cxx
 * Revision 1.4  1999/07/28 20:27:23  calderon
 * Version with SL99f libraries
 *
 *
 *************************************************/

#include <iostream.h>
#include <cmath>

//#include "TStyle.h"
//#include "TCanvas.h"
//#include "TH1.h"
//#include "TH2.h"
using std::string;
using std::vector;
#include "StTrackPairInfo.hh"

#include "StThreeVector.hh"
//#include "StPhysicalHelixD.hh"
#include "SystemOfUnits.h"
#include "StChain/StChain.h"


#include "St_DataSet.h"
#include "StDevRow.hh"
#include "StDevice.hh"
#include "StLocalHit.hh"
#include "StMcParameterDB.h"
#include "StSubDetector.hh"
#include "StTpcLocalHit_mc.hh"
#include "StTpcLocalHit_recon.hh"
#include "StTrackPairInfo.hh"

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
#include "StEventTypes.h"
#include "StEventReaderMaker/StEventReaderMaker.h"

#include "StMcEventReaderMaker/StMcEventReaderMaker.h"

#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
bool compHit::operator()(const StTpcHit* h1, const StTpcHit* h2) {
    if (h1->position().x() != h2->position().x()) {
	return h1->position().x() < h2->position().x();
bool compMcFtpcHit::operator()(const StMcFtpcHit* h1, const StMcFtpcHit* h2) const {
    else if (h1->position().y() != h2->position().y()) {
	return h1->position().y() < h2->position().y();
    }
    else if   (h1->position().y() != h2->position().y()) {
	return h1->position().y() <  h2->position().y();
    }
    else return h1->position().z() < h2->position().z();
    
bool compTrack::operator()(const StGlobalTrack* t1, const StGlobalTrack* t2) {
    // return t1->startVertex()->position().x() < t2->startVertex()->position().x();
    
    // I was using the x position of the start vertex before, but somehow, when you use
    // count and equal range, it uses this comparison instead of just checking the keys.
    // Therefore, if several tracks had the same vertex, then it would think that they
    // were the same!  Hence, just compare the pointers themselves.  For the hit map,
    // it is rather unlikely that they would have the same x position so to safeguard
    // against having the same x, then compare y, if this also fails then compare z.  If they
    // are all the same, then it better be the same hit!

bool compTrack::operator()(const StGlobalTrack* t1, const StGlobalTrack* t2) const {
// Print out a pair
//template <class First, class Second>
}

// Print out Track pairs
    cout << (void*)p.first << " and " << (void*)(p.second->partnerMcTrack()) << " share " << p.second->commonHits() << " hits.";
    out << "Common SVT  Hits : " << p.second->commonSvtHits()  << endl;
    out << "Common FTPC Hits : " << p.second->commonFtpcHits() << endl;
    
// Print out a multimap
ostream& operator<<(ostream& out, const trackMapType& tm)

    copy(tm.begin(),tm.end(), ostream_iterator
	 <trackMapType::value_type>(cout,"\n"));
#else
    out << "Sorry, can't use ostream_iterator with this version of iostream.h !" << endl ;
#endif

#endif
    return out;
}


ClassImp(StAssociationMaker)


//_________________________________________________
StAssociationMaker::StAssociationMaker(const char *name, const char *title):StMaker(name,title)
{
    mTpcHitMap = 0;
    mTrackMap = 0;
    
    mNumberOfPings  = 0;   
    mTpcLocalHitResolution = 0;   
    mSvtHitResolution      = 0;   
    mFtpcHitResolution     = 0;


}

//_________________________________________________
    
    SafeDelete(mTpcHitMap);
    SafeDelete(mTrackMap);
    SafeDelete(mNumberOfPings);
	SafeDelete(mMcXiMap);
	cout << "Deleted M.C. Xi Map" << endl;
    }
}

//_____________________________________________________________________________
    // StMcEventReaderMaker - Clear,

    delete mTpcHitMap;
    mTpcHitMap = 0;
    delete mTrackMap;
    mTrackMap = 0;
    delete mNumberOfPings;
    mNumberOfPings = 0;
    
	SafeDelete(mMcXiMap);
	cout << "Deleted M.C. Xi Map" << endl;
    }
    StMaker::Clear();
}

  cout << "in StAssociationMaker::Finish....." << endl;
  
//_________________________________________________
Int_t StAssociationMaker::Finish()
{
  return StMaker::Finish();
}

//_________________________________________________
				  50, -8, 0.8);
    mFtpcHitResolution->SetXTitle("Rmc - Rrec (cm)");
    mFtpcHitResolution->SetYTitle("PHImc - PHIrec (deg)");
    
    return StMaker::Init();

//_________________________________________________
    cout << "AssociationMaker -- In Make()" << endl;
    
    
    //
    rEvent = ((StEventReaderMaker*) gStChain->Maker("events"))->event();
    if (!rEvent) cout << "No StEvent!!! " << endl;
    //
    mEvent = ((StMcEventReaderMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
    if (!mEvent) cout << "No StMcEvent!!! " << endl;
    
    
	cerr << "Bailing out ..." << endl;
    // fill SubDetector (TPC) structure with local >Reconstructed< hits
    
    StSubDetector* rTpcLocal = new StSubDetector(24,45);
    cout << "Defined Tpc for rec. hits" << endl;
    StTpcHitCollection* rTpcHits = rEvent->tpcHitCollection();
    cout << "Got " << rTpcHits->size() << " rec. Tpc Hits in this event" << endl;
    StTpcHitIterator riter;
    StTpcHit* rHit;
    for (riter=rTpcHits->begin(); riter!=rTpcHits->end(); riter++){
	rHit = *riter;
	rTpcLocal->addHit(rHit);
	
    }
    cout << "Prepared reconstructed hits." << endl;
    
    // fill SubDetector (TPC) structure with local >Monte Carlo< hits
    
    StSubDetector* mTpcLocal = new StSubDetector(24,45);
    cout << "Defined Tpc for mc hits" << endl;
    
    StMcTpcHitCollection* mcTpcHits = mEvent->tpcHitCollection();
    cout << "Got " << mcTpcHits->size() << " mc Tpc Hits in this event" << endl;
    StMcTpcHitIterator miter;
    StMcTpcHit* mHit;
    for (miter=mcTpcHits->begin(); miter!=mcTpcHits->end(); miter++){
	mHit = *miter;
	mTpcLocal->addHit(mHit);
    }
    cout << "Prepared MonteCarlo hits." << endl;
    

    // This could be a prime moment to sort the hits.
    // Ftpc
    StFtpcHitCollection*   rcFtpcHitColl = rEvent->ftpcHitCollection();
    // Get the pointer to the DB, the definitions of the cuts
    
    // Get the pointer to the parameter DB,
    // the definitions of the cuts
//     parDB->setXCut(.1); // 1 mm
//     parDB->setZCut(.2); // 2 mm
//     parDB->setReqCommonHits(3); // Require 3 hits in common for tracks to be associated
    cout << " X Cut : " << parDB->xCut()/millimeter << " mm" << endl;
    cout << " Z Cut : " << parDB->zCut()/millimeter << " mm" << endl;
    cout << " Required Hits for Associating Tracks: " << parDB->reqCommonHits() << endl;

    // Loop over hits and make Associations
    
    cout << "Making Hit Associations..." << endl;
    StTpcLocalHit_recon*  recHit;
    StTpcLocalHit_mc*     mcHit;
    
    // Instantiate the map
    //mTpcHitMap = new multimap<StTpcHit*, StMcTpcHit*, compHit>;
    mTpcHitMap = new tpcHitMapType;
    for (unsigned int iSector=0; iSector<rTpcLocal->numOfDevices(); iSector++) {
    
    for (unsigned int iSector=0;
	
	
	for (unsigned int iPadrow=0; iPadrow<rTpcLocal->device(iSector)->numOfRows(); iPadrow++) {
	    
	    
	    for (unsigned int iHit=0; iHit<rTpcLocal->device(iSector)->row(iPadrow)->nHits(); iHit++){
		
		recHit = (StTpcLocalHit_recon*) rTpcLocal->device(iSector)->row(iPadrow)->hit(iHit);
		
		for (unsigned int jHit=0; jHit<mTpcLocal->device(iSector)->row(iPadrow)->nHits(); jHit++){
		    
		    mcHit = (StTpcLocalHit_mc*) mTpcLocal->device(iSector)->row(iPadrow)->hit(jHit);
		    
		    if ( (StLocalHit) *recHit == (StLocalHit) *mcHit) {
			// Make Associations  Use map,
					
			//mTpcHitMap->insert(make_pair(recHit->globalHitPtr(), mcHit->globalHitPtr()));
			mTpcHitMap->insert(pair<StTpcHit* const, StMcTpcHit*>(recHit->globalHitPtr(), mcHit->globalHitPtr()) );
			//const pair<StTpcHit *const,StMcTpcHit *>
			// Make Associations  Use maps,
			mRcTpcHitMap->insert(rcTpcHitMapValType (rcTpcHit, mcTpcHit) );
		} // End of Hit loop for MC Hits
	    } // End of Hit loop for Rec. Hits
						 closestTpcHit->position().z()-
						 rcTpcHit->position().z() );
	    } // End of Hits in Padrow loop for Rec. Hits
    cout << "Finished Making Hit Associations *********" << endl;
    mMcFtpcHitMap = new mcFtpcHitMapType;
			rDiffMin=rDiff;
	cout << "Suggest increase distance cuts." << endl;
    
    }
    const StMcTpcHit* mcValueTpcHit;
    StTrackCollection* recTracks = rEvent->trackCollection();
    StTrackIterator recTrackIter;
    StGlobalTrack* recTrack;
    
    StVecPtrTpcHitIterator recHitIter;


    StTpcHit*  keyHit;    // key on the reconstructed Tpc Hit
    pair<tpcHitMapIter,tpcHitMapIter> bounds;

    StMcTpcHit* monteCarloHit;
    const StMcSvtHit* mcValueSvtHit;
    const StMcFtpcHit* mcValueFtpcHit;
    initializedTrackPing.nPingsSvt = 0;
    vector<trackPing> candidates(1000);
    
    vector<trackPing, allocator<trackPing> > candidates(1000);
    vector<trackPing> candidates(100, initializedTrackPing);
// #ifndef ST_NO_TEMPLATE_DEF_ARGS
//     vector<StTrackPairInfo*> candidates(1000);
// #else
//     vector<StTrackPairInfo*, allocator<StTrackPairInfo*> > candidates(1000);
// #endif

    vector<trackPing, allocator<trackPing> > candidates(100, initializedTrackPing);
    mTrackMap = new trackMapType;

    // Define the Histogram of Number of Pings
    if (mNumberOfPings!=0) {
	delete mNumberOfPings;
	mNumberOfPings =0;
    }
    mNumberOfPings = new TH1F("Associated hits","Pings for Rec. Tracks",
			      51,0,50);
    mNumberOfPings->SetXTitle("Number of Hits");

    // Instantiate the Track map
    mRcTrackMap = new rcTrackMapType;

    
    for (recTrackIter  = recTracks->begin(); recTrackIter != recTracks->end(); recTrackIter++){
	// Loop over tracks in StEvent
	
	recTrack = *recTrackIter; // For a by-pointer collection we need to dereference once

	// if (mTrackMap->count(recTrack) > 0) {
// 	    cout << "The track " << recTrack << " was here before!!! Partners: " << mTrackMap->count(recTrack) << endl;
// 	}
// 	else { cout << "Track not in Map yet" << endl;}

	// print out the map
        // cout << "The map is now" << endl << *mTrackMap << endl;

	rcTrack = dynamic_cast<StGlobalTrack*>(trkNode->track(global));
	if (!rcTrack || !(rcTrack->detectorInfo()->hits().size()))

	// Clear the candidate vector
	candidates.clear();
	const StVecPtrTpcHit& recTpcHits = recTrack->tpcHits();
	for (int i=0; i<recTpcHits.size(); i++) { // Had to put this in here because we don't have 
	  keyHit = recTpcHits[i];                 // a const iterator for the hits of a track.  HP complains
// 	for (recHitIter  = recTpcHits.begin();
// 	     recHitIter != recTpcHits.end();
// 	     recHitIter++) { // Loop over the hits of the track
	// Loop over the East FTPC hits of the track
// 	    keyHit = *recHitIter;
	    bounds = mTpcHitMap->equal_range(keyHit);
		
	    
	    
	    
	    for (tpcHitMapIter hmIter=bounds.first; hmIter!=bounds.second; ++hmIter) {

		monteCarloHit = hmIter->second;
		trackCand = monteCarloHit->parentTrack();
		
		mcValueFtpcHit = (*ftpcHMIter).second;
		trackCand = mcValueFtpcHit->parentTrack();
				
		// At this point we have a candidate Monte Carlo Track
		
		// If there are no candidates, create the first candidate.
		// If already there, increment its nPings.
		    candidates[0].mcTrack = trackCand;
		    candidates[0].nPings  = 1;
		if (nCandidates == 0) {
		    candidates[0].mcTrack    = trackCand;
		    candidates[0].nPingsFtpc  = 1;
		    nCandidates++;
		    
		}
		
			    candidates[iCandidate].nPings++;
		    for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
			if (trackCand==candidates[iCandidate].mcTrack){
			    candidates[iCandidate].nPingsFtpc++;
			    break;
			    candidates[nCandidates].nPings  = 1;
			if (iCandidate == (nCandidates-1)){
			    candidates[nCandidates].mcTrack = trackCand;
			    candidates[nCandidates].nPingsFtpc  = 1;
			    nCandidates++;
			    break;
			}
		    } // candidate loop
		    
	} // Hits from Track from StEvent loop
	
	// Now we need to associate the tracks that meet the commonHits criterion.

	
	if (nCandidates>1000) cout << "We Have More than 1000 candidates!!! " << endl;
	//
	if (nCandidates>100) cout << "We Have More than 100 candidates!!! " << endl;
	  mNumberOfPings->Fill((float) candidates[iCandidate].nPings);

	    
	  if (candidates[iCandidate].nPings > parDB->reqCommonHits() ){
	  if (candidates[iCandidate].nPingsTpc  >= parDB->reqCommonHitsTpc() ||
	      candidates[iCandidate].nPingsSvt  >= parDB->reqCommonHitsSvt() ||
	      candidates[iCandidate].nPingsFtpc >= parDB->reqCommonHitsFtpc()){
	    // mTrackMap->insert(make_pair(recTrack, candidates[iCandidate].mcTrack));
	    trkPair = new StTrackPairInfo(candidates[iCandidate].mcTrack, candidates[iCandidate].nPings);
	    //mTrackMap->insert(make_pair(recTrack, trkPair));
	    mTrackMap->insert(pair<StGlobalTrack* const, StTrackPairInfo*>(recTrack, trkPair));
	    
	    // print out the map
	    //cout << "The map is now" << endl << *mRcTrackMap << endl;
	  }
	
	}
	
    
    // Clear the candidate vector
    
    candidates.clear();
    delete rTpcLocal;
    delete mTpcLocal;
    
	}
    }
      
    return kStOK;
}
