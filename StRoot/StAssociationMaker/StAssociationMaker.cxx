/*************************************************
 *
 * $Id: StAssociationMaker.cxx,v 1.9 1999/09/28 14:35:00 fisyak Exp $
 * $Log: StAssociationMaker.cxx,v $
 * Revision 1.9  1999/09/28 14:35:00  fisyak
 * Remove cons dependence on non existing headers
 *
 * Revision 1.9  1999/09/28 14:35:00  fisyak
 * Remove cons dependence on non existing headers
 *
 * Revision 1.8  1999/09/23 21:25:18  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.7  1999/09/15 18:40:56  calderon
 * -If there is no StEvent or StMcEvent, print message to cerr and exit.
 * -Update README for changes
 *
 * Revision 1.6  1999/09/09 23:51:21  calderon
 * Made the following changes:
 * StAssociationMaker
 *
 * -correct definition of multimap for Solaris/ObjectSpace
 * -clear candidate vector at the end of reconstructed track loop
 * -remove # of pings histogram
 *
 * StLocalHit
 *
 * -use math.h instead of cmath because of abs()
 * -change abs() to fabs() everywhere
 * -change bool's to int's so Solaris doesn't complain
 *
 * Revision 1.5  1999/07/30 16:19:13  calderon
 * Use value_type typedef for inserting pairs in multimaps, Victor corrected iterators on HP in SL99h, Improved use of const for HP compilation
 *
 * Revision 1.4  1999/07/28 20:27:23  calderon
 * Version with SL99f libraries
 *
 *
 *************************************************/

#include <iostream.h>
#include <stdlib.h>
using std::string;
using std::vector;
#include "StMcParameterDB.h"
#include "StTrackPairInfo.hh"

#include "SystemOfUnits.h"
#include "StThreeVectorF.hh"


#include "St_DataSet.h"
#include "StDevRow.hh"
#include "StDevice.hh"
#include "StLocalHit.hh"
#include "StMcParameterDB.h"
#include "StSubDetector.hh"
#include "StTpcLocalHit_mc.hh"
#include "StTpcLocalHit_recon.hh"
#include "StTrackPairInfo.hh"

#define USING_PERSISTENT
#ifndef USING_PERSISTENT
#include "StThreeVector.hh"

#include <StEvent.hh>
#include <StTpcHit.hh>
#include <StVecPtrTpcHit.hh>
#include <StGlobalTrack.hh>
#include <StTrackCollection.hh>
#include <StVertex.hh>
#include <StVertexCollection.hh>

#else
#include "StThreeVectorF.hh"
#include "St_DataSetIter.h"
#include "StEvent.h"
#include "StTpcHit.h"
#include "StGlobalTrack.h"
#include "StVertex.h"


#endif

#include "StMcEvent.hh"
#include "StMcTpcHit.hh"
#include "StMcTpcHitCollection.hh"
#include "StMcTrack.hh"
#include "StMcTrackCollection.hh"
#include "StMcVertex.hh"
#include "StMcVertexCollection.hh"
#include "StEventTypes.h"

#include "StMcEventTypes.hh"

#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
bool compHit::operator()(const StTpcHit* h1, const StTpcHit* h2) const {
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
    
}
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
    
    //mNumberOfPings  = 0;   
    mTpcLocalHitResolution = 0;   
    mSvtHitResolution      = 0;   
    mFtpcHitResolution     = 0;


}

//_________________________________________________
StAssociationMaker::~StAssociationMaker()
    SafeDelete(mTpcHitMap);
    SafeDelete(mTrackMap);
    //SafeDelete(mNumberOfPings);
	SafeDelete(mMcXiMap);
	cout << "Deleted M.C. Xi Map" << endl;
    }
}

//_____________________________________________________________________________

    cout << "StAssociationMaker::Clear *** " << endl;
    delete mTpcHitMap;
    mTpcHitMap = 0;
    delete mTrackMap;
    mTrackMap = 0;
    
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
}

//_________________________________________________
    cout << "AssociationMaker -- In Make()" << endl;
    
   
    //
#ifndef USING_PERSISTENT
    rEvent = ((StEventReaderMaker*) gStChain->Maker("events"))->event();
#else
    // Get StEvent
#endif
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");

    if (!rEvent) {
	cerr << "No StEvent!!! " << endl;
    //
    // Get StMcEvent
    //
    StMcEvent* mEvent = 0;
    mEvent = ((StMcEventMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
    if (!mEvent) {
	cerr << "No StMcEvent!!! " << endl;
	cerr << "Bailing out ..." << endl;
    // fill SubDetector (TPC) structure with local >Reconstructed< hits
    
    StSubDetector* rTpcLocal = new StSubDetector(24,45);
    cout << "Defined Tpc for rec. hits" << endl;
    
    StTpcHitCollection* rTpcHits = rEvent->tpcHitCollection();
    cout << "Got " << rTpcHits->size() << " rec. Tpc Hits in this event" << endl;
    StTpcHit* rHit;
    StTpcHitIterator riter;
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
    cout << " X Cut : " << parDB->xCut()/millimeter << " mm" << endl;
    cout << " Z Cut : " << parDB->zCut()/millimeter << " mm" << endl;
    cout << " Required Hits for Associating Tracks: " << parDB->reqCommonHits() << endl;

    // Loop over hits and make Associations
    
    cout << "Making Hit Associations..." << endl;
    StTpcLocalHit_recon*  recHit;
    StTpcLocalHit_mc*     mcHit;
    
    // Instantiate the map
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
						
			mTpcHitMap->insert(tpcHitMapValType (recHit->globalHitPtr(), mcHit->globalHitPtr()) );
			
			// Make Associations  Use maps,
			mRcTpcHitMap->insert(rcTpcHitMapValType (rcTpcHit, mcTpcHit) );
			mMcTpcHitMap->insert(mcTpcHitMapValType (mcTpcHit, rcTpcHit) );
						 rcTpcHit->position().x(),
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
    StGlobalTrack* recTrack;

    StTpcHit*  keyHit;    // key on the reconstructed Tpc Hit
    pair<tpcHitMapIter,tpcHitMapIter> bounds;

    const StMcTpcHit* monteCarloHit;
    const StMcSvtHit* mcValueSvtHit;
    const StMcFtpcHit* mcValueFtpcHit;
    initializedTrackPing.nPingsSvt = 0;
    vector<trackPing> candidates(100);
    
    vector<trackPing, allocator<trackPing> > candidates(100);
    vector<trackPing> candidates(100, initializedTrackPing);
    vector<trackPing, allocator<trackPing> > candidates(100, initializedTrackPing);
    mTrackMap = new trackMapType;

    // Instantiate the Track map
    mRcTrackMap = new rcTrackMapType;
    StTrackIterator recTrackIter;
    for (recTrackIter  = recTracks->begin(); recTrackIter != recTracks->end(); recTrackIter++){
	// Loop over tracks in StEvent
	recTrack = *recTrackIter; // For a by-pointer collection we need to dereference once
	if (!(recTrack->numberOfTpcHits())) continue; // If there are no Tpc Hits, skip track.
	// print out the map
        // cout << "The map is now" << endl << *mTrackMap << endl;
	rcTrack = dynamic_cast<StGlobalTrack*>(trkNode->track(global));
	if (!rcTrack || !(rcTrack->detectorInfo()->hits().size()))
	    } // mc ftpc hits in multimap
	const StVecPtrTpcHit& recTpcHits = recTrack->tpcHits();
        StTpcHitIterator recHitIter;
	for (recHitIter  = recTpcHits.begin();
	     recHitIter != recTpcHits.end();
	     recHitIter++) { // Loop over the hits of the track
	// Loop over the East FTPC hits of the track
	    keyHit = *recHitIter;
	    bounds = mTpcHitMap->equal_range(keyHit);
		
	    for (tpcHitMapIter hmIter=bounds.first; hmIter!=bounds.second; ++hmIter) {
	    boundsFtpc = mRcFtpcHitMap->equal_range(rcKeyFtpcHit);
		monteCarloHit = (*hmIter).second;
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
	
	//
	// Now we need to associate the tracks that meet the commonHits criteria.
	//
	
	if (nCandidates>100) cout << "We Have More than 100 candidates!!! " << endl;
	for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){
	  if (candidates[iCandidate].nPings > parDB->reqCommonHits() ){
	  if (candidates[iCandidate].nPingsTpc  >= parDB->reqCommonHitsTpc() ||
	      candidates[iCandidate].nPingsSvt  >= parDB->reqCommonHitsSvt() ||
	      candidates[iCandidate].nPingsFtpc >= parDB->reqCommonHitsFtpc()){
	    trkPair = new StTrackPairInfo(candidates[iCandidate].mcTrack, candidates[iCandidate].nPings);
	    //mTrackMap->insert(make_pair(recTrack, trkPair));
	    mTrackMap->insert(trackMapValType (recTrack, trkPair));
	    
	    // print out the map
	    //cout << "The map is now" << endl << *mRcTrackMap << endl;
	  }
	}
	
	
    }// StEvent track loop

    // Clear the candidate vector
    
    candidates.clear();
    delete rTpcLocal;
    delete mTpcLocal;
    
	}
    }
      
    return kStOK;
}
