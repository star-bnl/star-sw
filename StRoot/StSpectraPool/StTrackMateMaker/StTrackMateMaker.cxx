//
// $Id: StTrackMateMaker.cxx,v 1.1 2004/09/13 22:04:53 calderon Exp $
//
#include <iostream>
#include <map>
#include <algorithm>
#include <vector>
#include <set>

#include "StTrackMateMaker.h"
#include "StTrackPing.hh"

#include "TH2.h"
#include "TFile.h"
#include "TTree.h"

#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StTpcHitCollection.h"
#include "StContainers.h"
#include "StTrackNode.h"
#include "StTrack.h"
#include "StTrackDetectorInfo.h"
#include "StTpcHit.h"
#include "StTrackGeometry.h"

// //#include "StMemoryInfo.hh"
// #include "StParticleDefinition.hh"
// #include "StParticleTable.hh"
// #include "StGlobals.hh"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
size_t buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,map<StHit*,StTrack*>& htMap);
float getTpcDedx(StTrack* trk);

static const char rcsid[] = "$Id: StTrackMateMaker.cxx,v 1.1 2004/09/13 22:04:53 calderon Exp $";
ClassImp(StTrackMateMaker)

StTrackMateMaker::StTrackMateMaker(const char *name): StMaker(name)
{
    // if this line is not present, causes a seg fault?
    cout << "StTrackMateMaker::StTrackMateMaker() not zeroing pointer" << endl;
//     mCorrectionAccPeriphAll = 0;

}
StTrackMateMaker::~StTrackMateMaker() { /* nopt */ }
Int_t StTrackMateMaker::Init()
{
    cout << "StTrackMateMaker::Init() - creating histogram" << endl;
    mPtDiff = new TH2D("mPtDiff","mPtDiff",40,0,4,160,-1,1);
    mPtDiff->Sumw2();
    TString names = "tptPt/F:ittfPt/F:tptP/F:ittfP/F:tptFitPts/F:ittfFitPts/F:tptDedx/F:ittfDedx/F";
    trackTree = new TTree("trackMateComp","trackMateComp");
    trackTree->Branch("data_array",output,names.Data());
    cout << "StTrackMateMaker::Init() - successful" << endl;
    
    return StMaker::Init();
}
void StTrackMateMaker::Clear(const char* c)
{
    return StMaker::Clear(c);
}

Int_t StTrackMateMaker::Make(){
    cout << "In StTrackMateMaker::Make " << endl;
    StEvent* rEvent1 = 0;
    StEvent* rEvent2 = 0;
    
    
    rEvent1 = (StEvent*) GetDataSet("IO1/.make/IO1_Root/.data/bfcTree/eventBranch/StEvent");
    rEvent2 = (StEvent*) GetDataSet("IO2/.make/IO2_Root/.data/bfcTree/eventBranch/StEvent");

    cout << "Pointers obtained from GetDataSet" << endl;
    cout << "Event1 At: " << rEvent1 << endl;
    cout << "Event2 At: " << rEvent2 << endl;

    if (!rEvent1 || !rEvent2) {
	cout << "Bailing out! One of the StEvent's is missing!" << endl;
	return kStOk;
    }
    cout << "Run # and Event #, should be the same for both StEvents" << endl;
    cout << "Event1: Run "<< rEvent1->runId() << " Event1 No: " << rEvent1->id() << endl;
    cout << "Event2: Run "<< rEvent2->runId() << " Event2 No: " << rEvent2->id() << endl;
    cout << "Vertex Positions" << endl;
    if (rEvent1->primaryVertex() ) {
	cout << "Event1: Vertex Position " << rEvent1->primaryVertex()->position() << endl;
    }
    else {
	cout << "Event1: Vertex Not Found" << endl;
    }
    if (rEvent2->primaryVertex() ) {
	cout << "Event2: Vertex Position " << rEvent2->primaryVertex()->position() << endl;
    }
    else {
	cout << "Event2: Vertex Not Found" << endl;
    }

    cout << "Size of track containers" << endl;
    const StSPtrVecTrackNode& trackNodes1 = rEvent1->trackNodes();
    cout << "Event1: Track Nodes " << trackNodes1.size() << endl;

    const StSPtrVecTrackNode& trackNodes2 = rEvent2->trackNodes();
    cout << "Event2: Track Nodes " << trackNodes2.size() << endl;

    cout << "Tpc Hits" << endl;
    // Note:
    // For StTpcHits: sector = [1-24], padrow = [1-45]
    // For StTpcHitCollections: sector [0-23], padrow = [0,44]
    const StTpcHitCollection* tpchitcoll1 = rEvent1->tpcHitCollection();
    const StTpcHitCollection* tpchitcoll2 = rEvent2->tpcHitCollection();
    if (Debug()>2) {
	for (size_t iSec=0; iSec<tpchitcoll1->numberOfSectors(); ++iSec) { // [0,23]
	    const StTpcSectorHitCollection* sectorcoll1 = tpchitcoll1->sector(iSec);
	    const StTpcSectorHitCollection* sectorcoll2 = tpchitcoll2->sector(iSec);
	    for (size_t iPadR=0; iPadR<sectorcoll1->numberOfPadrows(); ++iPadR) { //[0,44]
		const StTpcPadrowHitCollection* padrowcoll1 = sectorcoll1->padrow(iPadR);
		const StTpcPadrowHitCollection* padrowcoll2 = sectorcoll2->padrow(iPadR);
		cout << "Sector(+1) " << iSec+1 << ", Padrow(+1) " << iPadR+1 << endl;
		cout << "hits1 " << padrowcoll1->hits().size() << endl;
		cout << "hits2 " << padrowcoll2->hits().size() << endl;
		if (padrowcoll1->hits().size()) {
		    cout << "hits1[0] position " << padrowcoll1->hits()[0]->position() << endl;
		    cout << "hits1[0] position " << padrowcoll2->hits()[0]->position() << endl;
		}
		// Here we assume (CHECK!) that the hits are the same for both events.
		// So we don't need to associate them.  That is, if there is are hits in this
		// padrow, then the hits in one event and the hits in the other event
		// are the same and are stored in the same order in the container.
		// e.g. padrowcoll1->hits()[0] represents the same hit as
		//      padrowcoll2->hits()[0].
	    }// padrow loop
	}// sector loop
    }//Debug hits in tpc for both events

    
    // It is easy in StEvent to go from a track to its hits, but it is
    // rather time consuming to call StHit::relatedTracks several times
    // as this will loop over all tracks, then for each track loop over all its
    // hits.  It is better to construct a way to navigate from hit to track in an
    // easy way.  We can do it via a map.  We don't need a multimap, to keep it
    // simple, we will only match one hit to one track, not one hit to many tracks.
    // Neither tracker allows this anyway.  We can check this by counting the
    // number of failed insertions into the map, if everything is Ok, it should be 0.
    // So we can loop over the StTrack::hits(kTpcId) and build the maps once.
    // We only need to build one map, because we will connect the two track via
    // - track 1 to hit 1, each track knows about its hits
    // - hit 1 to hit 2, assuming they are in the same sector, padrow and have the same index
    // - hit 2 to track 2, using the map we will build next

    //map<StHit*,StTrack*> hitTrackMap1;
    //buildRecHitTrackMap(trackNodes1,hitTrackMap1);
    map<StHit*,StTrack*> hitTrackMap2;
    size_t failedTries = buildRecHitTrackMap(trackNodes2,hitTrackMap2);
    cout << "Hits used by more than 1 track: " << failedTries << endl;
    
    // Do Track association.
    cout << "Begin Track Association..." << endl;
    int matcTrkCounter = 0;
    int origTrkCounter = 0;

    // Algorithm is a simplified version of the one in StAssociationMaker
    // since we don't need to worry about merged tracks, or splitting, or
    // multiple matches.  Just worry about the simple case of one-to-one matching.
    
    // Loop over tracks in event 1.
    //   Loop over hits.
    //     This is the same hit as in event 2.
    //     Use hitTrackMap2 to find candidate track.
    //   Call a match if there is a candidate.  For the cases where there
    //   are more than 1 candidates, only match the track with most hits in common.
    //
    StTrackPing initTrackPing;
    initTrackPing.mTrack=0;
    initTrackPing.mNPings=0;

    // Keep a set of the StTracks from event 2 that are associated,
    // so that at the end we can loop over the track container in event
    // 2 and find out the tracks that were not associated from event 2

    set<StTrack*> assocTracks2;
    for (size_t iTrk=0; iTrk<trackNodes1.size();++iTrk) {
	StTrack* trk1 = trackNodes1[iTrk]->track(global);
	if (!trk1 || trk1->flag()<=0) continue;
	if (Debug()>2) cout << "Processing Track " << iTrk << endl;
	++origTrkCounter;
	vector<StTrackPing> candidates(20,initTrackPing); //make sure it's filled with null pointers and zeros
	size_t nCandidates = 0;
	StPtrVecHit trkhits1 = trk1->detectorInfo()->hits(kTpcId);
	for (StPtrVecHitIterator hIterTrk = trkhits1.begin(); hIterTrk != trkhits1.end(); ++hIterTrk) {
	    // get the hit from the track
	    const StTpcHit* hit1 = static_cast<const StTpcHit*>(*hIterTrk);
	    // go to the hit collection and get the hit container for its sector,padrow
	    const StSPtrVecTpcHit& hits1 = tpchitcoll1->sector(hit1->sector()-1)->padrow(hit1->padrow()-1)->hits();
	    // find the hit in this container and get the index
	    StPtrVecTpcHitConstIterator hIterColl =  find(hits1.begin(),hits1.end(),hit1);
	    int index = hIterColl - hits1.begin();
	    // get the hit container for the sector,padrow in event 2
	    // using the SAME sector, padrow as for hit 1
	    const StSPtrVecTpcHit& hits2 = tpchitcoll2->sector(hit1->sector()-1)->padrow(hit1->padrow()-1)->hits();
	    // use the index found above to find the hit in event 2
	    StTpcHit* hit2 = hits2[index];
	    // use the map to find its track!
	    StTrack* trackCand = hitTrackMap2[hit2];
	    if (!trackCand) {
		// the hit is not on a track in the 2nd event
		continue;
	    }
	    // At this point we have a candidate StTrack
	    // If there are no candidates, create the first candidate.
	    // If already there, increment its nPings.
	    // If doesn't match any of the previous candidates, create new candidate.
	    
	    if (nCandidates == 0) {
		candidates[0].mTrack   = trackCand;
		candidates[0].mNPings  = 1;
		nCandidates++;
		
	    }//if, case of no tracks in candidate vector
	    else {
		for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
		    if (trackCand==candidates[iCandidate].mTrack){
			candidates[iCandidate].mNPings++;
			break;
		    }
		    if (iCandidate == (nCandidates-1)){
			candidates[nCandidates].mTrack  = trackCand;
			candidates[nCandidates].mNPings = 1;
			nCandidates++;
			// check that we don't overstep the bounds,
			// if so increase the size of the vector in steps of 20 candidates
			if (nCandidates>=candidates.size()) {
			    candidates.resize(nCandidates+20);
			    if (Debug()) cout << "Resizing in the TPC hits of the track " << endl;
			}
			break;
		    }
		} // candidate loop
	    }//else, case of tracks in candidate vector
	    
	}// trk1 hit loop

	// at this point, we have all possible track candidates
	// based on the tpc hits of trk1.
	// find candidate with most common tpc hits,
	// could rewrite using
	// max_element...
	if (!nCandidates) {
	    // the track was not found in the 2nd event!
	    const StThreeVectorF& mom1 = trk1->geometry()->momentum();
	    output[0] = mom1.perp(); // ptTptg;
	    output[1] = -9999;       // ptIttf;
	    output[2] = mom1.mag();  //  pTptg;
	    output[3] = -9999;       //  pIttf;
	    output[4] = trk1->fitTraits().numberOfFitPoints(kTpcId); //fitPtsTptg;
	    output[5] = -9999;                                       //fitPtsIttf;
	    output[6] = getTpcDedx(trk1);
	    output[7] = -9999;
	    output[8] = 0;
	    trackTree->Fill();
	    continue;
	}
	vector<StTrackPing>::iterator max = max_element(candidates.begin(),candidates.end(),compStTrackPing);
	
	if (Debug()>2) {
	    cout << "Matching track " << (*max).mTrack << " has " << (*max).mNPings << ", index " << max-candidates.begin() << endl;
	}
	
	StTrack* trk2 = (*max).mTrack;
	if (trk2->flag()<=0) continue;
	++matcTrkCounter;
	assocTracks2.insert(trk2);
	// Ok! Now we found a pair!
	// So now we can make histograms and compare
	// momentum... add code below.
	const StThreeVectorF& mom1 = trk1->geometry()->momentum();
	const StThreeVectorF& mom2 = trk2->geometry()->momentum();
	output[0] = mom1.perp();// ptTptg;
	output[1] = mom2.perp();// ptIttf;
	output[2] = mom1.mag(); //  pTptg;
	output[3] = mom2.mag(); //  pIttf;
	output[4] = trk1->fitTraits().numberOfFitPoints(kTpcId); //fitPtsTptg;
	output[5] = trk2->fitTraits().numberOfFitPoints(kTpcId); //fitPtsIttf;
	output[6] = getTpcDedx(trk1);
	output[7] = getTpcDedx(trk2);
	output[8] = (*max).mNPings;
	trackTree->Fill();

    }//track loop

    // simple track loop to count ittf global tracks with flag>0
    int stigTrkCounter = 0;
    for (size_t iTrk=0; iTrk<trackNodes2.size();++iTrk) {
	StTrack* trk2 = trackNodes2[iTrk]->track(global);
	if (!trk2 || trk2->flag()<=0) continue;
	++stigTrkCounter;
	// Now try to see if this track was associated
	set<StTrack*>::iterator itTrk2 = find(assocTracks2.begin(),assocTracks2.end(),trk2);
	if (itTrk2==assocTracks2.end()) {
	    // Track was not associated.  Enter it into output tree
	    const StThreeVectorF& mom2 = trk2->geometry()->momentum();
	    output[0] = -9999;
	    output[1] = mom2.perp(); 
	    output[2] = -9999;       
	    output[3] = mom2.mag();  
	    output[4] = -9999;                                       
	    output[5] = trk2->fitTraits().numberOfFitPoints(kTpcId); 
	    output[6] = -9999;
	    output[7] = getTpcDedx(trk2);
	    output[8] = 0;
	    trackTree->Fill();
	    
	}
    }    
    if (Debug()) {
	cout << "Tpt+EGR  Global Tracks (flag>0) " << origTrkCounter << endl;
	cout << "Sti      Global Tracks (flag>0) " << stigTrkCounter << endl;
	cout << "Matched  Global Tracks (flag>0) " << matcTrkCounter << endl;
    }
//     int stiPTrkCounter = 0;
//     int oriPTrkCounter = 0;
//     int ftpGTrkCounter = 0;
//     int ftpPTrkCounter = 0;
//     for (const_StTrackNodeIterator iter = trackNodes.begin(); iter != trackNodes.end(); iter++) {
// 	StTrackNode* node = *iter;
// 	StTrack* track = node->track(global);
// 	if (track->flag()<=0) continue;
// 	int nFitPts = track->fitTraits().numberOfFitPoints(kTpcId);



	
    return kStOK;
}
Int_t StTrackMateMaker::Finish()
{
    TString fileName("/trackMateFile");
    fileName.Append(mFileIndex);
    fileName.Append(".root");
    fileName.Prepend(mOutDir);
    cout << "Writing " << fileName << endl;
    TFile* dummyFile = new TFile(fileName.Data(),"RECREATE");
    //mPtDiff->Write(); //! 
    trackTree->Write();
    dummyFile->Close();
    
    return kStOK;
}
