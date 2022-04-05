//
// $Id: StTrackMateMaker.cxx,v 1.4 2013/01/16 21:56:45 fisyak Exp $
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
#include "StPrimaryTrack.h"
#include "StTrack.h"
#include "StTrackDetectorInfo.h"
#include "StTpcHit.h"
#include "StTrackGeometry.h"

#include "StEventUtilities/StuRefMult.hh"
#include "StMessMgr.h"

// //#include "StMemoryInfo.hh"
// #include "StParticleDefinition.hh"
// #include "StParticleTable.hh"
// #include "StGlobals.hh"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
size_t buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,map<StHit*,StTrack*>& htMap);
float getTpcDedx(StTrack* trk);

static const char rcsid[] = "$Id: StTrackMateMaker.cxx,v 1.4 2013/01/16 21:56:45 fisyak Exp $";
ClassImp(StTrackMateMaker)
// tpt => old
// sti => new

TString names("oldPtGl/F:newPtGl/F:oldEtaGl/F:newEtaGl/F:oldPhiGl/F:newPhiGl/F:oldPGl/F:newPGl/F:oldFitPtsGl/F:"
	      "newFitPtsGl/F:oldPtPr/F:newPtPr/F:oldEtaPr/F:newEtaPr/F:oldPhiPr/F:newPhiPr/F:oldPPr/F:newPPr/F:"
	      "oldFitPtsPr/F:newFitPtsPr/F:oldDedx/F:newDedx/F:oldCharge/F:newCharge/F:maxPing/F:Prim/F:"
	      "oldChi2Gl0/F:newChi2Gl0/F:oldChi2Gl1/F:newChi2Gl1/F:oldChi2Pr0/F:newChi2Pr0/F:oldChi2Pr1/F:"
	      "newChi2Pr1/F:firstHitsDist/F:lastHitsDist/F:"
	      "oldPrimX/F:oldPrimY/F:oldPrimZ/F:newPrimX/F:newPrimY/F:newPrimZ/F");
struct mc_data_array {
  Char_t begin;
  float oldPtGl;
  float newPtGl;
  float oldEtaGl;
  float newEtaGl;
  float oldPhiGl;
  float newPhiGl;
  float oldPGl;
  float newPGl;
  float oldFitPtsGl;
  float newFitPtsGl;
  float oldPtPr;
  float newPtPr;
  float oldEtaPr;
  float newEtaPr;
  float oldPhiPr;
  float newPhiPr;
  float oldPPr;
  float newPPr;
  float oldFitPtsPr;
  float newFitPtsPr;
  float oldDedx;
  float newDedx;
  float oldCharge;
  float newCharge;
  float maxPing;
  float Prim;
  float oldChi2Gl0;
  float newChi2Gl0;
  float oldChi2Gl1;
  float newChi2Gl1;
  float oldChi2Pr0;
  float newChi2Pr0;
  float oldChi2Pr1;
  float newChi2Pr1;
  float firstHitsDist;
  float lastHitsDist;
  float oldPrimX;
  float oldPrimY;
  float oldPrimZ;
  float newPrimX;
  float newPrimY;
  float newPrimZ;
  Char_t end;
 public:
  void set() {memset(&begin, 0, &end-&begin);}
};
mc_data_array data;    
//________________________________________________________________________________
Int_t StTrackMateMaker::Init() {
  TFile *f = GetTFile();
  if (f) {
    f->cd();
  }
  LOG_INFO << "StTrackMateMaker::Init() - creating histogram" << endm;
  TString evNames = "refMult/F";
  trackTree = new TTree("trackMateComp","trackMateComp");
  trackBr = trackTree->Branch("data_array",&data.oldPtGl,names.Data());
  eventBr = trackTree->Branch("ev_array",evOutput,evNames.Data());
  LOG_INFO << "StTrackMateMaker::Init() - successful" << endm;
  
  return StMaker::Init();
}
//________________________________________________________________________________
void StTrackMateMaker::Clear(const char* c)
{
    return StMaker::Clear(c);
}
//________________________________________________________________________________
Int_t StTrackMateMaker::Make(){
    LOG_INFO << "In StTrackMateMaker::Make " << endm;
    StEvent* rEvent1 = 0;
    StEvent* rEvent2 = 0;
    
    
    rEvent1 = (StEvent*) GetDataSet("IO1/.make/IO1_Root/.data/bfcTree/eventBranch/StEvent");
    rEvent2 = (StEvent*) GetDataSet("IO2/.make/IO2_Root/.data/bfcTree/eventBranch/StEvent");

    LOG_INFO << "Pointers obtained from GetDataSet" << endm;
    LOG_INFO << "Event1 At: " << rEvent1 << endm;
    LOG_INFO << "Event2 At: " << rEvent2 << endm;

    if (!rEvent1 || !rEvent2) {
      LOG_WARN << "Bailing out! One of the StEvent's is missing!" << endm;
      return kStWarn;
    }
    LOG_INFO << "Run # and Event #, should be the same for both StEvents" << endm;
    LOG_INFO << "Event1: Run "<< rEvent1->runId() << " Event1 No: " << rEvent1->id() << endm;
    LOG_INFO << "Event2: Run "<< rEvent2->runId() << " Event2 No: " << rEvent2->id() << endm;
    LOG_INFO << "Vertex Positions" << endm;
    assert (rEvent1->runId() == rEvent2->runId() && rEvent1->id() == rEvent2->id());
    if (rEvent1->primaryVertex() ) {
	LOG_INFO << "Event1: Vertex Position " << rEvent1->primaryVertex()->position() << endm;
    }
    else {
	LOG_INFO << "Event1: Vertex Not Found" << endm;
    }
    if (rEvent2->primaryVertex() ) {
	LOG_INFO << "Event2: Vertex Position " << rEvent2->primaryVertex()->position() << endm;
    }
    else {
	LOG_INFO << "Event2: Vertex Not Found" << endm;
    }
#if 0
    if (rEvent1->runInfo()->spaceChargeCorrectionMode() != 21 ||
	rEvent2->runInfo()->spaceChargeCorrectionMode() != 21) {
      LOG_INFO << "Event1:  spaceChargeCorrectionMode "
			 << rEvent1->runInfo()->spaceChargeCorrectionMode() << endm;
      LOG_INFO << "Event2:  spaceChargeCorrectionMode "
			 << rEvent2->runInfo()->spaceChargeCorrectionMode() << endm;
      LOG_INFO << " Skip it " << endm;
      return kStWarn;
    }
#endif
    LOG_INFO << "Size of track containers" << endm;
    const StSPtrVecTrackNode& trackNodes1 = rEvent1->trackNodes();
    LOG_INFO << "Event1: Track Nodes " << trackNodes1.size() << endm;

    const StSPtrVecTrackNode& trackNodes2 = rEvent2->trackNodes();
    LOG_INFO << "Event2: Track Nodes " << trackNodes2.size() << endm;
    if (! trackNodes1.size() || ! trackNodes2.size()) return kStWarn;
    //eventwise info
    evOutput[0] = uncorrectedNumberOfPrimaries(*rEvent2);
    //eventBr->Fill();
    LOG_INFO << "Tpc Hits" << endm;
    // Note:
    // For StTpcHits: sector = [1-24], padrow = [1-45]
    // For StTpcHitCollections: sector [0-23], padrow = [0,44]
    const StTpcHitCollection* tpchitcoll1 = rEvent1->tpcHitCollection();
    const StTpcHitCollection* tpchitcoll2 = rEvent2->tpcHitCollection();
    if (! tpchitcoll1 || ! tpchitcoll2) {
      LOG_WARN << "Empty tpc hit collection in one of the events" << endm;
      return kStWarn;
    }
    if (Debug()>2) {
	for (size_t iSec=0; iSec<tpchitcoll1->numberOfSectors(); ++iSec) { // [0,23]
	    const StTpcSectorHitCollection* sectorcoll1 = tpchitcoll1->sector(iSec);
	    const StTpcSectorHitCollection* sectorcoll2 = tpchitcoll2->sector(iSec);
	    for (size_t iPadR=0; iPadR<sectorcoll1->numberOfPadrows(); ++iPadR) { //[0,44]
		const StTpcPadrowHitCollection* padrowcoll1 = sectorcoll1->padrow(iPadR);
		const StTpcPadrowHitCollection* padrowcoll2 = sectorcoll2->padrow(iPadR);
		LOG_INFO << "Sector(+1) " << iSec+1 << ", Padrow(+1) " << iPadR+1 << endm;
		LOG_INFO << "hits1 " << padrowcoll1->hits().size() << endm;
		LOG_INFO << "hits2 " << padrowcoll2->hits().size() << endm;
		if (padrowcoll1->hits().size()) {
		    LOG_INFO << "hits1[0] position " << padrowcoll1->hits()[0]->position() << endm;
		    LOG_INFO << "hits2[0] position " << padrowcoll2->hits()[0]->position() << endm;
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
    LOG_INFO << "Hits used by more than 1 track: " << failedTries << endm;
    
    // Do Track association.
    LOG_INFO << "Begin Track Association..." << endm;
    int matcTrkCounter = 0;
    int origTrkCounter = 0;
    int oldOnlyCounter = 0;
    int oldDoubCounter = 0;
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
	if (!trk1 || trk1->flag()<=0 || trk1->topologyMap().trackFtpc()) continue;
	StTrack* ptrk1 = trackNodes1[iTrk]->track(primary);
	if (Debug()>2) LOG_INFO << "Processing Track " << iTrk << endm;
	++origTrkCounter;
	vector<StTrackPing> candidates(20,initTrackPing); //make sure it's filled with null pointers and zeros
	size_t nCandidates = 0;
	if (! trk1->detectorInfo()) continue;
	StPtrVecHit trkhits1 = trk1->detectorInfo()->hits(kTpcId);
	
	for (StPtrVecHitIterator hIterTrk = trkhits1.begin(); hIterTrk != trkhits1.end(); ++hIterTrk) {
	    // get the hit from the track
	    const StTpcHit* hit1 = static_cast<const StTpcHit*>(*hIterTrk);
	    // go to the hit collection and get the hit container for its sector,padrow
	    if (! tpchitcoll1->sector(hit1->sector()-1)->padrow(hit1->padrow()-1)) continue;
	    const StSPtrVecTpcHit& hits1 = tpchitcoll1->sector(hit1->sector()-1)->padrow(hit1->padrow()-1)->hits();
	    if (! hits1.size()) continue;
	    // find the hit in this container and get the index
	    StPtrVecTpcHitConstIterator hIterColl =  find(hits1.begin(),hits1.end(),hit1);
	    int index = hIterColl - hits1.begin();
	    // get the hit container for the sector,padrow in event 2
	    // using the SAME sector, padrow as for hit 1
	    if (! tpchitcoll2->sector(hit1->sector()-1)->padrow(hit1->padrow()-1)) continue;
	    const StSPtrVecTpcHit& hits2 = tpchitcoll2->sector(hit1->sector()-1)->padrow(hit1->padrow()-1)->hits();
	    if (! hits2.size()) continue;
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
			    if (Debug()) LOG_INFO << "Resizing in the TPC hits of the track " << endm;
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
	    ++oldOnlyCounter;
	    Fill(trk1,ptrk1,0,0,0);
	    continue;
	}
	vector<StTrackPing>::iterator max = max_element(candidates.begin(),candidates.end(),compStTrackPing);
	
	if (Debug()>2) {
	    LOG_INFO << "Matching track " << (*max).mTrack << " has " << (*max).mNPings << ", index " << max-candidates.begin() << endm;
	}
	
	StTrack* trk2 = (*max).mTrack;
	StTrack* ptrk2 = trk2->node()->track(primary);
	if (trk2->flag()<=0 || trk2->topologyMap().trackFtpc()) {
	    ++oldOnlyCounter;
	    Fill(trk1,ptrk1,0,0,-1);
	    continue;
	    
	}
	if (assocTracks2.find(trk2)!=assocTracks2.end()) {
	    //This is a match to an New track that has already been matched.
	    // We can enter the old track and flag this double match by
	    // setting the fit points to be negative.
	    ++oldDoubCounter;
	    Fill(trk1,ptrk1,trk2,ptrk2,(*max).mNPings);
	    continue;
	}
	// Ok! Now we found a pair if we're here!	
	++matcTrkCounter;
	assocTracks2.insert(trk2);
	Fill(trk1,ptrk1,trk2,ptrk2,(*max).mNPings);
    }//track loop

    // simple track loop to count new global tracks with flag>0
    int newgTrkCounter = 0;
    int newOnlyCounter = 0;    
    for (size_t iTrk=0; iTrk<trackNodes2.size();++iTrk) {
	StTrack* trk2 = trackNodes2[iTrk]->track(global);
	if (!trk2 || trk2->flag()<=0 || trk2->topologyMap().trackFtpc()) continue;
	++newgTrkCounter;
	// Now try to see if this track was associated
	set<StTrack*>::iterator itTrk2 = find(assocTracks2.begin(),assocTracks2.end(),trk2);
	if (itTrk2==assocTracks2.end()) {
	    ++newOnlyCounter;    	    
	    StTrack* ptrk2 = 0;
	    if (trk2->node()) ptrk2 = trk2->node()->track(primary);
	    Fill(0,0,trk2,ptrk2,-2);
	}
    }    
    if (Debug()) {
	LOG_INFO << "Old+EGR  Global Tracks (flag>0) " << origTrkCounter << endm;
	LOG_INFO << "New      Global Tracks (flag>0) " << newgTrkCounter << endm;
	LOG_INFO << "Matched  Global Tracks (flag>0) " << matcTrkCounter << endm;
	LOG_INFO << "Old+EGR  Only   Tracks (flag>0) " << oldOnlyCounter << endm;
	LOG_INFO << "Old+EGR  Double Matches to New  " << oldDoubCounter << endm;
	LOG_INFO << "size of Set, should = Matched.. " << assocTracks2.size() << endm; 
	LOG_INFO << "New      Only   Tracks (flag>0) " << newOnlyCounter << endm;
    }
	
    return kStOK;
}
//________________________________________________________________________________
void StTrackMateMaker::Fill(StTrack* trk1, StTrack* ptrk1,StTrack* trk2, StTrack* ptrk2,Int_t maxPing) {
  data.set();
  data.firstHitsDist = data.lastHitsDist = -999.;
  if (trk1) {
    const StThreeVectorF& mom1 = trk1->geometry()->momentum();
    data.oldPtGl = mom1.perp(); 
    data.oldEtaGl = mom1.pseudoRapidity();
    data.oldPhiGl = mom1.phi();
    data.oldPGl = mom1.mag();
    data.oldFitPtsGl = trk1->fitTraits().numberOfFitPoints(kTpcId);
    data.oldDedx = getTpcDedx(trk1);
    data.oldCharge = trk1->geometry()->charge();
    data.oldChi2Gl0 = trk1->fitTraits().chi2(0);
    data.oldChi2Gl1 = trk1->fitTraits().chi2(1);
    data.maxPing = 0;
    if (ptrk1) {
      const StThreeVectorF& pmom1 = ptrk1->geometry()->momentum();
      data.oldPtPr  = pmom1.perp(); 
      data.oldEtaPr = pmom1.pseudoRapidity();
      data.oldPhiPr = pmom1.phi();
      data.oldPPr   = pmom1.mag();
      data.oldFitPtsPr = trk1->fitTraits().numberOfFitPoints(kTpcId);
      data.Prim    += 1;
      data.oldChi2Pr0 = ptrk1->fitTraits().chi2(0);
      data.oldChi2Pr1 = ptrk1->fitTraits().chi2(1);
      StPrimaryTrack *prim = (StPrimaryTrack *) ptrk1;
      const StVertex *vertex = prim->vertex();
      if (vertex) {
	data.oldPrimX = vertex->position().x();
	data.oldPrimY = vertex->position().y();
	data.oldPrimZ = vertex->position().z();
      }
    }
  }
  if (trk2) {
    const StThreeVectorF& mom2 = trk2->geometry()->momentum();
    data.newPtGl  = mom2.perp();
    data.newEtaGl = mom2.pseudoRapidity();
    data.newPhiGl = mom2.phi();
    data.newPGl   = mom2.mag();
    data.newFitPtsGl = trk2->fitTraits().numberOfFitPoints(kTpcId);
    data.newDedx = getTpcDedx(trk2);
    data.newCharge = trk2->geometry()->charge();
    data.newChi2Gl0 = trk2->fitTraits().chi2(0);
    data.newChi2Gl1 = trk2->fitTraits().chi2(1);
    data.maxPing = 0;
    if (ptrk2) {
      const StThreeVectorF& pmom2 = ptrk2->geometry()->momentum();
      data.newPtPr  = pmom2.perp();
      data.newEtaPr = pmom2.pseudoRapidity();
      data.newPhiPr = pmom2.phi();
      data.newPPr   = pmom2.mag();
      data.newFitPtsPr = trk2->fitTraits().numberOfFitPoints(kTpcId);
      data.Prim    += 10;
      data.newChi2Pr0 = ptrk2->fitTraits().chi2(0);
      data.newChi2Pr1 = ptrk2->fitTraits().chi2(1);
      StPrimaryTrack *prim = (StPrimaryTrack *) ptrk2;
      const StVertex *vertex = prim->vertex();
      if (vertex) {
	data.newPrimX = vertex->position().x();
	data.newPrimY = vertex->position().y();
	data.newPrimZ = vertex->position().z();
      }
    }
  }
  data.maxPing = maxPing;
  if (trk1 && trk2) {
    StTrackDetectorInfo *det1 = trk1->detectorInfo();
    StTrackDetectorInfo *det2 = trk2->detectorInfo();
    if (det1 && det2) {
      StThreeVectorF difFirst = det1->firstPoint() - det2->firstPoint();
      data.firstHitsDist = difFirst.mag();
      StThreeVectorF difLast = det1->lastPoint() - det2->lastPoint();
      data.lastHitsDist = difLast.mag();
    }
  }
  trackTree->Fill();
}
