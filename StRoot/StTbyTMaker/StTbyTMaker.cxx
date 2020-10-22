//
// $Id: StTbyTMaker.cxx,v 1.4 2013/01/16 21:56:45 fisyak Exp $
//
#include <iostream>
#include <algorithm>
#include <set>

#include "StTbyTMaker.h"
#include "StContainers.h"
#include "StTpcHitCollection.h"
#include "StEvent.h"
#include "StPrimaryVertex.h"
#include "StTrackNode.h"
#include "StPrimaryTrack.h"
#include "StTrackGeometry.h"
#include "StDcaGeometry.h"
#include "StDedxPidTraits.h"
#include "StTrackDetectorInfo.h"
#include "StGlobalTrack.h"
#include "StTrack.h"
#include "StTpcHit.h"
#include "StMessMgr.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
#include "StDbUtilities/StCoordinates.hh" 
#define __DEBUG__
#if defined(__DEBUG__)
#define PrPP(A,B) if (Debug()%10 > 2) {LOG_INFO << "StTrackMaterMaker::" << (#A) << "\t" << (#B) << " = \t" << (B) << endm;}
#else
#define PrPP(A,B)
#endif

// //#include "StMemoryInfo.hh"
// #include "StParticleDefinition.hh"
// #include "StParticleTable.hh"
// #include "StGlobals.hh"

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif
struct CompareHits {
  bool operator() (StTpcHit* hit1,StTpcHit* hit2) { 
    if (! hit1) return kTRUE;
    return hit1->Compare(hit2);}
} comp;
bool compStTrackPing(const StTrackPing& rhs, const StTrackPing& lhs){
    return rhs.mNPings>lhs.mNPings;
}

static const char rcsid[] = "$Id: StTbyTMaker.cxx,v 1.4 2013/01/16 21:56:45 fisyak Exp $";
ClassImp(StTbyTMaker);
//________________________________________________________________________________
Int_t StTbyTMaker::Init() {
  TFile *f = GetTFile();
  if (f) {
    f->cd();
  }
  LOG_INFO << "StTbyTMaker::Init() - creating histogram" << endm;
  trackTree = new TTree("trackMateComp","trackMateComp");
  fTrackMatch = new TrackMatch;
  trackBr = trackTree->Branch("TrackMatch",&fTrackMatch);
#define __HIT_MATCH__
#ifdef __HIT_MATCH__
  hitTree = new TTree("hitMateComp","hitMateComp");
  fHitMatch = new HitMatch;
  hitBr = hitTree->Branch("HitMatch",&fHitMatch);
#endif /* __HIT_MATCH__ */
  LOG_INFO << "StTbyTMaker::Init() - successful" << endm;
  
  return StMaker::Init();
}
//________________________________________________________________________________
void StTbyTMaker::Clear(const char* c)
{
  return StMaker::Clear(c);
}
//________________________________________________________________________________
Bool_t StTbyTMaker::GoodTrack(StTrack* trk) {
  if (! trk || trk->flag()<=0 || trk->topologyMap().trackFtpc()) return kFALSE;
  if (! trk->detectorInfo())  return kFALSE;
  if ( trk->fitTraits().numberOfFitPoints(kTpcId) < 15) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t StTbyTMaker::GoodMatch(StTrack* trk1, StTrack* trk2, UInt_t NPings) {
  if (NPings < 5) return kFALSE;
  if (! trk1)    return kFALSE;
  if (! trk2)    return kFALSE;
  UInt_t minFit = 0.5*TMath::Min(trk1->fitTraits().numberOfFitPoints(kTpcId),trk2->fitTraits().numberOfFitPoints(kTpcId));
  if (NPings < minFit){
    if (Debug() >= 10) {
      StTrack *tracks[2] = {trk1, trk2};
      for (Int_t i = 0; i < 2; i++) {
        StPtrVecHit hits = tracks[i]->detectorInfo()->hits(kTpcId);
	UInt_t Nhits = hits.size(); 
	for (UInt_t j = 0; j < Nhits; j++) {
	  cout << "track" << i+1 << " hit" << j+1 << *hits[j] << endl;
	}
      }
    }
    return kFALSE;
  }
  return kTRUE;
}
//________________________________________________________________________________
Int_t StTbyTMaker::Make(){
  LOG_INFO << "In StTbyTMaker::Make " << endm;
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
  assert (rEvent1->runId() == rEvent2->runId() && rEvent1->id() == rEvent2->id());
  LOG_INFO << "Vertex Positions" << endm;
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
  LOG_INFO << "Size of track containers";
  const StSPtrVecTrackNode& trackNodes1 = rEvent1->trackNodes();
  LOG_INFO << "\tEvent1: Track Nodes " << trackNodes1.size();
  
  const StSPtrVecTrackNode& trackNodes2 = rEvent2->trackNodes();
  LOG_INFO << "\tEvent2: Track Nodes " << trackNodes2.size() << endm;
  if (! trackNodes1.size() || ! trackNodes2.size()) return kStWarn;
  //eventwise info
  //  evOutput[0] = uncorrectedNumberOfPrimaries(*rEvent2);
  //eventBr->Fill();
  // Note:
  // For StTpcHits: sector = [1-24], padrow = [1-72]
  // For StTpcHitCollections: sector [0-23], padrow = [0,71]
  const StTpcHitCollection* tpchitcoll1 = rEvent1->tpcHitCollection();
  const StTpcHitCollection* tpchitcoll2 = rEvent2->tpcHitCollection();
  if (! tpchitcoll1 || ! tpchitcoll2) {
    LOG_WARN << "Empty tpc hit collection in one of the events" << endm;
    return kStWarn;
  }
  LOG_INFO << "Size of Tpc hit containers";
  LOG_INFO << "\tEvent1: " << tpchitcoll1->numberOfHits();
  LOG_INFO << "\tEvent2: " << tpchitcoll2->numberOfHits() << endm;
  if (Debug()>2) {
    for (size_t iSec=0; iSec<tpchitcoll1->numberOfSectors(); ++iSec) { // [0,23]
      const StTpcSectorHitCollection* sectorcoll1 = tpchitcoll1->sector(iSec);
      const StTpcSectorHitCollection* sectorcoll2 = tpchitcoll2->sector(iSec);
      for (size_t iPadR=0; iPadR<sectorcoll1->numberOfPadrows(); ++iPadR) { //[0,71]
	const StTpcPadrowHitCollection* padrowcoll1 = sectorcoll1->padrow(iPadR);
	const StTpcPadrowHitCollection* padrowcoll2 = sectorcoll2->padrow(iPadR);
	if (! padrowcoll1->hits().size() && ! padrowcoll2->hits().size()) continue;
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
  // rather time consuming to call StTpcHit::relatedTracks several times
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
  
  Hit2TrackMap hitTrackMap1;
  size_t failedTries1 = buildRecHitTrackMap(trackNodes1,hitTrackMap1);
  LOG_INFO << "Hits1 used by more than 1 track: " << failedTries1 << endm;
  Hit2TrackMap hitTrackMap2;
  size_t failedTries2 = buildRecHitTrackMap(trackNodes2,hitTrackMap2);
  LOG_INFO << "Hits2 used by more than 1 track: " << failedTries2 << endm;
  Hit2HitMap Hit2ToHit1;
  Hit2HitMap Hit1ToHit2;
  buildHit2HitMaps(tpchitcoll1,tpchitcoll2,Hit1ToHit2,Hit2ToHit1);
  // Do Track association.
  LOG_INFO << "Begin Track Association..." << endm;
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
  // Keep a set of the StTracks from event 2 that are associated,
  // so that at the end we can loop over the track container in event
  // 2 and find out the tracks that were not associated from event 2
  
  //  set<StGlobalTrack*> assocTracks2;
  Track2TrackMap Track1ToTrack2;
  Track2TrackMap Track2ToTrack1;
  buildTrack2TrackMap(trackNodes1, trackNodes2, Hit1ToHit2, hitTrackMap2, Track1ToTrack2);
  buildTrack2TrackMap(trackNodes2, trackNodes1, Hit2ToHit1, hitTrackMap1, Track2ToTrack1);
  checkConsistency(Track1ToTrack2,Track2ToTrack1);
  checkConsistency(Track2ToTrack1,Track1ToTrack2);
  // at this point, we have all possible track candidates
  // based on the tpc hits of trk1.
  // find candidate with most common tpc hits,
  // could rewrite using
  // max_element...
  // track1 => track2
  for (auto x1 : Track1ToTrack2) {
    StGlobalTrack *trk1 = x1.first;
    if (! trk1 ) continue;
    StGlobalTrack *trk2 = x1.second;
    if (! trk2 ) {
      FillMatch(trk1,0);
    } else {
      FillMatch(trk1,trk2);
    }
  }
  for (auto x2 : Track2ToTrack1) {
    StGlobalTrack *trk2 = x2.first;
    if (! trk2 ) continue;
    StGlobalTrack *trk1 = x2.second;
    if (! trk1 ) {
      FillMatch(0,trk2);
    } else if (trk1->flagExtension() == 0 || trk2->flagExtension() == 0) {
      FillMatch(trk1,trk2);
    }
  }
  return kStOK;
}
//________________________________________________________________________________
void StTbyTMaker::FillMatch(const StGlobalTrack* trk1, const StGlobalTrack* trk2) {
  fTrackMatch->oldP = TrackParametersFill(trk1);
  fTrackMatch->newP = TrackParametersFill(trk2);
  trackTree->Fill();
}
//________________________________________________________________________________
void StTbyTMaker::FillMatch(const StTpcHit* hit1,const  StTpcHit* hit2) {
  fHitMatch->oldP = HitParametersFill(hit1);
  fHitMatch->newP = HitParametersFill(hit2);
  if (Debug()) {
    cout << "newP\t"; fHitMatch->newP.Print();
    cout << "oldP\t"; fHitMatch->oldP.Print();
    if (fHitMatch->newP.sector < 0 || fHitMatch->newP.sector > 24 ||
	fHitMatch->oldP.sector < 0 || fHitMatch->oldP.sector > 24) {
      static Int_t ibreak = 0;
      ibreak++;
    }
  }
  hitTree->Fill();
}
//________________________________________________________________________________
size_t StTbyTMaker::buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,Hit2TrackMap& htMap){
  size_t failedInserts = 0;
  for (size_t it = 0; it<nodes.size(); ++it) {
    StGlobalTrack* track = (StGlobalTrack*)nodes[it]->track(global);
    if (! GoodTrack(track)) continue;
    track->setFlagExtension(0);
    StPtrVecHit hits = track->detectorInfo()->hits(kTpcId);
    for (StPtrVecHitIterator hIterTrk = hits.begin(); hIterTrk != hits.end(); ++hIterTrk) {
      StTpcHit* h = (StTpcHit *) *hIterTrk;
      if (! h) continue;
      htMap.insert(HitTrackPair(h,track));
    }//hits in track loop
  }// track loop
  return failedInserts;
}
//________________________________________________________________________________
void StTbyTMaker::buildHit2HitMaps(const StTpcHitCollection *tpchitcoll1, const StTpcHitCollection *tpchitcoll2,
		       Hit2HitMap &Hit1ToHit2,Hit2HitMap &Hit2ToHit1) {
  for (Int_t s = 0; s < 24; s++) {
    const StTpcSectorHitCollection* sectorcoll1 = tpchitcoll1->sector(s);
    const StTpcSectorHitCollection* sectorcoll2 = tpchitcoll2->sector(s);
    if (! sectorcoll1 || ! sectorcoll2) continue;
    for (Int_t r = 0; r < 72; r++) {
      const StTpcPadrowHitCollection* padrowcoll1 = sectorcoll1->padrow(r);
      const StTpcPadrowHitCollection* padrowcoll2 = sectorcoll2->padrow(r);
      if (! padrowcoll1 || ! padrowcoll2) continue;
      const StSPtrVecTpcHit& hits1 = padrowcoll1->hits();
      const StSPtrVecTpcHit& hits2 = padrowcoll2->hits();
      UInt_t N1  = hits1.size();
      UInt_t N2  = hits2.size();
      for (UInt_t it1 = 0; it1 < N1; it1++) {
	const StTpcHit *hit1 = hits1[it1];
	if (! hit1) continue;
	//	if (hit1->flag()) continue;
	for (UInt_t it2 = 0; it2 < N2; it2++) {
	  const StTpcHit *hit2 = hits2[it2];
	  if (! hit2) continue;
	  //	  if (hit2->flag()) continue;
	  assert(hit1->sector() == hit2->sector() &&
		 hit1->padrow() == hit2->padrow());
	  Double_t R = 
	    (hit1->pad()        - hit2->pad()       )*(hit1->pad()        - hit2->pad()      ) +
	    (hit1->timeBucket() - hit2->timeBucket())*(hit1->timeBucket() - hit2->timeBucket());
	  if (R < 4) {
	    if (! Hit1ToHit2[hit1] && ! Hit2ToHit1[hit2]) {
	      Hit1ToHit2[hit1] = hit2;
	      Hit2ToHit1[hit2] = hit1;
	    } else {
	      const StTpcHit *hit1o = Hit2ToHit1[hit2];
	      const StTpcHit *hit2o = Hit1ToHit2[hit1];
	      if (hit1o) {
		Double_t Ro = 
		  (hit1o->pad()        - hit2->pad()       )*(hit1o->pad()        - hit2->pad()      ) +
		  (hit1o->timeBucket() - hit2->timeBucket())*(hit1o->timeBucket() - hit2->timeBucket());
		if (R < Ro) Hit2ToHit1[hit2] = hit1;
	      } else { 
		Hit2ToHit1[hit2] = hit1;
	      }
	      if (hit2o) {
		Double_t Ro = 
		  (hit1->pad()        - hit2o->pad()       )*(hit1->pad()        - hit2o->pad()      ) +
		  (hit1->timeBucket() - hit2o->timeBucket())*(hit1->timeBucket() - hit2o->timeBucket());
		if (R < Ro) Hit1ToHit2[hit1] = hit2;
	      } else {
		Hit1ToHit2[hit1] = hit2;
	      }
	    }
	  }
	} // hit2 loop
      } // hit1 loop
#ifdef __HIT_MATCH__
      for (UInt_t it1 = 0; it1 < N1; it1++) {
	const StTpcHit *hit1 = hits1[it1];
	if (! hit1) continue;
	//	if (hit1->flag()) continue;
	const StTpcHit *hit2 = Hit1ToHit2[hit1];
	FillMatch(hit1, hit2);
      }
      for (UInt_t it2 = 0; it2 < N2; it2++) {
	const StTpcHit *hit2 = hits2[it2];
	if (! hit2) continue;
	//	if (hit2->flag()) continue;
	const StTpcHit *hit1 = Hit2ToHit1[hit2];
	if (hit1) continue;
	FillMatch(hit1, hit2);
      }
#endif /* __HIT_MATCH__ */
    } // row loop
  } // sector loop
}
//________________________________________________________________________________
void    StTbyTMaker::buildTrack2TrackMap(const StSPtrVecTrackNode &trackNodes1, 
					      const StSPtrVecTrackNode &trackNodes2, 
					      Hit2HitMap &Hit1ToHit2, 
					      Hit2TrackMap &hitTrackMap2,
					      Track2TrackMap &Track1ToTrack2) {
  for (size_t iTrk=0; iTrk<trackNodes1.size();++iTrk) {
    StGlobalTrack *track1 = (StGlobalTrack *) trackNodes1[iTrk]->track(global);
    if (! GoodTrack(track1)) continue;
    if (Debug()>2) LOG_INFO << "Processing Track " << iTrk << endm;
    StGlobalTrack *track2 = 0;
    if (Debug() > 2 && Track1ToTrack2.count(track1) > 0 ) { 
      LOG_INFO << "Matching track count before " << Track1ToTrack2.count(track1) << endm;
    }
    vector<StTrackPing> candidates;
    StPtrVecHit trkhits1 = track1->detectorInfo()->hits(kTpcId);
    for (StPtrVecHitIterator hIterTrk = trkhits1.begin(); hIterTrk != trkhits1.end(); ++hIterTrk) {
      // get the hit from the track
      const StTpcHit* hit1 = static_cast<const StTpcHit*>(*hIterTrk);
      const StTpcHit* hit2 = Hit1ToHit2[hit1];
      if (! hit2) continue;
      // use the map to find its track!
      auto ret = hitTrackMap2.equal_range(hit2);
      for (auto it = ret.first; it !=ret.second; ++it) {
	track2 = it->second;
	if (!track2) continue;
	Bool_t found = kFALSE;
	for (auto it = candidates.begin(); it != candidates.end(); ++it) {
	  if (track2 == it->mTrack) {
	    found = kTRUE;
	    it->mNPings++;
	    break;
	  }
	}
	if (! found) candidates.push_back(StTrackPing(track2,1));
      } // track2 loop
      if (candidates.size() > 1) {
	sort(candidates.begin(), candidates.end(), compStTrackPing);
      }
    } // track1 hits
    Int_t noMatched = 0;
    for (auto it = candidates.begin(); it != candidates.end(); ++it) {
      if (Debug()>2) {
	LOG_INFO << "Matching track " << it->mTrack << " has " << it->mNPings << ", index " << it-candidates.begin() << endm;
      }
      // Association is considered as good if no. of matched (good) point > 5 and no. matched point > 0.5 * Min(no. of fit points on track1 or track2)
      StGlobalTrack* track2 = (StGlobalTrack*) it->mTrack;
      if (! track2) continue;
      Bool_t Matched = GoodMatch(track1, track2, it->mNPings);
      if (Matched) {
	noMatched++;
	Track1ToTrack2.insert( TrackTrackPair (track1, track2));
      }
    }
    if (! noMatched) {
      Track1ToTrack2.insert( TrackTrackPair (track1, 0));
    } // candidates
    if (Debug() > 2 && Track1ToTrack2.count(track1) > 1) {
      LOG_INFO << "Matching tracks = " << noMatched << "\tcount after " << Track1ToTrack2.count(track1) << endm;
      static Int_t breakI = 0;
      breakI++;
  }
  }//track1 loop
}
//________________________________________________________________________________
void StTbyTMaker::checkConsistency(Track2TrackMap &Track1ToTrack2, Track2TrackMap &Track2ToTrack1) {
  // Check consistency of the maps Track1 <=> Track2 
  for (auto it = Track1ToTrack2.begin(), end =  Track1ToTrack2.end(); it != end; it = Track1ToTrack2.upper_bound(it->first)) {
    StGlobalTrack *track1 = it->first;
    if (! track1) continue;
    auto ret = Track1ToTrack2.equal_range(track1);
    Int_t nt2 = Track1ToTrack2.count(track1);
    for (auto it2 = ret.first; it2 != ret.second; ++it2) {
      StGlobalTrack *track2 = it2->second;
      if (! track2 ) continue;
      Int_t nt1 = Track2ToTrack1.count(track2);
      if (nt1 == 1 && nt2 == 1) {
	auto ret2 = Track2ToTrack1.equal_range(track2);
	for (Track2TrackIter it3 = ret2.first; it3 !=ret2.second; ++it3) {
	  StGlobalTrack* track = it3->second;
	  if (track1 != track) {
	    LOG_INFO  << "Inconsistent tracks nt1 =" << nt1 << "\tnt2 = " <<nt2 << endm;
	    PrPP(checkConsistency,*track1);
	    PrPP(checkConsistency,*track2);
	    if (! track) {
	      if (Debug()%10 > 2) {LOG_INFO << "track is missing" << endm;}
	    } else {
	      PrPP(checkConsistency,*track);
	    }
	    StGlobalTrack *tracks[3] = {track1, track2, track};
	    for (Int_t i = 0; i < 3; i++) {
	      StGlobalTrack *t = tracks[i];
	      if (! t) continue;
	      if (Debug()%10 > 2) {LOG_INFO << Form("Track %i\t",i+1); PrPP(checkConsistency,*t);}
	      StTrackDetectorInfo *det = t->detectorInfo();
	      StPtrVecHit hits = det->hits(kTpcId);
	      UInt_t Nhits = hits.size(); 
	      if (Debug()%10 > 2) {LOG_INFO << Form("Track %i\t",i+1); PrPP(checkConsistency,Nhits);}
	      for (UInt_t j = 0; j < Nhits; j++) {
		StTpcHit *tpcHit = static_cast<StTpcHit *> (hits[j]);
		if (! tpcHit) continue;
		PrPP("hit", *tpcHit);
	      }
	    }
	    static Int_t iBreak = 0;
	    iBreak++;
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
HitParameters StTbyTMaker::HitParametersFill(const StTpcHit *tpcHit) {
  HitParameters P;
  if (! tpcHit) return P;
  const StThreeVectorF& xyz = tpcHit->position();
  P.sector = tpcHit->sector();
  P.row    = tpcHit->padrow();
  static StTpcCoordinateTransform tran;
  StGlobalCoordinate glob(xyz);
  //		    StTpcLocalCoordinate lTpc;
  StTpcLocalSectorCoordinate lTpc;
  tran(glob,lTpc,P.sector,P.row);
  P.x = xyz.x();
  P.y = xyz.y();
  P.z = xyz.z();
  P.xL = lTpc.position().x();
  P.yL = lTpc.position().y();
  P.zL = lTpc.position().z();
  P.q = 1.e6*tpcHit->charge();
  P.adc = tpcHit->adc();
  P.pad = tpcHit->pad();
  P.timebucket = tpcHit->timeBucket();
  P.IdTruth =  tpcHit->idTruth();
  P.npads   =  tpcHit->padsInHit();
  P.ntbks   =  tpcHit->maxTmbk() - tpcHit->minTmbk() + 1;
  P.trigId  = 0;
  P.us      = tpcHit->usedInFit();
  P.fl      = tpcHit->flag();
  if (Debug()) {
    cout << "P\t"; P.Print();
    if (P.sector <= 0 || P.sector > 24) {
      static Int_t ibreak = 0;
      ibreak++;
    }
  }
  return P;
}
//________________________________________________________________________________
TrackParameters StTbyTMaker::TrackParametersFill(const StGlobalTrack *gTrack) {
  TrackParameters par;
  if (! gTrack) return par;
  par.set();
  if (! gTrack) return par;
  par.Id = gTrack->key();
  StPrimaryTrack* pTrack = (StPrimaryTrack*) gTrack->node()->track(primary);
  par.MatchStatus = gTrack->flagExtension() + 1;
  ((StGlobalTrack *)gTrack)->setFlagExtension(par.MatchStatus);
  const StTrackDetectorInfo *det = gTrack->detectorInfo();
  if (det) {
    par.FirstPointX = det->firstPoint().x();
    par.FirstPointY = det->firstPoint().y();
    par.FirstPointZ = det->firstPoint().z();
    par.LastPointX = det->lastPoint().x();
    par.LastPointY = det->lastPoint().y();
    par.LastPointZ = det->lastPoint().z();
    par.hitMap  
      =    det->numberOfReferencedPoints(kPxlId) + 
      10  *det->numberOfReferencedPoints(kIstId) +
      100 *det->numberOfReferencedPoints(kSsdId);
  }
  const StDedxPidTraits* mTraits = 0;
  const StSPtrVecTrackPidTraits& vec = gTrack->pidTraits();
  for (unsigned int i=0; i<vec.size(); i++) {
    const StDedxPidTraits *p = dynamic_cast<const StDedxPidTraits*>(vec[i]);
    if (p && p->detector() == kTpcId && p->method() == kTruncatedMeanId) mTraits = p;
  }
  if (mTraits) par.Dedx = mTraits->mean();
  const StDcaGeometry *dca = gTrack->dcaGeometry();
  StThreeVectorF mom;
  if (dca) mom = dca->momentum();
  else      mom = gTrack->geometry()->momentum();
  par.PtGl = mom.perp(); 
  par.EtaGl = mom.pseudoRapidity();
  par.PhiGl = mom.phi();
  par.PGl = mom.mag();
  par.FitPtsGl = gTrack->fitTraits().numberOfFitPoints(kTpcId);
  par.Charge = gTrack->geometry()->charge();
  par.Chi2Gl0 = gTrack->fitTraits().chi2(0);
  par.Chi2Gl1 = gTrack->fitTraits().chi2(1);
  if (pTrack) {
    const StThreeVectorF& pmom = pTrack->geometry()->momentum();
    par.PtPr  = pmom.perp(); 
    par.EtaPr = pmom.pseudoRapidity();
    par.PhiPr = pmom.phi();
    par.PPr   = pmom.mag();
    par.FitPtsPr = gTrack->fitTraits().numberOfFitPoints(kTpcId);
    par.Prim    += 1;
    par.Chi2Pr0 = pTrack->fitTraits().chi2(0);
    par.Chi2Pr1 = pTrack->fitTraits().chi2(1);
    StPrimaryTrack *prim = (StPrimaryTrack *) pTrack;
    const StVertex *vertex = prim->vertex();
    if (vertex) {
      par.PrimX = vertex->position().x();
      par.PrimY = vertex->position().y();
      par.PrimZ = vertex->position().z();
    }
  }
  return par;
}
									     
