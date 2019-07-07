//
// $Id: StTrackMateMaker.cxx,v 1.4 2013/01/16 21:56:45 fisyak Exp $
//
#include <iostream>
#include <algorithm>
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
#include "StDcaGeometry.h"
#include "StEventUtilities/StuRefMult.hh"
#include "StDedxPidTraits.h"
#include "StMessMgr.h"
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

static const char rcsid[] = "$Id: StTrackMateMaker.cxx,v 1.4 2013/01/16 21:56:45 fisyak Exp $";
ClassImp(StTrackMateMaker);
// tpt => old
// sti => new
  
static  TString names("oldPtGl/F:newPtGl/F:oldEtaGl/F:newEtaGl/F:oldPhiGl/F:newPhiGl/F:oldPGl/F:newPGl/F:oldFitPtsGl/F:"
		      "newFitPtsGl/F:oldPtPr/F:newPtPr/F:oldEtaPr/F:newEtaPr/F:oldPhiPr/F:newPhiPr/F:oldPPr/F:newPPr/F:"
		      "oldFitPtsPr/F:newFitPtsPr/F:oldDedx/F:newDedx/F:oldCharge/F:newCharge/F:maxPing/F:Prim/F:"
		      "oldChi2Gl0/F:newChi2Gl0/F:oldChi2Gl1/F:newChi2Gl1/F:oldChi2Pr0/F:newChi2Pr0/F:oldChi2Pr1/F:"
		      "newChi2Pr1/F:firstHitsDist/F:lastHitsDist/F:"
		      "oldPrimX/F:oldPrimY/F:oldPrimZ/F:newPrimX/F:newPrimY/F:newPrimZ/F"
		      ":newSecF/F:oldSecF/F:newSecL/F:oldSecL/F:newHitMap/I:oldHitMap/I");
struct mc_data_array {
  Char_t begin;
  Float_t oldPtGl;
  Float_t newPtGl;
  Float_t oldEtaGl;
  Float_t newEtaGl;
  Float_t oldPhiGl;
  Float_t newPhiGl;
  Float_t oldPGl;
  Float_t newPGl;
  Float_t oldFitPtsGl;
  Float_t newFitPtsGl;
  Float_t oldPtPr;
  Float_t newPtPr;
  Float_t oldEtaPr;
  Float_t newEtaPr;
  Float_t oldPhiPr;
  Float_t newPhiPr;
  Float_t oldPPr;
  Float_t newPPr;
  Float_t oldFitPtsPr;
  Float_t newFitPtsPr;
  Float_t oldDedx;
  Float_t newDedx;
  Float_t oldCharge;
  Float_t newCharge;
  Float_t maxPing;
  Float_t Prim;
  Float_t oldChi2Gl0;
  Float_t newChi2Gl0;
  Float_t oldChi2Gl1;
  Float_t newChi2Gl1;
  Float_t oldChi2Pr0;
  Float_t newChi2Pr0;
  Float_t oldChi2Pr1;
  Float_t newChi2Pr1;
  Float_t firstHitsDist;
  Float_t lastHitsDist;
  Float_t oldPrimX;
  Float_t oldPrimY;
  Float_t oldPrimZ;
  Float_t newPrimX;
  Float_t newPrimY;
  Float_t newPrimZ;
  Float_t newSecF;  // first and last hit sector
  Float_t oldSecF;
  Float_t newSecL;
  Float_t oldSecL;
  Int_t newHitMap;
  Int_t oldHitMap;
  Char_t end;
public:
  void set() {memset(&begin, 0, &end-&begin);}
};
static mc_data_array myData;    
//________________________________________________________________________________
Int_t StTrackMateMaker::Init() {
  TFile *f = GetTFile();
  if (f) {
    f->cd();
  }
  LOG_INFO << "StTrackMateMaker::Init() - creating histogram" << endm;
  TString evNames = "refMult/F";
  trackTree = new TTree("trackMateComp","trackMateComp");
  trackBr = trackTree->Branch("data_array",&myData.oldPtGl,names.Data());
  //  eventBr = trackTree->Branch("ev_array",evOutput,evNames.Data());
  LOG_INFO << "StTrackMateMaker::Init() - successful" << endm;
  
  return StMaker::Init();
}
//________________________________________________________________________________
void StTrackMateMaker::Clear(const char* c)
{
  return StMaker::Clear(c);
}
//________________________________________________________________________________
Bool_t StTrackMateMaker::GoodTrack(StTrack* trk) {
  if (! trk || trk->flag()<=0 || trk->topologyMap().trackFtpc()) return kFALSE;
  if (! trk->detectorInfo())  return kFALSE;
  if ( trk->fitTraits().numberOfFitPoints(kTpcId) < 10) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t StTrackMateMaker::GoodMatch(StTrack* trk1, StTrack* trk2, UInt_t NPings) {
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
  LOG_INFO << "Size of track containers" << endm;
  const StSPtrVecTrackNode& trackNodes1 = rEvent1->trackNodes();
  LOG_INFO << "Event1: Track Nodes " << trackNodes1.size() << endm;
  
  const StSPtrVecTrackNode& trackNodes2 = rEvent2->trackNodes();
  LOG_INFO << "Event2: Track Nodes " << trackNodes2.size() << endm;
  if (! trackNodes1.size() || ! trackNodes2.size()) return kStWarn;
  //eventwise info
  //  evOutput[0] = uncorrectedNumberOfPrimaries(*rEvent2);
  //eventBr->Fill();
  LOG_INFO << "Tpc Hits" << endm;
  // Note:
  // For StTpcHits: sector = [1-24], padrow = [1-72]
  // For StTpcHitCollections: sector [0-23], padrow = [0,71]
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
  
  map<const StTpcHit*,StGlobalTrack*> hitTrackMap1;
  size_t failedTries1 = buildRecHitTrackMap(trackNodes1,hitTrackMap1);
  LOG_INFO << "Hits1 used by more than 1 track: " << failedTries1 << endm;
  map<const StTpcHit*,StGlobalTrack*> hitTrackMap2;
  size_t failedTries2 = buildRecHitTrackMap(trackNodes2,hitTrackMap2);
  LOG_INFO << "Hits2 used by more than 1 track: " << failedTries2 << endm;
  map<const StTpcHit*,const StTpcHit*> Hit2ToHit1;
  map<const StTpcHit*,const StTpcHit*> Hit1ToHit2;
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
  map<StGlobalTrack*,StGlobalTrack*> Track1ToTrack2;
  map<StGlobalTrack*,StGlobalTrack*> Track2ToTrack1;
  buildTrack2TrackMap(trackNodes1, trackNodes2, Hit1ToHit2, hitTrackMap2, Track1ToTrack2);
  buildTrack2TrackMap(trackNodes2, trackNodes1, Hit2ToHit1, hitTrackMap1, Track2ToTrack1);
  checkConsistency(Track1ToTrack2,Track2ToTrack1);
  checkConsistency(Track2ToTrack1,Track1ToTrack2);
  // at this point, we have all possible track candidates
  // based on the tpc hits of trk1.
  // find candidate with most common tpc hits,
  // could rewrite using
  // max_element...
  for (auto x1 : Track1ToTrack2) {
    StGlobalTrack *trk1 = x1.first;
    if (! trk1 ) continue;
    StPrimaryTrack* ptrk1 = (StPrimaryTrack*) trk1->node()->track(primary);
    StGlobalTrack *trk2 = x1.second;
    if (! trk2 ) {
      Fill(trk1,ptrk1,0,0,-1);
    } else {
      //      assert(trk1 == Track2ToTrack1[trk2]);
      StPrimaryTrack* ptrk2 = (StPrimaryTrack*) trk2->node()->track(primary);
      Fill(trk1,ptrk1,trk2,ptrk2,1);
    }
  }
  for (auto x2 : Track2ToTrack1) {
    StGlobalTrack *trk2 = x2.first;
    if (! trk2 ) continue;
    StPrimaryTrack* ptrk2 = (StPrimaryTrack*) trk2->node()->track(primary);
    StGlobalTrack *trk1 = x2.second;
    if (trk1) continue;
    Fill(0,0,trk2,ptrk2,-2);
  }
  return kStOK;
}
//________________________________________________________________________________
void StTrackMateMaker::Fill(StGlobalTrack* trk1, StPrimaryTrack* ptrk1,StGlobalTrack* trk2, StPrimaryTrack* ptrk2,Int_t maxPing) {
  myData.set();
  myData.firstHitsDist = myData.lastHitsDist = -999.;
  if (trk1) {
    StDcaGeometry *dca1 = trk1->dcaGeometry();
    StThreeVectorF mom1;
    if (dca1) mom1 = dca1->momentum();
    else      mom1 = trk1->geometry()->momentum();
    myData.oldPtGl = mom1.perp(); 
    myData.oldEtaGl = mom1.pseudoRapidity();
    myData.oldPhiGl = mom1.phi();
    myData.oldPGl = mom1.mag();
    myData.oldFitPtsGl = trk1->fitTraits().numberOfFitPoints(kTpcId);
    myData.oldDedx = getTpcDedx(trk1);
    myData.oldCharge = trk1->geometry()->charge();
    myData.oldChi2Gl0 = trk1->fitTraits().chi2(0);
    myData.oldChi2Gl1 = trk1->fitTraits().chi2(1);
    myData.maxPing = 0;
    if (ptrk1) {
      const StThreeVectorF& pmom1 = ptrk1->geometry()->momentum();
      myData.oldPtPr  = pmom1.perp(); 
      myData.oldEtaPr = pmom1.pseudoRapidity();
      myData.oldPhiPr = pmom1.phi();
      myData.oldPPr   = pmom1.mag();
      myData.oldFitPtsPr = trk1->fitTraits().numberOfFitPoints(kTpcId);
      myData.Prim    += 1;
      myData.oldChi2Pr0 = ptrk1->fitTraits().chi2(0);
      myData.oldChi2Pr1 = ptrk1->fitTraits().chi2(1);
      StPrimaryTrack *prim = (StPrimaryTrack *) ptrk1;
      const StVertex *vertex = prim->vertex();
      if (vertex) {
	myData.oldPrimX = vertex->position().x();
	myData.oldPrimY = vertex->position().y();
	myData.oldPrimZ = vertex->position().z();
      }
    }
  }
  if (trk2) {
    StDcaGeometry *dca2 = trk2->dcaGeometry();
    StThreeVectorF mom2;
    if (dca2) mom2 = dca2->momentum();
    else      mom2 = trk2->geometry()->momentum();
    myData.newPtGl  = mom2.perp();
    myData.newEtaGl = mom2.pseudoRapidity();
    myData.newPhiGl = mom2.phi();
    myData.newPGl   = mom2.mag();
    myData.newFitPtsGl = trk2->fitTraits().numberOfFitPoints(kTpcId);
    myData.newDedx = getTpcDedx(trk2);
    myData.newCharge = trk2->geometry()->charge();
    myData.newChi2Gl0 = trk2->fitTraits().chi2(0);
    myData.newChi2Gl1 = trk2->fitTraits().chi2(1);
    myData.maxPing = 0;
    if (ptrk2) {
      const StThreeVectorF& pmom2 = ptrk2->geometry()->momentum();
      myData.newPtPr  = pmom2.perp();
      myData.newEtaPr = pmom2.pseudoRapidity();
      myData.newPhiPr = pmom2.phi();
      myData.newPPr   = pmom2.mag();
      myData.newFitPtsPr = trk2->fitTraits().numberOfFitPoints(kTpcId);
      myData.Prim    += 10;
      myData.newChi2Pr0 = ptrk2->fitTraits().chi2(0);
      myData.newChi2Pr1 = ptrk2->fitTraits().chi2(1);
      StPrimaryTrack *prim = (StPrimaryTrack *) ptrk2;
      const StVertex *vertex = prim->vertex();
      if (vertex) {
	myData.newPrimX = vertex->position().x();
	myData.newPrimY = vertex->position().y();
	myData.newPrimZ = vertex->position().z();
      }
    }
  }
  myData.maxPing = maxPing;
  StTrackDetectorInfo *det1 = 0;
  StTrackDetectorInfo *det2 = 0;
  if (trk1)  det1 = trk1->detectorInfo();
  if (trk2)  det2 = trk2->detectorInfo();
  if (det1 && det2) {
    StThreeVectorF difFirst = det1->firstPoint() - det2->firstPoint();
    myData.firstHitsDist = difFirst.mag();
    StThreeVectorF difLast = det1->lastPoint() - det2->lastPoint();
    myData.lastHitsDist = difLast.mag();
  }
  if (det1) myData.oldHitMap  
    =    det1->numberOfReferencedPoints(kPxlId) + 
    10  *det1->numberOfReferencedPoints(kIstId) +
    100 *det1->numberOfReferencedPoints(kSsdId);
  if (det2) myData.newHitMap  
    =    det2->numberOfReferencedPoints(kPxlId) + 
    10  *det2->numberOfReferencedPoints(kIstId) +
    100 *det2->numberOfReferencedPoints(kSsdId); 
  trackTree->Fill();
}
//________________________________________________________________________________
size_t StTrackMateMaker::buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,map<const StTpcHit*,StGlobalTrack*>& htMap){
  size_t failedInserts = 0;
  for (size_t it = 0; it<nodes.size(); ++it) {
    StGlobalTrack* track = (StGlobalTrack*)nodes[it]->track(global);
    if (! GoodTrack(track)) continue;
    StPtrVecHit hits = track->detectorInfo()->hits(kTpcId);
    for (StPtrVecHitIterator hIterTrk = hits.begin(); hIterTrk != hits.end(); ++hIterTrk) {
      StHit* h = *hIterTrk;
      if (! h) continue;
      StTpcHit* hit = (StTpcHit*) h;
      StGlobalTrack* trackO = htMap[hit];
      if (trackO && track != trackO) {
	LOG_INFO << "Double track associated with hit \n" << *hit
		 << "\nOld Track : " << *trackO << " and \nNew Track : " << *track << endm;
	if (track->fitTraits().numberOfFitPoints() > trackO->fitTraits().numberOfFitPoints()) htMap[hit] = track;
	failedInserts++;
      } else {
	htMap[hit] = track;
      }
//       pair<map<const StTpcHit*,StGlobalTrack*>::iterator,bool> insRes = htMap.insert(map<const StTpcHit*,StGlobalTrack*>::value_type(hit,track));
//       if (insRes.second==false) ++failedInserts;	    
    }//hits in track loop
  }// track loop
  return failedInserts;
}
//________________________________________________________________________________
void StTrackMateMaker::buildHit2HitMaps(const StTpcHitCollection *tpchitcoll1, const StTpcHitCollection *tpchitcoll2,
		       map<const StTpcHit*,const StTpcHit*> &Hit1ToHit2,map<const StTpcHit*,const StTpcHit*> &Hit2ToHit1) {
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
	if (hit1->flag()) continue;
	for (UInt_t it2 = 0; it2 < N2; it2++) {
	  const StTpcHit *hit2 = hits2[it2];
	  if (! hit2) continue;
	  if (hit2->flag()) continue;
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
	}
      }
    }
  }
}
//________________________________________________________________________________
void    StTrackMateMaker::buildTrack2TrackMap(const StSPtrVecTrackNode &trackNodes1, 
					      const StSPtrVecTrackNode &trackNodes2, 
					      map<const StTpcHit*,const StTpcHit*> &Hit1ToHit2, 
					      map<const StTpcHit*,StGlobalTrack*> &hitTrackMap2,
					      map<StGlobalTrack*,StGlobalTrack*> &Track1ToTrack2) {
  StTrackPing initTrackPing;
  initTrackPing.mTrack=0;
  initTrackPing.mNPings=0;
  for (size_t iTrk=0; iTrk<trackNodes1.size();++iTrk) {
    StGlobalTrack *trk1 = (StGlobalTrack *) trackNodes1[iTrk]->track(global);
    if (! GoodTrack(trk1)) continue;
    if (Debug()>2) LOG_INFO << "Processing Track " << iTrk << endm;
    vector<StTrackPing> candidates(20,initTrackPing); //make sure it's filled with null pointers and zeros
    size_t nCandidates = 0;
    StPtrVecHit trkhits1 = trk1->detectorInfo()->hits(kTpcId);
    
    for (StPtrVecHitIterator hIterTrk = trkhits1.begin(); hIterTrk != trkhits1.end(); ++hIterTrk) {
      // get the hit from the track
      const StTpcHit* hit1 = static_cast<const StTpcHit*>(*hIterTrk);
      const StTpcHit* hit2 = Hit1ToHit2[hit1];
      if (! hit2) continue;
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
	for (UInt_t iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
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
    vector<StTrackPing>::iterator max = max_element(candidates.begin(),candidates.end(),compStTrackPing);
    UInt_t NPings = (*max).mNPings;
    if (Debug()>2) {
      LOG_INFO << "Matching track " << (*max).mTrack << " has " << NPings << ", index " << max-candidates.begin() << endm;
    }
    // Association is considered as good if no. of matched (good) point > 5 and no. of bad points for each track less than 5
    StGlobalTrack* trk2 = (StGlobalTrack*) (*max).mTrack;
    Bool_t Matched = GoodMatch(trk1, trk2, NPings);
    if (Matched) Track1ToTrack2[trk1] = trk2;
    else         Track1ToTrack2[trk1] =    0;
  }//track loop
 }
//________________________________________________________________________________
Float_t StTrackMateMaker::getTpcDedx(StTrack* trk) {
  const StSPtrVecTrackPidTraits& vec = trk->pidTraits();
  const StDedxPidTraits* mTraits = 0;
  for (unsigned int i=0; i<vec.size(); i++) {
    const StDedxPidTraits *p = dynamic_cast<const StDedxPidTraits*>(vec[i]);
    if (p && p->detector() == kTpcId && p->method() == kTruncatedMeanId) mTraits = p;
  }
  if (!mTraits) return 0;    // no info available
  return mTraits->mean();
}
//________________________________________________________________________________
void StTrackMateMaker::checkConsistency(map<StGlobalTrack*,StGlobalTrack*> &Track1ToTrack2, map<StGlobalTrack*,StGlobalTrack*> &Track2ToTrack1) {
  // Check consistency of the maps Track1 => Track2 
  for (auto x : Track1ToTrack2) {
    StGlobalTrack *track1 = x.first;
    if (! track1) continue;
    StGlobalTrack *track2 = x.second;
    if (! track2 ) continue;
    StGlobalTrack *track = Track2ToTrack1[track2];
    if (track1 != track) {
      cout << "Inconsistent tracks" << endl;
      PrPP(Make,*track1);
      PrPP(Make,*track2);
      if (! track) {
	cout << "track2 => track1 is missing" << endl;
      } else {
	PrPP(Make,*track);
      }
      StGlobalTrack *tracks[3] = {track1, track2, track};
      for (Int_t i = 0; i < 3; i++) {
	StGlobalTrack *t = tracks[i];
	if (! t) continue;
	PrPP(Form("Track %i",i+1),*t);
	StTrackDetectorInfo *det = t->detectorInfo();
	StPtrVecHit hits = det->hits(kTpcId);
	UInt_t Nhits = hits.size(); 
	PrPP(Form("Track %i",i+1),Nhits);
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
