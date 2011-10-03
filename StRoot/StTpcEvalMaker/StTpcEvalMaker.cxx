//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
// additions: manuel cbs
//-------------------------------------------------
// class definition of StTpcEvalMaker
//-------------------------------------------------

//-----------------------------------------------------------------------
// loosely based on
// * $Id: StTpcEvalMaker.cxx,v 1.1.1.1 2000/05/23 00:25:03 snelling Exp $
// * $Log: StTpcEvalMaker.cxx,v $
// * Revision 1.1.1.1  2000/05/23 00:25:03  snelling
// * Milton's and Manuel's version
// *
//-----------------------------------------------------------------------

#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <algorithm>
#include <math.h>

#include "StGlobals.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StPhysicalHelixD.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "TFile.h"
#include "StTpcDb/StTpcDbMaker.h"
#include "StMessMgr.h"

#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StMcParameterDB.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

#include "StEventTypes.h"

#include "StMcContainers.hh"
#include "StMcEvent.hh"
#include "StMcTpcHit.hh"
#include "StMcTpcHitCollection.hh"
#include "StMcTrack.hh"
#include "StMcVertex.hh"

#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"

#include "StTpcEvalMaker.h"
#include "StTpcEvalEvent.h"
#include "StTpcEvalHistograms.h"

static const char rcsid[] = "$Id: StTpcEvalMaker.cxx,v 1.1.1.1 2000/05/23 00:25:03 snelling Exp $";
ClassImp(StTpcEvalMaker)

//-------------------------------------------------
  // TO DO:
  // database interface
  // inclusion of vertex information
  // id number for StGlobalTrack (may not be necessary)
  // and....
//-------------------------------------------------

//-------------------------------------------------

StTpcEvalMaker::StTpcEvalMaker(const char *name, const char *title):StMaker(name,title)
{
  //
}
//-------------------------------------------------
StTpcEvalMaker::~StTpcEvalMaker()
{
  //
}
//-------------------------------------------------
void StTpcEvalMaker::Clear(const char*)
{
  StMaker::Clear();
}
//-------------------------------------------------
Int_t StTpcEvalMaker::Finish()
{
    cout << " StTpcEvalMaker:  Writing out histograms..." <<endl;
    mOutputFile->Write();

    cout << " StTpcEvalMaker:  Closing output file..." <<endl;
    mOutputFile->Close();

    return StMaker::Finish();
}
//-------------------------------------------------
Int_t StTpcEvalMaker::Init()
{
    // Create the event structure.
    mTpcEvalEvent = new StTpcEvalEvent();
    
    cout << " StTpcEvalMaker:  Opening output file..." << endl;
    
    // Create the ROOT file.
    // to do: pass on string to replace StTpcEval.hist.root
    mOutputFile = new TFile("StTpcEval.hist.root","RECREATE","StTpcEvalMaker Results");
    mOutputFile->SetCompressionLevel(1);

    // Create the ROOT Tree
    mTrackPairTree = new TTree("trackTree", "Track Pair Tree");
    mTrackPairTree->SetAutoSave(100000000); // autosave every 100 Mbytes

    // Split event into sub-branches
    Int_t splitlevel = 1;
    Int_t bufsize = 64000;
    mTrackPairTree->Branch("StTpcEvalBranch","StTpcEvalEvent",&mTpcEvalEvent, bufsize, splitlevel);

    // Book histograms
    histograms.Book();
    return StMaker::Init();
}
//-------------------------------------------------

Int_t StTpcEvalMaker::Make()
{
    
    // get pointers to global variables defined in the analysis
    // chain macro...
    
    // TPC database
    mStTpcDb = ((StTpcDbMaker*) GetMaker("tpcDb"))->tpcDbInterface();
    if (!mStTpcDb)  {
	cout<<" StTpcDb NOT FOUND!!!"<<endl;
	return kStErr;
    }
    
    // event data -- why are the methods to get StEvent and StMcEvent
    // so different? Because StEvent is persistent, and StMcEvent is not
    // One inherits from St_DataSet, the other one does not
    
    
    mStEvent = (StEvent*) GetInputDS("StEvent");
    mStMcEvent =  ((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
    if (!(mStEvent && mStMcEvent)) {
	cout<<" NO EVENT DATA FOUND!!!"<<endl;
	return kStWarn;
    }
    
    // StAssociationMaker multimaps
    // ditto the complaints given above...
    // mcbs: modified to no longer use a global pointer, and gave more consistent
    // titles
    StAssociationMaker* assoc = 
	(StAssociationMaker*) GetMaker("StAssociationMaker");
    if (!assoc) {
	cout<<" NO StAssocationMaker DATA!!!"<<endl;
	return kStWarn;
    }
    mmcTpcHitMap = assoc->mcTpcHitMap();
    mmcTrackMap  = assoc->mcTrackMap();
    mrcTpcHitMap = assoc->rcTpcHitMap();
    mrcTrackMap  = assoc->rcTrackMap();
    if (!mrcTpcHitMap || !mrcTrackMap ||
	!mmcTpcHitMap || !mmcTrackMap) {
	gMessMgr->Warning() << "Missing multimaps!!! " << endm;
	return kStWarn;
    }
    //
    // analyze data using StTpcEvalMaker methods
    //

    fillHeader();
    mcHitIteration();
    rcHitIteration();
    mcTrackIteration();
    rcTrackIteration();
    
    mTrackPairTree->Fill();
    mTpcEvalEvent->Clear();
    
    return kStOK;
}

//-------------------------------------------------

void StTpcEvalMaker::fillHeader() {

    UInt_t geantTrks, globTrks, geantPrimaries, recoPrimaries, geantTpcHits, recoTpcHits;
    geantTrks = globTrks = geantPrimaries = recoPrimaries = geantTpcHits = recoTpcHits = 0;

    // Only count g2t tracks.
    StMcTrackIterator mcTrkIter2 = mStMcEvent->tracks().begin();
    while (mcTrkIter2 != mStMcEvent->tracks().end() && !((*mcTrkIter2)->key())) ++mcTrkIter2;
    geantTrks = distance (mcTrkIter2, mStMcEvent->tracks().end());
					 
    globTrks = mStEvent->trackNodes().size();
    geantPrimaries = mStMcEvent->primaryVertex()->daughters().size();
    recoPrimaries  =   mStEvent->primaryVertex()->daughters().size();
    geantTpcHits   = mStMcEvent->tpcHitCollection()->numberOfHits();
    recoTpcHits    =   mStEvent->tpcHitCollection()->numberOfHits();
    
    mTpcEvalEvent->SetHeader(geantTrks, globTrks, geantPrimaries, recoPrimaries, geantTpcHits, recoTpcHits);
}

//-------------------------------------------------

void StTpcEvalMaker::mcHitIteration() {
    
    cout << "_ StMcTpcHit iteration __________________" << endl;
    
    StMcTpcHitCollection* mcTpcHitCollection =
	mStMcEvent->tpcHitCollection();
    if (!mcTpcHitCollection) {
	cout << "--> StTpcEvalMaker warning: no StMcTpcHits found!" << endl;
	return;
    }
    MatchedHitPair hitPair(mStTpcDb);
    
    for (unsigned int isec=0;
	 isec<mcTpcHitCollection->numberOfSectors(); isec++) {
	for (unsigned int irow=0;
	     irow<mcTpcHitCollection->sector(isec)->numberOfPadrows(); irow++) {
	    for (StMcTpcHitIterator hitIter =
		     mcTpcHitCollection->sector(isec)->padrow(irow)->hits().begin();
		 hitIter !=
		     mcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();
		 hitIter++) {
		
		// access StAssociationMaker mc tpc hit map information
		StMcTpcHit* mapKey = *hitIter;
		pair<mcTpcHitMapIter,mcTpcHitMapIter> mapBounds =
		    mmcTpcHitMap->equal_range(mapKey);
		
		histograms.mcHitPositionRad->Fill(mapKey->padrow());
		histograms.mcHitPositionZ->Fill(mapKey->position().z());
		
		// iterate over matched rc hits
		int nMatches = 0;
		for (mcTpcHitMapIter mapIter = mapBounds.first;
		     mapIter != mapBounds.second; mapIter++){
		    nMatches++;
		    hitPair.resolution((*mapIter).first,(*mapIter).second);
		    histograms.tpcHitResX->Fill(hitPair.resolution().x());
		    histograms.tpcHitResY->Fill(hitPair.resolution().y());
		    histograms.tpcHitResZ->Fill(hitPair.resolution().z());
		}
		//
		if (!nMatches) {
		    histograms.mcUnmatchedHitPositionSector->Fill(mapKey->sector());
		    histograms.mcUnmatchedHitPositionRad->Fill(mapKey->padrow());
		    histograms.mcUnmatchedHitPositionZ->Fill(mapKey->position().z());
		}
		histograms.matchesToRcHits->Fill(nMatches);
		//
	    }
	}
    }
    
}

//-------------------------------------------------

void StTpcEvalMaker::rcHitIteration() {
    
    cout << "_ StTpcHit iteration ____________________" << endl;
    
    StTpcHitCollection* rcTpcHitCollection =  mStEvent->tpcHitCollection();
    
    if (rcTpcHitCollection) {
	
	for (unsigned int isec=0;
	     isec<rcTpcHitCollection->numberOfSectors(); isec++) {
	    for (unsigned int irow=0;
		 irow<rcTpcHitCollection->sector(isec)->numberOfPadrows(); irow++) {
		for (StSPtrVecTpcHitIterator hitIter =
			 rcTpcHitCollection->sector(isec)->padrow(irow)->hits().begin();
		     hitIter !=
			 rcTpcHitCollection->sector(isec)->padrow(irow)->hits().end();
		     hitIter++) {
		    
		    // access StAssociationMaker recon tpc hit map information
		    StTpcHit* mapKey = *hitIter;
		    
		    histograms.rcHitPositionRad->Fill(mapKey->padrow());
		    histograms.rcHitPositionZ->Fill(mapKey->position().z());
		    
		    unsigned int nMatches = mrcTpcHitMap->count(mapKey);
		    
		    // fill histograms
		    if (nMatches) {
			if (nMatches==1) { // 1 to 1 matches
			    histograms.mc1to1HitPositionRad->Fill(mapKey->padrow());
			    histograms.mc1to1HitPositionZ->Fill(mapKey->position().z());
			}
			if (nMatches>1) { // Merged hits
			    histograms.mcMergedHitPositionRad->Fill(mapKey->padrow());
			    histograms.mcMergedHitPositionZ->Fill(mapKey->position().z());
			}
		    }
		    else {
			histograms.rcUnmatchedHitPositionSector->Fill(mapKey->sector());
			histograms.rcUnmatchedHitPositionRad->Fill(mapKey->padrow());
			histograms.rcUnmatchedHitPositionZ->Fill(mapKey->position().z());
		    }
		    histograms.matchesToMcHits->Fill(nMatches);
		    //
		}
	    }
	}
    }
}

//-------------------------------------------------

void StTpcEvalMaker::mcTrackIteration() {

  // this iteration is useful for calculating acceptances and
  // efficiencies.
  //
  // nothing for vertices yet...

  cout << "_ StMcTrack iteration ___________________" << endl;

  StMcVertex* mcPrimaryVertex = mStMcEvent->primaryVertex();
  StSPtrVecMcTrack mcTrackContainer = mStMcEvent->tracks();

  histograms.mMcMultiplicity = mcTrackContainer.size();
  histograms.mPairMultiplicity = mmcTrackMap->size();
  for (unsigned int iMcTrack = 0;
       iMcTrack < mcTrackContainer.size(); iMcTrack++) {

      
      MatchedTrackPair trackPair;
      
      StMcTrack* mcTrack = mcTrackContainer[iMcTrack];

    // use the primary key from the g2t tables for the mc track id
    // use id = 0 for shower tracks
    // set sign to negative for non-primary vertex tracks
    signed long trackId = mcTrack->key();
    if (mcTrack->startVertex() != mcPrimaryVertex) {
	trackId *= -1;
    }
    if (mcTrack->isShower()) {
	trackId = 0;
    }
    trackPair.mcInfo()->setId(trackId);
    
    addMcTrack(mcTrack,trackPair.mcInfo());
    
    // Count the number of recontstructed tracks associated with the
    // mc track.
    unsigned int nMatchedRcTracks=mmcTrackMap->count(mcTrack);
    trackPair.mcInfo()->setMatchedTracks(nMatchedRcTracks);

    pair<mcTrackMapIter,mcTrackMapIter> mcMapBounds =
      mmcTrackMap->equal_range(mcTrack);

    for (mcTrackMapIter mcMapIter = mcMapBounds.first;
	 mcMapIter != mcMapBounds.second; ++mcMapIter){
	
	//(*mcMapIter).first is the StMcTrack used as the map key
	StTrackPairInfo* assocPair = (*mcMapIter).second; //StAssociationMaker
	StGlobalTrack* rcTrack = assocPair->partnerTrack();

	addRcTrack(rcTrack,trackPair.rcInfo());

	trackPair.setCommonHits(assocPair->commonTpcHits());
	
	// how many StMcTrack are matched to rcTrack?
	unsigned int nMatchedMcTracks=mrcTrackMap->count(rcTrack);
	trackPair.rcInfo()->setMatchedTracks(nMatchedMcTracks);
	
	scanTrackPair(&trackPair, mcTrack, rcTrack); // defined below...

	histograms.fillTrackNtuple(&trackPair); // ntuple row for the pair...
	mTpcEvalEvent->addTrackPair(trackPair);

    } // iterate over matched StGlobalTrack
  }   // iterate over StMcTrack

}

//-------------------------------------------------

void StTpcEvalMaker::rcTrackIteration() {

  // this pass should be used to pick up ghost tracks
  // (the StGlobalTrack not matched to any StMcTrack)

  cout << "_ StGlobalTrack iteration ___________________" << endl;

  StSPtrVecTrackNode& rcTrackNodes = mStEvent->trackNodes();

  histograms.mRcMultiplicity = rcTrackNodes.size();

  for (StSPtrVecTrackNodeIterator nodeIter = rcTrackNodes.begin();
       nodeIter != rcTrackNodes.end(); ++nodeIter) {

    StTrackNode* trackNode = *nodeIter;
    StGlobalTrack* rcTrack =
      dynamic_cast<StGlobalTrack*>(trackNode->track(global));

    if (!rcTrack) continue;

    unsigned int nMatchedMcTracks = mrcTrackMap->count(rcTrack);

    if (nMatchedMcTracks == 0) {
	
	MatchedTrackPair trackPair;
	
	addRcTrack(rcTrack,trackPair.rcInfo());
	histograms.fillTrackNtuple(&trackPair);

	mTpcEvalEvent->addTrackPair(trackPair);
    }

  } // iterate over track nodes

}

//-------------------------------------------------
// the following methods are defined here because
// access is needed to pointers to event data and
// association maps...
//-------------------------------------------------

void StTpcEvalMaker::addMcTrack(StMcTrack* mcTrack, mcTrackInfo* mcInfo) {
  //
  // Fill mcTrackInfo
  //
  if (!mcTrack || !mcInfo) return;

  // momentum vector at start vertex
  mcInfo->setFourMomentum(mcTrack->fourMomentum());

  // number of StMcTpcHit
  StPtrVecMcTpcHit hitContainer = mcTrack->tpcHits();
  mcInfo->setHits(hitContainer.size());

  // count the track hits StMcTpcHit matched to any StTpcHit
  unsigned int matchedHits = 0;
  for (StMcTpcHitIterator i = hitContainer.begin();
       i != hitContainer.end(); i++) 
      if ( mmcTpcHitMap->count(*i) ) matchedHits++;

  mcInfo->setMatchedHits(matchedHits);
}

//-------------------------------------------------

void StTpcEvalMaker::addRcTrack(StGlobalTrack* rcTrack, rcTrackInfo* rcInfo) {
  //
  // Fills rcTrackInfo
  //
  if (!rcTrack || !rcInfo) return;

  
  // To compare the right momentum, use primary tracks.
  // Use the information directly from the primary track.
  // If it is not a primary track, change the sign of the
  // primary key and use the information from the global track.
  StPrimaryTrack* pTrk = dynamic_cast<StPrimaryTrack*>(rcTrack->node()->track(primary));
  if (pTrk) {
      rcInfo->setId(pTrk->key());
      rcInfo->setMomentum(pTrk->geometry()->momentum());
      
  }
  else {
      rcInfo->setId(-1*rcTrack->key());
      rcInfo->setMomentum(rcTrack->geometry()->momentum());     
  }

  // The detector info. is shared by the primary & global,
  // so it doesn't matter which one we use.
  
  // number of StTpcHit hits
  rcInfo->setHits(rcTrack->detectorInfo()->numberOfPoints(kTpcId));
  // number of Fit StTpcHit hits
  rcInfo->setFitHits(rcTrack->fitTraits().numberOfFitPoints(kTpcId));

  // count the track's StTpcHits that are matched to any StMcTpcHit
  // Note that here we just check whether the track's hit is matched,
  // but we don't accumulate the number of matches each hit has.
  StPtrVecHit hitContainer = rcTrack->detectorInfo()->hits(kTpcId);
  int matchedHits = 0;
  for (StPtrVecHitIterator i = hitContainer.begin();
       i != hitContainer.end(); i++) {
    StTpcHit* hitKey = dynamic_cast<StTpcHit*>(*i);
    if (hitKey && mrcTpcHitMap->count(hitKey) ) matchedHits++;
  }
  rcInfo->setMatchedHits(matchedHits);
}

//-------------------------------------------------

void StTpcEvalMaker::scanTrackPair(MatchedTrackPair* trackPair, StMcTrack* mcTrack, StGlobalTrack* rcTrack) {
    //
    // calculate momentum resolution
    // count common, matched hits between mcInfo.track() and rcInfo.track()
    // calculate hit-averaged spatial separation between the track pair
    //
    if (!trackPair) return;
    
    if (!(mcTrack && rcTrack)) return;
    
    // note: StTrack momentum is a StThreeVectorD, StMcTrack
    // momentum is a StThreeVectorF...
    StThreeVectorF momRes, &dp=momRes;
    momRes = trackPair->mcInfo()->fourMomentum().vect() - trackPair->rcInfo()->momentum();
    trackPair->setMomentumResolution(dp);
    
    // iterate over the StTpcHit of rcTrack and check if the
    // matched StMcTpcHit belongs to mcTrack
    // (This means I do it the other way around from what Milton was doing,
    // in order to use the fact that the navigation from StMcTpcHit to
    // StMcTrack is unambiguous, whereas the navigation from StTpcHit to
    // StGlobalTrack is not.)
    
    const StMcTpcHit* mcHit;
    const StTpcHit* rcHit;
    MatchedHitPair hitPair(mStTpcDb);
    
    StPtrVecHit rcTrackHitContainer = rcTrack->detectorInfo()->hits(kTpcId);
    
    for (StHitIterator hitIter = rcTrackHitContainer.begin();
	 hitIter != rcTrackHitContainer.end(); hitIter++) {
	rcHit = dynamic_cast<StTpcHit*>(*hitIter);
	if (!rcHit) continue;
	
	pair<rcTpcHitMapIter,rcTpcHitMapIter> mapBounds =
	    mrcTpcHitMap->equal_range(rcHit);
	
	// iterate over the StMcTpcHits matched to rcHit...
	for (rcTpcHitMapIter mapIter = mapBounds.first;
	     mapIter != mapBounds.second; mapIter++){
	  
	    mcHit = (*mapIter).second; 
	    
	    // does this mcHit belong to the StMcTrack stored in trackPair?
	    
	    StMcTrack* mcTrkFromHit = mcHit->parentTrack();
	    
	    if (mcTrkFromHit == mcTrack) {
		trackPair->addHitResolution(hitPair.resolution(mcHit,rcHit));
		break;
	    }
	}
	
    
    } // iterate over StTpcHit of StGlobalTrack
    
}

//-------------------------------------------------

