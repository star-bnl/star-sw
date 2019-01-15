/**
 * $Id: StMiniMcMaker.cxx,v 1.50 2019/01/15 19:24:33 genevb Exp $
 * \file  StMiniMcMaker.cxx
 * \brief Code to fill the StMiniMcEvent classes from StEvent, StMcEvent and StAssociationMaker
 * 
 *
 * \author Bum Choi, Manuel Calderon de la Barca Sanchez
 * \date   March 2001
 *
 *  
 */
#include "StMiniMcMaker.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TMath.h"
#include "TRandom.h"



#include "Stiostream.h"
#include <assert.h>

#include "StMessMgr.h"
#include "PhysicalConstants.h"
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"
#include "StIOMaker/StIOMaker.h"
#include "StParticleDefinition.hh"
#include "StMatrixF.hh"
#include "StChainOpt.h"
/*
#include "StPionPlus.hh"
#include "StPionMinus.hh"
#include "StProton.hh"
#include "StKaonMinus.hh"
#include "StKaonPlus.hh"
#include "StAntiProton.hh"
#include "StDeuteron.hh"
#include "StElectron.hh"
#include "StPositron.hh"
*/

#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"
#include "StuRefMult.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StuProbabilityPidAlgorithm.h"

#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/projection/StEmcPosition.h"

#include "StMiniMcHelper.h"

#include <vector>
#include <map>
#include "StTrack.h"
#include "StGlobalTrack.h"
#include "StDcaGeometry.h"
#include "StContainers.h"
#include "StTrackDetectorInfo.h"
#include "StEnumerations.h"
#include "StHit.h"
#include "THelixTrack.h"

static int StMiniMcMakerErrorCount=0;

//______________________________________________________________________________
//helper funtion prototypes
void dominatrackInfo(const StTrack*, int& dominatrackKey, short&, float&);

//______________________________________________________________________________
// increasing order
bool hitCmp(const StTpcHit* p1,const StTpcHit* p2)
{
  return (p1->position().perp()<p2->position().perp());
}
//______________________________________________________________________________
// decreasing energy
bool StMcCalorimeterHitCompdE(const StMcCalorimeterHit*  const &lhs,const StMcCalorimeterHit*  const &rhs) {
  return (lhs->dE() > rhs->dE());
}
//______________________________________________________________________________
// decreasing energy
bool StEmcRawHitCompEne(const StEmcRawHit* lhs, const StEmcRawHit* rhs) {
  return (lhs->energy() > rhs->energy());
}

//______________________________________________________________________________
float scaleFactor(double Eta, int hitType=0) {
//     hitType = 0 - towers
//     hitType = 1 - pre shower
//     hitType = 2 - shower max Eta
//     hitType = 3 - shower max Phi
    float P0[]={14.69,559.7,0.1185e6,0.1260e6};
    float P1[]={-0.1022,-109.9,-0.3292e5,-0.1395e5};
    float P2[]={0.7484,-97.81,0.3113e5,0.1971e5};
    
    float x=fabs(Eta);
    return P0[hitType]+P1[hitType]*x+P2[hitType]*x*x;
}

ClassImp(StMiniMcMaker)

//---------CONSTRUCTORS, ETC--------

//______________________________________________________________________________
StMiniMcMaker::StMiniMcMaker(const Char_t *name, const Char_t *title)
  : 
  StMaker(name,title),
  mMiniMcEvent(0),
  mIOMaker(0),
  mMiniMcTree(0),
  mMiniMcDST(0),
  mInFileName(),
  mInFilePrefix(),
  mOutFileName(),
  mOutDir("./"),
  mParameterFileName(),
  mRcEvent(0),
  mMcEvent(0),
  mRcHitMap(0),
  mRcTrackMap(0),
  mMcTrackMap(0),
  mRcVertexPos(0),
  mMcVertexPos(0),
  mTpcDedxAlgo(0),
  mPidAlgo(0),
  mEmcIndex(4801),
  mGhost(kTRUE), 
  mMinPt(0),mMaxPt(99999),
  mNSplit(0),mNRc(0),mNGhost(0),mNContam(0),
  mNMatched(0),mNMatGlob(0), mMainVtx(-1)
    
{
    
  // mParameterFileName = "/auto/data05/snelling/analysis/cvs/PIDTable.root";
  // mParameterFileName = "/auto/pdsfdv05/starhipt/cbum/PIDTable.root";

}

//______________________________________________________________________________
StMiniMcMaker::~StMiniMcMaker()
{
  /* */
} 

//
//---------- StMaker methods ----------
//

/*
  standard Clear()
 */
//______________________________________________________________________________
void StMiniMcMaker::Clear(Option_t* opt)
{
  StMaker::Clear();
}

/*
  called at the end
*/
//______________________________________________________________________________
Int_t StMiniMcMaker::Finish()
{
  cout << "###StMiniMcMaker::Finish()" << endl;

  closeFile();

  cout << "\treconstr.  = " << mNRc << endl
       << "\tmatched    = " << mNMatched << endl
       << "\tsplit      = " << mNSplit << endl
       << "\tcontam.    = " << mNContam << endl
       << "\tghost      = " << mNGhost << endl
       << "\tmat global = " << mNMatGlob << endl;

  if (Debug()) cout << "deleting mMiniMcEvent" << endl;
  delete mMiniMcEvent; mMiniMcEvent = 0;
// 	for some reason, the tree doesn't like to get deleted here...
//   	if (Debug()) cout << "deleting mMiniMcTree" << endl;
//   	delete mMiniMcTree;
//   	mMiniMcTree = 0;
  if (Debug()) cout << "deleting mMiniMcDST" << endl;
  delete mMiniMcDST;
  mMiniMcDST = 0;
  
  return StMaker::Finish();
}
/*
  
 */

//______________________________________________________________________________
Int_t StMiniMcMaker::InitRun(int runID) 
{
  cout << "###StMiniMcMaker::InitRun()" << endl;

  cout << "\tpt cut : " << mMinPt << " , " << mMaxPt << endl;
  Int_t stat=0;

  //
  // instantiate the event object here (embedding or simulation?)
  //
  if (!mMiniMcEvent) {
    if(Debug()) cout << "\tStMiniMcMaker::InitRun Creating StMiniMcEvent..." << endl;
      mMiniMcEvent =  new StMiniMcEvent();
  }
  else {
      if (Debug()) cout << "\tStMiniMcMaker::InitRun StMiniMcEvent Already created" << endl;
  }
  if(mGhost) {
    cout << "\tGhost loop is turned on." << endl;
  }

  //
  // init the tpc dedx algo once
  //
  if (!mTpcDedxAlgo)
      mTpcDedxAlgo = new StTpcDedxPidAlgorithm;
    
  //
  // create file, trees, etc.
  //
  stat = openFile();

  return stat + StMaker::InitRun(runID);

}   
    
//______________________________________________________________________________
Int_t StMiniMcMaker::Init()
{
  //Moved everything important to InitRun(int)
  cout << "###StMiniMcMaker::Init()" << endl;

  cout << "\tpt cut : " << mMinPt << " , " << mMaxPt << endl;
  
  if (mInFileName == "") {
    const StChainOpt *chain = GetChainOpt();
    assert(chain);
    mInFileName = chain->GetFileOut();
  }
  return StMaker::Init();
}   

/*
  Make called every event
 */

//______________________________________________________________________________
Int_t StMiniMcMaker::Make()
{
  if(Debug()) cout << "###StMiniMcMaker::Make()" << endl;
  
  
  //
  // initialize StEvent, StMcEvent, and StAssociationMaker
  //
  mRcEvent = (StEvent*) GetDataSet("StEvent");
  if(!mRcEvent) return kStOk; // last event apparently
  mMcEvent = (StMcEvent*) GetDataSet("StMcEvent");
  if(!mMcEvent) return kStErr;

  //
  // association
  //
  Bool_t assOk = initAssociation();
  if(!assOk) {
    gMessMgr->Warning() << "Association problems " << endm;
//    return kStErr;
  }
  //
  // vertex
  //
  Bool_t vtxOk = initVertex();
  if(!vtxOk) {
    cout << "\t\t----No primary vertex---- " << endl;
    return kStOk;
  }

  // fill emc Helper array to index StEmcRawHits by
  // SoftId.
  buildEmcIndexArray();
  //
  // loop over the tracks
  //
  if (m_Mode == 1) trackLoopIdT();
  else             trackLoop();

  if (Debug()) mMiniMcEvent->Print();
  // Append MC tracks that are not daughters of primary MC vertex. 
  AppendMCDaughterTrack();

  //
  // fill the tree and clear all the tracks
  //
  if (Debug()) mMiniMcEvent->Print();
  mMiniMcTree->Fill();
  mMiniMcEvent->Clear();

  return kStOk;
}

/*
  check if all the association stuff is there
 */

//______________________________________________________________________________
Bool_t StMiniMcMaker::initAssociation()
{
  StAssociationMaker* assoc = 0;
  assoc = (StAssociationMaker*) GetMaker("StAssociationMaker");
  //
  // multimaps
  //
  if (assoc) {
    mRcHitMap   = assoc->rcTpcHitMap();
    mRcTrackMap = assoc->rcTrackMap();
    mMcTrackMap = assoc->mcTrackMap();
  }
  return (assoc && mRcHitMap && mRcTrackMap && mMcTrackMap);
}

/*
  check if we have a valid vertex
 */

//______________________________________________________________________________
Bool_t StMiniMcMaker::initVertex()
{

  if(!mRcEvent->numberOfPrimaryVertices()) {
        cout << "\tno primary vertex from stevent" << endl;
	return kFALSE;
  }
  if(!mMcEvent->primaryVertex()){
        cout << "\tno primary vertex from stmcevent" << endl;
        return kFALSE;
  }
  mMainVtx = -1;
  int maxTks = -1;
  for (int i=0;i<(int)mRcEvent->numberOfPrimaryVertices();i++) {
    int numTks = mRcEvent->primaryVertex(i)->numberOfDaughters();
    if (maxTks > numTks) continue;
    maxTks = numTks;mMainVtx = i;
  }
  if (maxTks <= 0) {
        cout << "\tEmpty primary vertex from stevent" << endl;
	return kFALSE;
  }



  mRcVertexPos = &mRcEvent->primaryVertex(mMainVtx)->position();
  mMcVertexPos = &mMcEvent->primaryVertex()->position();

  if(Debug()){
    cout
      << "----------vertex info---------------------\n"
      << "Position of primary vertex from StEvent: \n"
      << *mRcVertexPos << endl;
    cout
      << "Position of primary vertex from StMcEvent: "<<endl
      << *mMcVertexPos << endl;
    cout << "N daughters, StEvent   : " << mRcEvent->primaryVertex(mMainVtx)->daughters().size() << endl;
    cout << "N daughters, StMcEvent : " << mMcEvent->primaryVertex()->daughters().size() << endl;
  }
  //
  // if there was no primary vertex before embedding,
  // the mc vertex position for each coordinate is equal
  //
  return !((std::isnan(mRcVertexPos->x()) || std::isnan(mRcVertexPos->y())));
}
  
/*

 */
//______________________________________________________________________________
void  StMiniMcMaker::trackLoop()
{
  if(Debug()) cout << "##StMiniMcMaker::trackLoop()" << endl;

  Int_t nMatched(0), nAcceptedRaw(0),nAccepted(0), 
      nMerged(0), nSplit(0), nContam(0), nGhost(0), nMatGlob(0), nContamNew(0),
      nRcGoodGlobal20(0), nRcGlobal(0), nMcGoodGlobal20(0), 
      nMcNch(0), nMcHminus(0), nMcFtpcWNch(0), nMcFtpcENch(0);
      

  RCFOUNDMAP rcFoundMap; // to find split tracks
  MCFOUNDMAP mcFoundMap; // dont look for a rc match to mc tracks 
                         // already flagged as merged or matched
  //
  // create the StMiniMcPair class which will hold all
  // enum Category types.
  //


  //
  // simple loop to associate global tracks
  // Since the primary track loop has all the bells and
  // whistles to do the proper accounting of split, merged, contamination
  // backgrounds, etc.  We will do a much simplified version of the
  // matching loop.
  // Essentially, I will start only from the monte carlo track container as my seeds,
  // -I'll apply some simple acceptance cut (10 tpc hits)
  // -then I will query the association maker for the associated tracks
  // -I will apply a simple 10 fit points cut to the global tracks
  // -I will select the one track with the most common hits among those
  // -enter the matched pair for this global track if there is one.
  // Note that if there is no associated track, there will be no entry.
  // The above is all that's needed in the absence of track merging.
  // To deal with that, I will
  // -Record which global tracks are already entered
  // -if a track has been entered, skip the match.
  // I won't do any more merging accounting, as that is already done for primaries.
  std::vector<int> enteredGlobalTracks;
  const StPtrVecMcTrack& allmcTracks = mMcEvent->tracks();
  cout << "size of mcEvent->tracks() : " << allmcTracks.size() << endl;
  
  StMcTrackConstIterator allMcTrkIter = allmcTracks.begin();
  for ( ; allMcTrkIter != allmcTracks.end(); ++allMcTrkIter) {
      const StMcTrack* mcGlobTrack = *allMcTrkIter;
      if(!acceptRaw(mcGlobTrack)) continue; // loose eta cut (4 units, so should include ftpc).
      if(accept(mcGlobTrack) || mcGlobTrack->ftpcHits().size()>=5) { // 10 tpc hits or 5 ftpc hits
	if (Debug()>1) {
	  cout << "accepted mc global track, key " << mcGlobTrack->key() << endl;
	}
	  // Ok, track is accepted, query the map for its reco tracks.

	StTrackPairInfo* candTrackPair = findBestMatchedGlobal(mcGlobTrack);
	
	  if (candTrackPair) {
	      // ok, found a match! Enter into the array and store the glob id
	      // to only enter a glob track once
	      const StGlobalTrack* glTrack = candTrackPair->partnerTrack();

	      if (Debug()>1) {
		cout << "global match " << endl;
		cout << "mc, rc pt " << mcGlobTrack->pt() << ", " << glTrack->geometry()->momentum().perp() << endl;
	      }
	      
	      if (find(enteredGlobalTracks.begin(),enteredGlobalTracks.end(),glTrack->key())!=enteredGlobalTracks.end()) continue; //if it's already matched, skip it.
	      StMiniMcPair* miniMcPair      = new StMiniMcPair;
	      Int_t commonHits = 
		candTrackPair->commonTpcHits()%100+
		((candTrackPair->commonSvtHits()%10)*100)+
		((candTrackPair->commonSsdHits()%10)*1000);
	      fillTrackPairInfo(miniMcPair, mcGlobTrack,
				0, glTrack, 
				commonHits, mRcTrackMap->count(glTrack),
				mMcTrackMap->count(mcGlobTrack), 0,
				kTRUE);
	      mMiniMcEvent->addTrackPair(miniMcPair,MATGLOB);
	      delete miniMcPair;
	      enteredGlobalTracks.push_back(glTrack->key()); // store the keys of the tracks we've matched.
	      nMatGlob++;
	      
	  }
      }// mc hits condition
  }// end of global track match loop

  // primary track begins here
  //
  // loop over mc tracks.
  //

  const StPtrVecMcTrack& mcTracks = mMcEvent->primaryVertex()->daughters();

  cout << "size of MC primary tracks : " << mcTracks.size() << endl;

  StMcTrackConstIterator mcTrkIter = mcTracks.begin();
  for( ; mcTrkIter != mcTracks.end(); mcTrkIter++){
    const StMcTrack* mcTrack = *mcTrkIter;

    // DEBUG
    if(!mcTrack) { cout << "No mc track? " << endl; continue; }

    //if(!acceptPt(mcTrack)) continue; // pt cut?
    if(!acceptRaw(mcTrack)) continue; // loose eta cuts, etc

    nAcceptedRaw++;

    if(fabs(mcTrack->pseudoRapidity())<.5 && isPrimaryTrack(mcTrack) && mcTrack->particleDefinition()){
	if(mcTrack->particleDefinition()->charge()!=0) nMcNch++;
	if(mcTrack->particleDefinition()->charge()<0) nMcHminus++;
    }
    if(mcTrack->particleDefinition() && mcTrack->particleDefinition()->charge()!=0 && isPrimaryTrack(mcTrack) ) {
        if(mcTrack->pseudoRapidity()<-2.8 && mcTrack->pseudoRapidity()>-3.8) nMcFtpcENch++;
        if(mcTrack->pseudoRapidity()>2.8 && mcTrack->pseudoRapidity()<3.8) nMcFtpcWNch++;
    }

    if(acceptGood20(mcTrack)) nMcGoodGlobal20++;

    Int_t nAssocGl = mMcTrackMap->count(mcTrack);
    Int_t nAssocPr = 0;  // value maybe reset below.

    //
    // minimum requirement to accept the mc track and search a rc match
    //
    if(accept(mcTrack)){ //10 tpc hits

      nAccepted++;

      // check if already included as merged or matched
      //
      if(!mcFoundMap.count(mcTrack->key())){
    
	//
	// follow manuel's example.
	// find the best matched associated rc track only.
	//
	
	PAIRVEC candPair = findMatchedRc(mcTrack);
	nAssocPr = candPair.size(); // # of primaries matched to mcTrack
		
	if(candPair.size()>0){ // match to at least one primary track
	 
	  // find the best matched rc track

	  PAIRVEC::iterator iterBestMatchPair 
	    = max_element(candPair.begin(),candPair.end(),pairCmp);

	  //*** DEBUG
	  /*
	  if(candPair.size()>1){
	    cout << "best:" << endl
		 << "\t" << (*iterBestMatchPair)->commonTpcHits() << endl;
	    cout << "all: " << endl;
	    for(unsigned int j=0; j<candPair.size(); j++){
	      cout << "\t" << candPair[j]->commonTpcHits() << endl;
	    }
	  }
	  */
	  //*** DEBUGEND
	    

	  const StGlobalTrack* glTrack   = (*iterBestMatchPair)->partnerTrack();
	  const StPrimaryTrack* prTrack  = isPrimaryTrack(glTrack);

	  //
	  // 02/15/01
	  // safer idea.  given an mc track, find the pr track
	  // that's best matched.  but then find and sort all the mc tracks
	  // that are best matched to the same pr track.  
	  // the best of these best matched mc tracks will be stored 
	  // as 'matched', while the other will be stored as merged.
	  // the user can then apply a tighter(or looser) common hits
	  // cut downstream to reduce these # of merged.
	  // probably much ado about nothing since the number of 
	  // merged is very small.
	  //
	  
	  PAIRVEC mcMergedPair; // actually, merged or matched pairs
	  
	  pair<rcTrackMapIter,rcTrackMapIter> rcBounds 
	    = mRcTrackMap->equal_range(glTrack);
	  rcTrackMapIter rcMapIter = rcBounds.first;
	  
	  // # of mc tracks matched to this rc track
	  //
	  UInt_t nAssocMc = mRcTrackMap->count(glTrack); 
	  std::vector<UInt_t> nAssocGlVec; // # of rc globals matched to a merged mc cand
	  std::vector<UInt_t> nAssocPrVec; // # of rc primaries matched to a merged mc 
	  //
	  // loop over the mc tracks associated with this rc track
	  //
	  
	  for( ; rcMapIter != rcBounds.second; rcMapIter++){
	    StTrackPairInfo* assocPair = (*rcMapIter).second; 
	    const StMcTrack* mcCandTrack = assocPair->partnerMcTrack();

	    // no pt cut
	    // but must lie with the loose cut (acceptRaw)
	    // and mc hits cut (accept)
	    if(!acceptRaw(mcCandTrack) || !accept(mcCandTrack)) continue; 
	    
	    // 
	    // loop over the rc tracks matched with this cand mc track.
	    // is this mc track best matched with the original rc track?
	    //	    
	    PAIRVEC rcCandPair = findMatchedRc(mcCandTrack);

	    PAIRVEC::iterator iterRcBestPair = 
	      max_element(rcCandPair.begin(),rcCandPair.end(),pairCmp);
	    
	    // yes. this is a merged track candidate.
	    //
	    if((*iterRcBestPair)->partnerTrack() == glTrack){
	      
	      nAssocGlVec.push_back(mMcTrackMap->count(mcCandTrack));
	      nAssocPrVec.push_back(rcCandPair.size());

	      mcMergedPair.push_back(assocPair);
	    }
	  }

	  //
	  // now we have all the possible mc merged tracks to this rc track.
	  // (always sort.  probably not cpu intensive to sort one element)
	  //
	  sort(mcMergedPair.begin(),mcMergedPair.end(),sortCmp);
	  
	  //
	  // ok, save all primary mc tracks.
	  // best 'best' matched is saved as a matched pair.

	  if(Debug()==2 && mcMergedPair.size()>1) {
	    cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" << endl;
	    cout << "MERGED" << endl;
	  }

	  Bool_t foundBest = kFALSE;
	  Bool_t isBestContam = kFALSE; // the best 'best matched' mc track
	                                // is not a primary mc 
	  for(unsigned int i=0; i<mcMergedPair.size(); i++){
	    const StMcTrack* mergedMcTrack = (mcMergedPair[i])->partnerMcTrack();
	    UInt_t mergedCommonHits = (mcMergedPair[i])->commonTpcHits();
	   	    
	    if(Debug()==2 && mcMergedPair.size()>1) {
	      TString hello = (foundBest) ? "yes" : "no";
	      cout << "-----------" << " foundBest? " << hello.Data() << endl;
	      checkMerged(mergedMcTrack, mergedCommonHits,prTrack);
	    }
	    
	    // only primary mc
	    if(!isPrimaryTrack(mergedMcTrack)){
	      if(i==0) isBestContam = kTRUE;
	      continue;
	    }

	    if(!foundBest){
	      // 02/02/02 rc pt cut
	      if(acceptPt(glTrack) || acceptPt(prTrack)){
		StMiniMcPair* miniMcPair = new StMiniMcPair;
		Int_t commonHits = mergedCommonHits%100+
		  ((mcMergedPair[i]->commonSvtHits()%10)*100)+
		  ((mcMergedPair[i]->commonSsdHits()%10)*1000);
		fillTrackPairInfo(miniMcPair, mergedMcTrack, 
				  prTrack, glTrack, 
				  commonHits, nAssocMc,
				  nAssocGlVec[i], nAssocPrVec[i],
				  isBestContam);
		mMiniMcEvent->addTrackPair(miniMcPair,MATCHED);
		delete miniMcPair;
	      }
	      rcFoundMap[prTrack->key()]=1; // the value is meaningless
	      nMatched++;
	      foundBest = kTRUE;
	    }
	    else{
	      // 02/02/02 rc pt cut
	      if(acceptPt(glTrack) || acceptPt(prTrack)){
		StMiniMcPair* miniMcPair = new StMiniMcPair;
		Int_t commonHits = 
		  mergedCommonHits+
		  ((mcMergedPair[i]->commonSvtHits()%10)*100)+
		  ((mcMergedPair[i]->commonSsdHits()%10)*1000);
		fillTrackPairInfo(miniMcPair,mergedMcTrack,prTrack,glTrack,
				  commonHits, nAssocMc,nAssocGlVec[i], 
				  nAssocPrVec[i]);
		 mMiniMcEvent->addTrackPair(miniMcPair,MERGED);
		 delete miniMcPair;
	      }
	      if(Debug()==2 && acceptDebug(mergedMcTrack)) 
		cout << "YES! satisfies cuts" << endl;
	      
	      nMerged++; 

	    }
	    
	    //
	    // flag this mc track so we dont process it again.
	    //
	    mcFoundMap[mergedMcTrack->key()]=1;
	
	  } // 'merged' pair loop
	  if(Debug()==2 && mcMergedPair.size()>1) 
	    cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" << endl;
	} // found a matched rc track
      } // not merged	
    } // accept mc track

    //
    // fill the RAW mc track info here, since we need the number of rc
    // primaries associated with this mc track
    //
    // 02/25/02
    // accept all mc primary tracks within some eta window
    if(acceptRaw(mcTrack)){
      StTinyMcTrack tinyMcTrack;
      fillMcTrackInfo(&tinyMcTrack,mcTrack,nAssocGl,nAssocPr);
      mMiniMcEvent->addMcTrack(&tinyMcTrack);
    }
  } // mc track iter

  //nSplit(0), nContam(0), nGhost(0), nContamNew(0);
  
  //
  // need to loop over the rc primary tracks to get
  // the event centrality values, etc.
  // also look for 'split tracks','ghost tracks'...
  //
	
  Int_t nUncorrectedGlobals(0);
  StSPtrVecTrackNode& trackNode = mRcEvent->trackNodes();
  int nTracks = trackNode.size();
  for (int i=0; i < nTracks; i++) {
    StTrackNode *node = trackNode[i]; 
    if (!node) continue;
    StTrack *glTrack = node->track(global);
    if (! glTrack) continue;
    if(acceptGlobals(glTrack)) nUncorrectedGlobals++;
  }

  Int_t nGoodTrackEta(0), nFtpcWUncorrected(0), nFtpcEUncorrected(0);

  const StSPtrVecPrimaryTrack& prTracks = 
    mRcEvent->primaryVertex(mMainVtx)->daughters();
  
  for(UInt_t i=0; i<prTracks.size(); i++){
    StPrimaryTrack* prTrack = prTracks[i];
    
    //
    // check for positive flag
    //
    if(!ok(prTrack)) continue; 

    const StGlobalTrack* glTrack 
      = static_cast<const StGlobalTrack*>(prTrack->node()->track(global));
    
    
    //
    // centrality 
    //
    if(acceptCentrality(prTrack)) nGoodTrackEta++;
    nRcGlobal++;	//These are identical and ok(prTrack) checks the flag
    if(acceptGood20(glTrack)) nRcGoodGlobal20++;

    //
    // uncorrected negative primaries
    // 02/25/02 no longer used.  uses manuels function in fillEventInfo
    if(acceptFTPC(prTrack) && prTrack->geometry()->momentum().pseudoRapidity() < 0. ) nFtpcEUncorrected++;
    if(acceptFTPC(prTrack) && prTrack->geometry()->momentum().pseudoRapidity() > 0. ) nFtpcWUncorrected++;
    
    //
    // rc pt cut ?
    // 
    if(!acceptPt(glTrack) && !acceptPt(prTrack)) continue;

    //
    // minimum cut
    //
    if(!accept(glTrack) || !accept(prTrack)) continue; 

    // tracks we'll put in the tree.
    mNRc++; // for printing in Finish()

    UInt_t nAssocGl=0, nAssocPr=0;
    UInt_t nAssocMc = mRcTrackMap->count(glTrack);
    
    //
    // tracks that were matched to a mc primary but never best matched..
    // split?
    //
    if(mRcTrackMap->count(glTrack) && !rcFoundMap.count(prTrack->key())){
      
      // make sure the match was best matched to a mc primary
      //

      pair<rcTrackMapIter,rcTrackMapIter> rcBounds 
	= mRcTrackMap->equal_range(glTrack);
   
      PAIRVEC candPair; 

      rcTrackMapIter rcMapIter = rcBounds.first;

      for( ; rcMapIter != rcBounds.second; rcMapIter++){
	StTrackPairInfo* assocPair = (*rcMapIter).second; 
	//const StMcTrack* mcCandTrack = assocPair->partnerMcTrack();

	//
	// new bug.  make sure the mc track was accepted!
	//
	// no pt cut
	//** 01/28/02
	//  now there's absolutely no cut on the mc track.
	// if(!acceptRaw(mcCandTrack) || !accept(mcCandTrack)) continue;

	candPair.push_back(assocPair); // for contamination

      }
      
      //** 01/28/02 - now this should always be true.
      if(candPair.size()){ // found a matching mc track that passes the cuts
	
	PAIRVEC::iterator iterBestMatchPair 
	  = max_element(candPair.begin(),candPair.end(),pairCmp);
	
	const StMcTrack* mcTrack = (*iterBestMatchPair)->partnerMcTrack();
	UInt_t commonHits = (*iterBestMatchPair)->commonTpcHits();
	
	nAssocGl = mMcTrackMap->count(mcTrack);
	
	//
	// how many rc primaries does this mc track match with?
	//
	PAIRVEC matchPair = findMatchedRc(mcTrack);
	nAssocPr = matchPair.size();
	
	//
	// best matched to a mc primary ?
	// yes, it's a split track
	//
	if(isPrimaryTrack(mcTrack)){
	  StMiniMcPair* miniMcPair = new StMiniMcPair;
	  Int_t cHits = 
	    commonHits%100+
	    (((*iterBestMatchPair)->commonSvtHits()%10)*100)+
	    (((*iterBestMatchPair)->commonSsdHits()%10)*1000);
	  fillTrackPairInfo(miniMcPair,mcTrack,prTrack,glTrack,
			    cHits, nAssocMc, nAssocGl, nAssocPr);
	  mMiniMcEvent->addTrackPair(miniMcPair,SPLIT);
	  delete miniMcPair;
	  nSplit++; 
	  
	  //
	  // reality check.  
	  //	  
	  if(Debug()==2) checkSplit(mcTrack,glTrack,commonHits);
	}
	else{ // no, it's best matched to a non primary, contamination
	  StContamPair* contamPair      = new StContamPair;
	  Int_t cHits =
	    commonHits%100+
	    (((*iterBestMatchPair)->commonSvtHits()%10)*100)+
	    (((*iterBestMatchPair)->commonSsdHits()%10)*1000);
	  fillTrackPairInfo(contamPair,mcTrack,
			    prTrack,glTrack,cHits,
			    nAssocMc,nAssocGl,nAssocPr);
	  mMiniMcEvent->addContamPair(contamPair);
	  delete contamPair;
	  if(Debug()>=2) checkContam(mcTrack,glTrack,commonHits);
	  nContam++;
	}
      } // cand.size()?
      else{
	cout << "ERROR- no match for rc track when determining split/contaminations? " << endl;
	exit(1);
      }

    } // split?
    //
    // ghosts?
    //
    else if(mGhost && mRcTrackMap->count(glTrack)==0){
      StMiniMcPair* miniMcPair = new StMiniMcPair;
      fillRcTrackInfo(miniMcPair,
		      prTrack,glTrack,nAssocMc);
      mMiniMcEvent->addTrackPair(miniMcPair,GHOST);
      delete miniMcPair;
      nGhost++;
      if(Debug()) {
	cout << "#############" << endl;
	cout << "GHOST!" << endl;
	cout << "pr pt: " << prTrack->geometry()->momentum().perp() << endl
	     << "TPC fit hits : " << glTrack->fitTraits().numberOfFitPoints(kTpcId)
	     << "SVT fit hits : " << glTrack->fitTraits().numberOfFitPoints(kSvtId)
	     << endl;
      }
    }
  }
  //
  // fill all the event info
  // 

  fillEventInfo(nGoodTrackEta,nRcGlobal,nRcGoodGlobal20,
		nAcceptedRaw,
		nMcGoodGlobal20,nMcNch,nMcHminus,
		nMcFtpcENch, nMcFtpcWNch,nFtpcEUncorrected,nFtpcWUncorrected, 
		nUncorrectedGlobals);
  
  // delete the most probable pid functor
  // 
  //  delete mPidAlgo;

  cout << "\tall rc tracks: " << prTracks.size() << endl;
  cout << "\tn good eta   : " << nGoodTrackEta << endl;
  cout << "\tcentrality   : " << mMiniMcEvent->centrality()  << endl;
  cout << "\tuncorrected  : " << mMiniMcEvent->nUncorrectedPrimaries() << endl;
  cout << "\tall mc tracks: " << mcTracks.size() << endl;
  cout << "\taccepted raw : " << nAcceptedRaw << endl;
  cout << "\taccepted mc  : " << nAccepted << endl;
  cout << "\tmatched rc gl: " << nMatGlob << endl;
  cout << "\tmatched rc pr: " << nMatched << endl;
  cout << "\tmerged rc    : " << nMerged << endl;
  cout << "\tsplit rc     : " << nSplit << endl;
  if(mGhost) {
    cout << "\tghost rc    : " << nGhost << endl;   
    cout << "\tcontam rc   : " << nContam << endl;
    cout << "\tcontam new  : " << nContamNew << endl;
  }

  // counters
  mNSplit += nSplit; mNGhost+= nGhost; mNContam += nContam; 
  mNMatched += nMatched;
  mNMatGlob += nMatGlob;
}
//________________________________________________________________________________
struct MyHolder_t { const StTrack *gl;const StTrack *pr;int hits; float qa;};
void StMiniMcMaker::trackLoopIdT() // match with IdTruth
{


  typedef std::map< int,StTinyMcTrack*>  	McTinyMap_t;
  typedef McTinyMap_t::iterator 		McTinyMapIter_t;

//  struct MyHolder_t { public: const StTrack *gl;const StTrack *pr;int hits; float qa;};

  typedef std::multimap< int,MyHolder_t>  	StTrackMap_t;
  typedef std::pair<int,MyHolder_t>  	        StTrackPair_t;
  typedef StTrackMap_t::const_iterator 		StTrackMapIter_t;

  Int_t nAcceptedRaw(0),nAccepted(0), 
      nRcGoodGlobal20(0), nRcGlobal(0), nMcGoodGlobal20(0), 
      nMcNch(0), nMcHminus(0), nMcFtpcWNch(0), nMcFtpcENch(0);
  Int_t nGoodTrackEta(0), nFtpcWUncorrected(0), nFtpcEUncorrected(0);
	Int_t nUncorrectedGlobals(0);

  const StVertex *rcVertex = mRcEvent->primaryVertex(mMainVtx);

  const StPtrVecMcTrack& mcTracks = mMcEvent->tracks();
  int NMcTracks = mcTracks.size();
  cout << "size of mcEvent->tracks() : " << NMcTracks << endl;

//		Fill global and primary StTrackMap's
  StTrackMap_t glMap,prMap;
  StSPtrVecTrackNode& trackNode = mRcEvent->trackNodes();
  int nTracks = trackNode.size(),myKey;
  for (int i=0; i < nTracks; i++) {
    StTrackNode *node = trackNode[i]; 
    if (!node) 		continue;
    StTrack *glTrack = node->track(global);
    if (! glTrack) 	continue;
    if(acceptGlobals(glTrack)) nUncorrectedGlobals++;

    nRcGlobal++;	
    if(acceptGood20(glTrack)) nRcGoodGlobal20++;

    StPrimaryTrack *prTrack = (StPrimaryTrack*)node->track(primary);
    if (prTrack && prTrack->vertex()!=rcVertex) prTrack=0;
    MyHolder_t h; h.gl = glTrack;h.pr = prTrack;
    dominatTkInfo(glTrack,myKey ,h.hits,h.qa);
    glMap.insert(StTrackPair_t(myKey,h));
    if (!prTrack) 	continue;
    prMap.insert(StTrackPair_t(myKey,h));

    if(acceptCentrality(prTrack)) nGoodTrackEta++;
    if(acceptFTPC(prTrack) && prTrack->geometry()->momentum().pseudoRapidity() < 0. ) nFtpcEUncorrected++;
    if(acceptFTPC(prTrack) && prTrack->geometry()->momentum().pseudoRapidity() > 0. ) nFtpcWUncorrected++;
  }

//		Fill Tiny Mc tracks
    McTinyMap_t  mcTinyMap ;
    for (int k = 0; k < NMcTracks; k++) {
      const StMcTrack* mcTrack = mcTracks[k];
      if (! mcTrack->geantId()) continue;
      int tkKey = mcTrack->key();  if (!tkKey) continue;
      if(!acceptRaw(mcTrack)) 	continue; // loose eta cuts, etc
      nAcceptedRaw++;
      if(!accept(mcTrack))	continue;
      nAccepted++;
      if(acceptGood20(mcTrack)) nMcGoodGlobal20++;
      if(fabs(mcTrack->pseudoRapidity())<.5 && isPrimaryTrack(mcTrack) && mcTrack->particleDefinition()){
	if(mcTrack->particleDefinition()->charge()!=0) nMcNch++;
	if(mcTrack->particleDefinition()->charge() <0) nMcHminus++;
      }
      if(mcTrack->particleDefinition() && mcTrack->particleDefinition()->charge()!=0 && isPrimaryTrack(mcTrack) ) {
        if(mcTrack->pseudoRapidity()<-2.8 && mcTrack->pseudoRapidity()>-3.8) nMcFtpcENch++;
        if(mcTrack->pseudoRapidity()> 2.8 && mcTrack->pseudoRapidity()< 3.8) nMcFtpcWNch++;
      }
      int nAssocGl = glMap.count(tkKey);  
      int nAssocPr = prMap.count(tkKey);  
      StTinyMcTrack *tinyMcTrack = mMiniMcEvent->addMcTrack(0);
      fillMcTrackInfo(tinyMcTrack,mcTrack,nAssocGl,nAssocPr);
      mcTinyMap[tkKey] = tinyMcTrack;
      tinyMcTrack->ResetBit(1);
    }

//		Fill miniMcPair's by matched info

    for (StTrackMapIter_t it =glMap.begin(); it!= glMap.end();++it) {
      int tkKey = (*it).first;
      const MyHolder_t &h = (*it).second;
      StMiniMcPair *miniMcPair = mMiniMcEvent->addTrackPair(0,MATCHED);
      StTinyMcTrack *tinyMcTrack = mcTinyMap[tkKey];
      if (tinyMcTrack) {
        *(StTinyMcTrack*)miniMcPair = *tinyMcTrack;
        tinyMcTrack->SetBit(1);		//Mark this as used
      }  
      fillRcTrackInfo(miniMcPair,h.pr,h.gl,0); 
      miniMcPair->setDominatrack(tkKey);
      miniMcPair->setDominCommonHit(h.hits);
      miniMcPair->setAvgQuality(h.qa);
      miniMcPair->setNCommonHit(h.hits);	//??VP
    }

//		
//		Fill miniMcPair's by UNmatched info
    for( McTinyMapIter_t it =mcTinyMap.begin();it !=mcTinyMap.end();++it) 
    { 
      StTinyMcTrack *tinyMcTrack = (*it).second;
      if (!tinyMcTrack) 		continue;
      if (tinyMcTrack->TestBit(1)) 	continue;
      StMiniMcPair *miniMcPair = mMiniMcEvent->addTrackPair(0,MATCHED);
       *(StTinyMcTrack*)miniMcPair = *tinyMcTrack;
    }




    fillEventInfo(nGoodTrackEta,nRcGlobal,nRcGoodGlobal20,
		  nAcceptedRaw,nMcGoodGlobal20,nMcNch           ,nMcHminus,
		  nMcFtpcENch ,nMcFtpcWNch    ,nFtpcEUncorrected,nFtpcWUncorrected, 
      nUncorrectedGlobals);
}
//______________________________________________________________________________
void StMiniMcMaker::buildEmcIndexArray() 
{
    if (Debug()>1) cout << "StMiniMcMaker::buildEmcIndexArray" << endl;
  // Put all the EMC hits into an array that can then be
  // indexed by SoftId, for use when storing tower info for
  // tracks.

  // zero all pointers in the array
  // Note: vector::clear() is not what should be used
  // clear() removes all the elements from the vector, i.e. the
  // size() after a clear() will be zero.
  for (int softIdNum=1; softIdNum<4801; ++softIdNum) mEmcIndex[softIdNum]=0;

  StEmcGeom* emcGeom = StEmcGeom::getEmcGeom(1);
  if (! mRcEvent->emcCollection()) return;
  StEmcDetector* bemcDet = mRcEvent->emcCollection()->detector(kBarrelEmcTowerId);
  if (! bemcDet) return;
  if (Debug()>1) {
    cout << "emcGeom " << emcGeom << endl;
    cout << "bemcDet " << bemcDet << endl;
  }
  // Loop over modules.  Note, the modules are returned by module number
  // i.e. in the range 1-120 for BEMC.
  for (size_t iMod=1; iMod<=bemcDet->numberOfModules(); ++iMod) {
    StSPtrVecEmcRawHit& modHits = bemcDet->module(iMod)->hits();
    if (Debug()>1) {
      cout << "Module " << iMod << endl;
      cout << "Hits in Module " << modHits.size() << endl;
    }
    for (size_t iHit=0; iHit<modHits.size(); ++iHit) {
      StEmcRawHit* rawHit = modHits[iHit];
      unsigned int softId = rawHit->softId(1); // 1 is "bemc" (see StEmcGeom::getDetNumFromName)
      if (Debug()>2) {
	cout << "iHit  " << iHit << endl;
	cout << "soft Id " << softId << endl;
	cout << "ene, mod, eta, sub   " << rawHit->energy()
	     << ", " << rawHit->module()
	     << ", " << rawHit->eta()
	     << ", " << rawHit->sub()
	     << endl;
      }
      // Make sure the softId is valid.
      // Note: checkId returns 0 (false) when the softId is between 1 and mNRaw
      // so need an extra "!" to use inside the if statement.
      if (!emcGeom->checkId(softId)) { 
	mEmcIndex[softId] = rawHit;
      } //check for valid ID
    } // loop over hits
  } // loop over modules
  return;
}

//________________________________________________________________________________
/*
  Create the output root file and the TTree
*/

Int_t StMiniMcMaker::openFile()
{
  cout << "###StMiniMcMaker::openFile()" << endl;
  
  //
  // for the output root file,
  // - replace geant.root with minimc.root
  // in case there is an event.root, or any other blabla.root, we will
  // replace the blabla.root with minimc.root.  This takes some funky TString
  // manipulation, and will only work if the filename is of the form  somefilestring.blablabla.root
  // it will change it to somefilestring.minimc.root
  // - for the case of running in bfc, the Infilename will have only the extension .root
  //   so we can keep this working in both cases by checking that the first and the last "." are
  //   not the same before attempting to remove what's in between them.
  cout << "Infilename = " << mInFileName << endl;
  if (mInFileName == "") {
    TFile *tfile = GetTFile();
    assert(tfile);
    tfile->cd();
  } else {
    TString outFileName(mInFileName);
    //outFileName.ReplaceAll("geant.root","minimc.root");
    short indx1 = outFileName.First('.');
    short indx2 = outFileName.Last('.');
    if (indx1!=indx2) outFileName.Remove(indx1+1,(indx2-indx1));
    outFileName.Insert(indx1+1,"minimc.");
    outFileName.Prepend(mOutDir);
    
    closeFile();
    cout << "Opening File " << outFileName << endl;
    mMiniMcDST = new TFile(outFileName.Data(),"RECREATE");
    
    if(!mMiniMcDST || !mMiniMcDST->IsOpen()){ 
      gMessMgr->Error() << "Cannot create = " << outFileName << endm;
      return kStErr;
    }
    //   mMiniMcDST->SetFormat(1); 
    cout << "\toutfile = " << outFileName << endl;
    mOutFileName = outFileName;
    //
    // top level tree
    //
  }
  if(Debug()) cout << "##Creating the top level tree..." << endl;
  mMiniMcTree = new TTree("StMiniMcTree","StMiniMcTree");
#if ROOT_VERSION_CODE >= ROOT_VERSION(3,01,05)
  mMiniMcTree->SetBranchStyle(0);
#endif  
  Int_t bufSZ = 64000; // make this bigger?
  mMiniMcTree->Branch("StMiniMcEvent","StMiniMcEvent",&mMiniMcEvent, bufSZ,1);
  mMiniMcTree->SetAutoSave(10000000); // 10 MB
  
  cout << "##...done" << endl;
  
  return kStOk;
}

//______________________________________________________________________________
//  close file and write.
Int_t StMiniMcMaker::closeFile()
{
  cout << "###StMiniMcMaker::closeFile()" << endl;  
  cout << "\tWriting " << mOutFileName << endl;
  
  if(mMiniMcDST && mMiniMcDST->IsOpen()){
    mMiniMcDST->cd();
    mMiniMcTree->Write();
    mMiniMcDST->Write();
    mMiniMcDST->Close();
    delete mMiniMcDST; mMiniMcDST=0;
  }
  mMiniMcTree=0;  //It is deleted by TFile::Close
  cout << "\t...done\n";
  
  return kStOk;
}

//______________________________________________________________________________
void StMiniMcMaker::fillEventInfo(Int_t nGoodTrackEta, Int_t nRcGlobal, Int_t nRcGoodGlobal20,
			          Int_t nMcGlobal, Int_t nMcGoodGlobal20,
			          Int_t nMcNch, Int_t nMcHminus, Int_t nMcFtpcENch, Int_t nMcFtpcWNch, Int_t nFtpcEUncorrected, 
			          Int_t nFtpcWUncorrected, Int_t nUncorrectedGlobals)
{
  mMiniMcEvent->setEventId((Int_t) mRcEvent->id());
  mMiniMcEvent->setRunId((Int_t) mRcEvent->runId());
  mMiniMcEvent->setOriginMult((Int_t)mRcEvent->primaryVertex(mMainVtx)->numberOfDaughters());
  mMiniMcEvent->setCentralMult(nGoodTrackEta);
  
  mMiniMcEvent->setImpact	(mMcEvent->impactParameter()  );
  mMiniMcEvent->setImpactPhi	(mMcEvent->phiReactionPlane() );
  mMiniMcEvent->setTimeOffset	(mMcEvent->triggerTimeOffset());

  mMiniMcEvent->setNMcNch(nMcNch);
  mMiniMcEvent->setNMcFtpcWNch(nMcFtpcWNch);
  mMiniMcEvent->setNMcFtpcENch(nMcFtpcENch);
  mMiniMcEvent->setNMcHminus(nMcHminus);
  
  mMiniMcEvent->setNMcGlobal(nMcGlobal); // from nAcceptedRaw, no point in doing two variables for the same number
  mMiniMcEvent->setNMcGoodGlobal20(nMcGoodGlobal20);
  
  mMiniMcEvent->setNRcGlobal(nRcGlobal);
  mMiniMcEvent->setNRcGoodGlobal20(nRcGoodGlobal20);
  
  mMiniMcEvent->setNUncorrectedNegativePrimaries(uncorrectedNumberOfNegativePrimaries(*mRcEvent));
  mMiniMcEvent->setNUncorrectedPrimaries(uncorrectedNumberOfPrimaries(*mRcEvent));

	mMiniMcEvent->setNUncorrectedGlobals(nUncorrectedGlobals);
  
  mMiniMcEvent->setNFtpcWUncorrectedPrimaries(nFtpcWUncorrected);
  mMiniMcEvent->setNFtpcEUncorrectedPrimaries(nFtpcEUncorrected);
  
  mMiniMcEvent->setCentrality(getIndex((size_t) mMiniMcEvent->nUncorrectedPrimaries()));
  mMiniMcEvent->setMcMult(mMcEvent->numberOfPrimaryTracks());
  
  mMiniMcEvent->setVertexX(mRcVertexPos->x());
  mMiniMcEvent->setVertexY(mRcVertexPos->y());
  mMiniMcEvent->setVertexZ(mRcVertexPos->z());
  StMatrixF C(mRcEvent->primaryVertex(mMainVtx)->covariantMatrix());
  Float_t cov[6] = {C(1,1),C(2,1),C(2,2),C(3,1),C(3,2),C(3,3)};
  mMiniMcEvent->setVertexCovMatrix(cov);
  mMiniMcEvent->setMcVertexX(mMcVertexPos->x());
  mMiniMcEvent->setMcVertexY(mMcVertexPos->y());
  mMiniMcEvent->setMcVertexZ(mMcVertexPos->z());
  
  if (mRcEvent->runInfo()) {
    mMiniMcEvent->setMagField(static_cast<Float_t>(mRcEvent->runInfo()->magneticField()));
    mMiniMcEvent->setBackgroundRate(mRcEvent->runInfo()->backgroundRate());
    mMiniMcEvent->setCenterOfMassEnergy(mRcEvent->runInfo()->centerOfMassEnergy());
    mMiniMcEvent->setBeamMassNumberEast(mRcEvent->runInfo()->beamMassNumber(east));
    mMiniMcEvent->setBeamMassNumberWest(mRcEvent->runInfo()->beamMassNumber(west));
  }
  
  Float_t ctb  = -1., zdce = -1, zdcw = -1;
  
  StTriggerDetectorCollection *triggers 
    = mRcEvent->triggerDetectorCollection();
  if (triggers) {
    StCtbTriggerDetector &CTB = triggers->ctb();
    StZdcTriggerDetector &ZDC = triggers->zdc();
    // get CTB
    for (UInt_t slat=0; slat<CTB.numberOfSlats(); slat++) {
      for (UInt_t tray=0; tray<CTB.numberOfTrays();tray++) {
	ctb += CTB.mips(tray,slat,0);
      }
    }
    //get ZDCe and ZDCw        
    zdce = ZDC.adcSum(east);
    zdcw = ZDC.adcSum(west);
  } 
  
  mMiniMcEvent->setCtb(ctb);
  mMiniMcEvent->setZdcE(zdce);
  mMiniMcEvent->setZdcW(zdcw);
  
}

//______________________________________________________________________________
void StMiniMcMaker::fillTrackPairInfo(	StMiniMcPair* miniMcPair,
				 	const StMcTrack* mcTrack, 
				 	const StTrack* prTrack, 
				 	const StTrack* glTrack,
				 	Int_t commonHits,
				 	Int_t nAssocMc, Int_t nAssocGl, 
				 	Int_t nAssocPr, Bool_t isBestContam)
{
  
  if(mcTrack) fillMcTrackInfo(miniMcPair,mcTrack,nAssocGl,nAssocPr);
  
  if(prTrack || glTrack) fillRcTrackInfo(miniMcPair,prTrack,glTrack,nAssocMc);
  
  // common association info
  miniMcPair->setNCommonHit(commonHits);
  miniMcPair->setIsBestContam(isBestContam);
  // 
  int aeonFlux(-999);
  short aeonFluxHits(0);
  float aeonFluxQuality(-999); // MCBS: name suggested by Jerome.
  if (prTrack) dominatrackInfo(prTrack,aeonFlux,aeonFluxHits,aeonFluxQuality);
  else if (glTrack) dominatrackInfo(glTrack,aeonFlux,aeonFluxHits,aeonFluxQuality);
  miniMcPair->setDominatrack(aeonFlux);
  miniMcPair->setDominCommonHit(aeonFluxHits);
  miniMcPair->setAvgQuality(aeonFluxQuality);
  // special case for contam pairs
  //
  StContamPair* contamPair = dynamic_cast<StContamPair*>(miniMcPair);
  if(contamPair){
    const StMcTrack*    mcTParent = (mcTrack ? mcTrack->parent() : 0);
    
    if (mcTParent){
      contamPair->setParentGeantId(mcTrack->parent()->geantId());
      contamPair->setPtMcParent(mcTrack->parent()->momentum().perp());
      contamPair->setEtaMcParent(mcTrack->parent()->pseudoRapidity());
      contamPair->setGeantProcess(mcTrack->startVertex()->geantProcess());
      
      contamPair->setStartX(mcTrack->startVertex()->position().x());
      contamPair->setStartY(mcTrack->startVertex()->position().y());
      contamPair->setStartZ(mcTrack->startVertex()->position().z());  
      
      Int_t parentParentGeantId=0;
      Float_t parentParentPt=0;
      // check for parent of parent
      if(mcTrack->parent()->parent() && 
	 mcTrack->parent()->parent()->geantId()>0){
	parentParentGeantId = mcTrack->parent()->parent()->geantId();
	parentParentPt = mcTrack->parent()->parent()->momentum().perp();
      }
      contamPair->setParentParentGeantId(parentParentGeantId);
      contamPair->setPtMcParentParent(parentParentPt);
      
      
      //* check if the parent doesnt start from the primary vertex
	  /*
	    if(mcTrack->parent()->parent() && 
	    mcTrack->parent()->parent()->geantId()>0){
	    cout << ">>WARNING: parent doesnt come from the primary vertex!" <<endl
	    << "geant id : " << mcTrack->geantId() << endl
	    << "r parent : " << mcTrack->parent()->startVertex()->position().perp() << endl
	    << "parent geantId: " << mcTrack->parent()->geantId() << endl
	    << "parent's parent: "
	    << mcTrack->parent()->parent()->geantId() << endl
	    << "pr rc pt: " << prTrack->geometry()->momentum().perp() << endl
	    << "pr rc eta: " << prTrack->geometry()->momentum().pseudoRapidity() << endl
	    << "rc fit pts: " << glTrack->fitTraits().numberOfFitPoints(kTpcId) << endl
	    << "daughter pt: " << mcTrack->momentum().perp() << endl
	    << "daughter p: " << mcTrack->momentum().mag() << endl
	    << "daughter eta: " << mcTrack->pseudoRapidity() << endl
	    << "parent pt: " << mcTrack->parent()->momentum().perp() << endl
	    << "parent p: " << mcTrack->parent()->momentum().mag() << endl
	    << "parent eta: " << mcTrack->parent()->pseudoRapidity() << endl
	    << "parent's parent: " << mcTrack->parent()->parent()->momentum().perp() << endl;
	    }
	  */
	  } else {
      StMiniMcMakerErrorCount++;
      if ( StMiniMcMakerErrorCount < 50){
	cout << "StMiniMcMaker::fillTrackPairInfo: WARNING mcTrack->parent() is NULL  !! " 
	     << "   If this comes from a normal simulation or embeding, please report !!" 
	     << "   This is known to happen in pilup mode ONLY ... "                      << endl;
      }
    }
  }
  
}

//______________________________________________________________________________
void StMiniMcMaker::fillRcTrackInfo(StTinyRcTrack* tinyRcTrack,
			            const StTrack* prTrack,
			            const StTrack* glTrack,
			            Int_t nAssocMc)
{
  if (!glTrack) {
    cout << "Error StMiniMcMaker::fillRcTrackInfo, glTrack pointer is zero " << glTrack << endl;
    return;
  }
  const StGlobalTrack *global = (StGlobalTrack*)glTrack;
  const StDcaGeometry* dcaGeo = global->dcaGeometry();
  tinyRcTrack->setValidGl();
  tinyRcTrack->setRecoKey(glTrack->key());
  tinyRcTrack->setDca00(1000.);
  if (dcaGeo ) { 
    tinyRcTrack->setDca00(dcaGeo->impact());
    tinyRcTrack->setDca(1);
    StThreeVectorF glMom = dcaGeo->momentum();
    THelixTrack glHelix = dcaGeo->thelix();
    
     
    enum {	kImpImp=0,
    		kZImp,   kZZ,
    		kPsiImp, kPsiZ, kPsiPsi,
    		kPtiImp, kPtiZ, kPtiPsi, kPtiPti,
    		kTanImp, kTanZ, kTanPsi, kTanPti, kTanTan};
    const float *dcaErr=dcaGeo->errMatrix();
    // the indices of the error matrix correspond to
    // 0 - error on y (track position along pad row direction)
    // 1 - error on z (track position along drift direction)
    // 2 - error on Psi 
    // 3 - error on pt, 
    // 4 - error on tan(dipAngle)

    double pt = glMom.perp();
    tinyRcTrack->setPtGl(pt);
    tinyRcTrack->setPzGl(glMom.z());
    tinyRcTrack->setEtaGl(glMom.pseudoRapidity());
    tinyRcTrack->setPhiGl(glMom.phi()); 
    tinyRcTrack->setCurvGl(dcaGeo->curvature());
    tinyRcTrack->setTanLGl(dcaGeo->tanDip());
    tinyRcTrack->setSeedQuality(glTrack->seedQuality());
    float errorGl[5] = 
      {float(dcaErr[kImpImp])                             ,
       float(dcaErr[kZZ])                                 ,
       float(dcaErr[kPsiPsi])                             ,
       float(dcaErr[kPtiPti]*pow(pt,4))                   ,
       float(dcaErr[kTanTan])
      };
    for (int j=0;j<5;j++) {errorGl[j] = sqrt(errorGl[j]);} 
    tinyRcTrack->setErrGl(errorGl);
    double vtx[3]={mRcVertexPos[0][0],mRcVertexPos[0][1],mRcVertexPos[0][2]};
    double dcaXY,dcaZ;
    glHelix.Dca(vtx,dcaXY,dcaZ,0);
    tinyRcTrack->setDcaXYGl(dcaXY);
    tinyRcTrack->setDcaZGl(dcaZ);
    tinyRcTrack->setDcaGl(sqrt(dcaXY*dcaXY+dcaZ*dcaZ));
    double mcv[3]={mMcVertexPos[0][0],mMcVertexPos[0][1],mMcVertexPos[0][2]};
    glHelix.Dca(mcv,dcaXY,dcaZ,0);
    tinyRcTrack->setDcaXYGlMcV(dcaXY);
    tinyRcTrack->setDcaZGlMcV(dcaZ);
  } else {
    tinyRcTrack->setDca(0);
    const StThreeVectorF& glMom = glTrack->geometry()->momentum();

    const StPhysicalHelixD& glHelix = glTrack->geometry()->helix();



    double pt = glMom.perp();
    tinyRcTrack->setPtGl(pt);
    tinyRcTrack->setPzGl(glMom.z());
    tinyRcTrack->setEtaGl(glMom.pseudoRapidity());
    tinyRcTrack->setPhiGl(glMom.phi()); 
    tinyRcTrack->setCurvGl(glTrack->geometry()->curvature());
    tinyRcTrack->setTanLGl(tan(glTrack->geometry()->dipAngle()));
    tinyRcTrack->setSeedQuality(glTrack->seedQuality());
    StMatrixF gCM = glTrack->fitTraits().covariantMatrix();
//VP Float_t errorGl[5] = {gCM(1,1),gCM(2,2),gCM(3,3),gCM(4,4),gCM(5,5)};
    Float_t errorGl[5] = {
      Float_t(gCM[0][0]*pow(M_PI/180,2))  	,	//YY
      Float_t(gCM[1][1])			,	//ZZ
      Float_t(gCM[3][3]*pow(M_PI/180,2))	,       //PsiPsi
      Float_t(gCM[4][4]*pow(pt,4))		,	//PtPt
      Float_t(gCM[2][2])				//tanLtanL
    };
    for (int j=0;j<5;j++) {errorGl[j] = sqrt(errorGl[j]);} 
    tinyRcTrack->setErrGl(errorGl);
    //
    // reality check
    //
    //  if(fabs(tinyRcTrack->mDcaPr - prTrack->impactParameter())>.001){
    //   cout << " helix : " << tinyRcTrack->mDcaPr 
    //	 << " impact: " << prTrack->impactParameter() << endl;
    // }

    tinyRcTrack->setDcaGl(glTrack->impactParameter());
    tinyRcTrack->setDcaXYGl(computeXY(mRcVertexPos,glTrack));
    //tinyRcTrack->setDcaZGl(computeZDca(mRcVertexPos,glTrack));
    tinyRcTrack->setDcaXYGl(computeXY(mRcVertexPos,glTrack));
    tinyRcTrack->setDcaZGlMcV(dcaz(glHelix,*mMcVertexPos,glTrack));
    tinyRcTrack->setDcaXYGlMcV(computeXY(mMcVertexPos,glTrack));
  }
  
  StDedxPidTraits* pid = findDedxPidTraits(glTrack);
  float meanDedx = (pid) ? pid->mean() : -999;
  tinyRcTrack->setDedx(meanDedx);
  short nDedxPts = (pid) ? pid->numberOfPoints() : 0;
  tinyRcTrack->setDedxPts(nDedxPts);
  
  //
  // common rc info
  //
  // first and last hit
  PAIRHIT hits = findFirstLastHit(glTrack);
  PAIRHIT fitHits = findFirstLastFitHit(glTrack);
  
  if(hits.first){
    //    cout << "first hit z " << hits.first->position().z() << endl;
    tinyRcTrack->setFirstZ(hits.first->position().z());
    tinyRcTrack->setLastZ(hits.second->position().z());
    tinyRcTrack->setFirstPadrow(hits.first->padrow());
    tinyRcTrack->setLastPadrow(hits.second->padrow());
    tinyRcTrack->setFirstSector(hits.first->sector());
    tinyRcTrack->setLastSector(hits.second->sector());
  }
  else if ( glTrack->detectorInfo()->numberOfPoints()==0) {
      // could be an FTPC track, for which firstLastHit won't work
    cout << "Error: no hits?" << endl;
    cout << "Tpc points   : " << glTrack->detectorInfo()->numberOfPoints(kTpcId) << endl;
    cout << "Svt points   : " << glTrack->detectorInfo()->numberOfPoints(kTpcId) << endl;
    cout << "Ftpc points E: " << glTrack->detectorInfo()->numberOfPoints(kFtpcEastId) << endl;
    cout << "Ftpc points W: " << glTrack->detectorInfo()->numberOfPoints(kFtpcWestId) << endl;
  }
  if (fitHits.first) {
    tinyRcTrack->setFirstFitPadrow(fitHits.first->padrow());
    tinyRcTrack->setLastFitPadrow(fitHits.second->padrow());
  }
  else if ( glTrack->fitTraits().numberOfFitPoints()==0) {
    cout << "Error: no hit with usedInFit()>0" << endl;
    cout << "Tpc fit pts   :" << glTrack->fitTraits().numberOfFitPoints(kTpcId) << endl;
    cout << "Svt fit pts   :" << glTrack->fitTraits().numberOfFitPoints(kSvtId) << endl;
    cout << "Ftpc fit pts E:" << glTrack->fitTraits().numberOfFitPoints(kFtpcEastId) << endl;
    cout << "Ftpc fit pts C:" << glTrack->fitTraits().numberOfFitPoints(kFtpcWestId) << endl;
  }
  
  tinyRcTrack->setFitPts(glTrack->fitTraits().numberOfFitPoints(kTpcId));
  tinyRcTrack->setFitSvt(glTrack->fitTraits().numberOfFitPoints(kSvtId));
  tinyRcTrack->setFitSsd(glTrack->fitTraits().numberOfFitPoints(kSsdId));
  size_t ftpcFitPts = 0;
  if (tinyRcTrack->etaGl()>1.8)
    ftpcFitPts = glTrack->fitTraits().numberOfFitPoints(kFtpcWestId);
  if (tinyRcTrack->etaGl()<-1.8)
    ftpcFitPts = glTrack->fitTraits().numberOfFitPoints(kFtpcEastId);
  tinyRcTrack->setFitFtpc(ftpcFitPts);
  tinyRcTrack->setAllPts(glTrack->detectorInfo()->numberOfPoints(kTpcId));
  tinyRcTrack->setCharge(glTrack->geometry()->charge());
  
  tinyRcTrack->setNAssocMc(nAssocMc);
  tinyRcTrack->setNPossible(glTrack->numberOfPossiblePoints(kTpcId));
  
  if (prTrack) {
    tinyRcTrack->setValidPr();
    // with the introduction of the global track branch,
    // having the primary track pointer here is optional. 
    const StThreeVectorF& prMom = prTrack->geometry()->momentum();
    const StPhysicalHelixD& prHelix = prTrack->geometry()->helix();
    
    double pt = prMom.perp();
    tinyRcTrack->setPtPr(pt);
    tinyRcTrack->setPzPr(prMom.z()); 
    tinyRcTrack->setEtaPr(prMom.pseudoRapidity());
    tinyRcTrack->setPhiPr(prMom.phi());
    tinyRcTrack->setCurvPr(prTrack->geometry()->curvature());
    tinyRcTrack->setTanLPr(tan(prTrack->geometry()->dipAngle()));
    tinyRcTrack->setChi2Pr(prTrack->fitTraits().chi2());
    tinyRcTrack->setFlag(prTrack->flag());
    tinyRcTrack->setDcaPr(prTrack->impactParameter());
    tinyRcTrack->setDcaXYPr(computeXY(mRcVertexPos,prTrack));
    //tinyRcTrack->setDcaZPr(computeZDca(mRcVertexPos,prTrack));
    tinyRcTrack->setDcaZPr(dcaz(prHelix,*mRcVertexPos));
    tinyRcTrack->setDcaXYPrMcV(computeXY(mMcVertexPos,prTrack));
    tinyRcTrack->setDcaZPrMcV(dcaz(prHelix,*mMcVertexPos));
    tinyRcTrack->setFitPts(prTrack->fitTraits().numberOfFitPoints(kTpcId));
    tinyRcTrack->setFitSvt(prTrack->fitTraits().numberOfFitPoints(kSvtId));
    tinyRcTrack->setFitSsd(prTrack->fitTraits().numberOfFitPoints(kSsdId));
    StMatrixF pCM = prTrack->fitTraits().covariantMatrix();
    Float_t errorPr[5] = {
      Float_t(pCM[0][0]*pow(M_PI/180,2))     ,		//YY
      Float_t(pCM[1][1])		     ,		//ZZ
      Float_t(pCM[3][3]*pow(M_PI/180,2))     ,		//PsiPsi
      Float_t(pCM[4][4]*pow(pt,4))	     ,		//PtPt
      Float_t(pCM[2][2])	                        //tanLtanL
    };		 	
    for (int j=0;j<5;j++) {errorPr[j] = sqrt(errorPr[j]);} 
    tinyRcTrack->setErrPr(errorPr);
    size_t ftpcFitPts = 0;
    if (tinyRcTrack->etaGl()>1.8)
      ftpcFitPts = prTrack->fitTraits().numberOfFitPoints(kFtpcWestId);
    if (tinyRcTrack->etaGl()<-1.8)
      ftpcFitPts = prTrack->fitTraits().numberOfFitPoints(kFtpcEastId);
    tinyRcTrack->setFitFtpc(ftpcFitPts);
    
  }

  //
  // EMC Information additions (Sept 2007) MCBS
  // Add to the track information about EMC towers.
  // - Find the tower at which the track points.
  // - Obtain all 8 towers (StEmcRawHit) surrounding the above tower (9 total, modulo edge effects)
  // - Sort all 9 towers according to their energy
  // - Store the ADC and energy of the 3 highest towers out of these 9
  // - Store the Soft Id of the highest tower
  // - Store the energy of StEmcPoint related to this track, if any.
  StEmcPosition emcPos;
  StThreeVectorD pos(0,0,0);
  StThreeVectorD mom(0,0,0);
  double magField = mRcEvent->runInfo()->magneticField();

  if (Debug()>2) {
    cout << "fillRcTrack, EMC information" << endl;
    cout << "Extrapolating to BEMC using B Field = " << magField*kilogauss/tesla << " tesla" << endl;
//     if (glTrack) {
//       cout << "hx curvature original from track: " << glTrack->outerGeometry()->curvature() << endl;
//       StPhysicalHelixD helix(glTrack->outerGeometry()->momentum(),glTrack->outerGeometry()->origin(),magField*kilogauss,glTrack->outerGeometry()->charge());
//       cout << "hx curvature as in EmcProjection: " << helix.curvature() << endl;
//    }
  }
  
  // Project Track onto BEMC radius.
  // Use the mag field obtained from run Info above, it
  // will get multiplied by tesla in StEmcPosition::projTrack.
  bool projOk;
  
  if (prTrack) {
    projOk = emcPos.trackOnEmc(&pos,&mom,prTrack,magField*kilogauss/tesla);
  }
  else {
    projOk = emcPos.trackOnEmc(&pos,&mom,glTrack,magField*kilogauss/tesla);
  }
  if (projOk) {
    // Track hits BEMC.  Find the 9 closest towers around the projection.
    // Sort them according to energy.  Keep the highest 3.
    int softIdProj(-1);
    std::vector<StEmcRawHit*> towersOfTrack;
    StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
    emcGeom->getId(pos.phi(),pos.pseudoRapidity(),softIdProj); // softId range [1,4800];
    for(int idEta=-1; idEta<2; ++idEta) {
      for (int idPhi=-1; idPhi<2; ++idPhi) {
	int towerId = emcPos.getNextTowerId(softIdProj,idEta,idPhi);
	if ( towerId == 0 )
	  {
	    continue; // off the end of the barrel
	  }
	if (!emcGeom->checkId(towerId)) {
	  // Valid tower SoftId.  (Again, note that checkId returns "false" when the
	  // softId is valid!)
	  // Get StEmcRawHit corresponding to this softId
	  // here is where the helper Emc container comes in handy
	  StEmcRawHit* emcHit = mEmcIndex[towerId];
	  if (emcHit!=0) {
	    towersOfTrack.push_back(emcHit);
	  } // check valid StEmcRawHit pointer
	}// check valid tower id
      }// dPhi loop
    }// dEta loop
    if (Debug()>1) {
      cout << "Outer Helix " << glTrack->outerGeometry()->helix() << endl;
      cout << "Track Projects to tower " << softIdProj << endl;
      cout << "Track hits at R= " << pos.perp() << " eta,phi: " << pos.pseudoRapidity() << ", " << pos.phi() << endl;
      cout << "Track Has " << towersOfTrack.size() << " total candidate towers" << endl;
    }
    if (towersOfTrack.size()>0) {
      // Obtained all towers (9 at most).
      // Sort them by energy, using helper function defined atop this file
      sort(towersOfTrack.begin(),towersOfTrack.end(),StEmcRawHitCompEne);
      
      // Store the ADC, Energy and SoftId of the 3 highest towers
      size_t maxTowers=3;
      if (towersOfTrack.size()<maxTowers) maxTowers=towersOfTrack.size();
      for (size_t iTow=0; iTow<maxTowers; ++iTow) {
	tinyRcTrack->setEmcTowerAdc(towersOfTrack[iTow]->adc(),iTow);
	tinyRcTrack->setEmcEnergyRcHit(towersOfTrack[iTow]->energy(),iTow);
	tinyRcTrack->setEmcSoftIdHiTowerRc(towersOfTrack[iTow]->softId(1),iTow);
      } //3 highest towers loop
      
    } // make sure there are towers
    else {
      for (size_t iTow=0; iTow<0; ++iTow) {
	tinyRcTrack->setEmcTowerAdc(-9,iTow);
	tinyRcTrack->setEmcEnergyRcHit(-9,iTow);
	tinyRcTrack->setEmcSoftIdHiTowerRc(-9,iTow);
      } //3 highest towers loop
      
    }
    if (Debug()>1) {
      cout << "rc track, key " << tinyRcTrack->recoKey() << endl;
      cout << "n Tpc Fit Pts " << tinyRcTrack->fitPts() << endl;
      cout << "3 bemc hit ene " << tinyRcTrack->emcEnergyRcHit(0)
	   << ", " << tinyRcTrack->emcEnergyRcHit(1)
	   << ", " << tinyRcTrack->emcEnergyRcHit(2) << endl;
      cout << "3 HiTow SoftId Rc " << tinyRcTrack->emcSoftIdHiTowerRc(0)
	   << ", " << tinyRcTrack->emcSoftIdHiTowerRc(1)
	   << ", " << tinyRcTrack->emcSoftIdHiTowerRc(2) << endl;
      float etaTow, phiTow;
      emcGeom->getEtaPhi(tinyRcTrack->emcSoftIdHiTowerRc(0),etaTow,phiTow);
      cout << "Hi Tow eta, phi Rc " <<  etaTow << ", " << phiTow << endl;
      
    }
  }// track has valid projection to EMC.
  return;
}

//______________________________________________________________________________
void  StMiniMcMaker::fillMcTrackInfo(StTinyMcTrack* tinyMcTrack,
			             const StMcTrack* mcTrack,
			             Int_t nAssocGl, Int_t nAssocPr)
{
  if (mcTrack) {
    tinyMcTrack->setValid();
    const StThreeVectorF& mcMom = mcTrack->momentum();
    
    tinyMcTrack->setKey(mcTrack->key());
    tinyMcTrack->setPrimary(mcTrack->IsPrimary());
    tinyMcTrack->setPtMc(mcMom.perp());
    tinyMcTrack->setPzMc(mcMom.z());
    tinyMcTrack->setEtaMc(mcMom.pseudoRapidity());
    tinyMcTrack->setPhiMc(mcMom.phi());
    tinyMcTrack->setNHitMc(mcTrack->tpcHits().size());
    tinyMcTrack->setNSvtHitMc(mcTrack->svtHits().size());
    tinyMcTrack->setNSsdHitMc(mcTrack->ssdHits().size());
    tinyMcTrack->setNFtpcHitMc(mcTrack->ftpcHits().size());
    tinyMcTrack->setGeantId(mcTrack->geantId());
    tinyMcTrack->setPdgId(mcTrack->pdgId());
    short chargeMc = -9999;
    if (mcTrack->particleDefinition()) chargeMc = static_cast<short>(mcTrack->particleDefinition()->charge());
    tinyMcTrack->setChargeMc(chargeMc);
    
    tinyMcTrack->setNAssocGl(nAssocGl);
    tinyMcTrack->setNAssocPr(nAssocPr);
    
    float stopR=(mcTrack->stopVertex()) ? mcTrack->stopVertex()->position().perp() : 999;
    tinyMcTrack->setStopR(stopR);
    
    //  if(stopR<999) cout << ">>stop r=" << stopR << endl;
    tinyMcTrack->setKey(mcTrack->key());
    if (mcTrack->parent()!=0) {
      tinyMcTrack->setParentKey(mcTrack->parent()->key());
      tinyMcTrack->setParentGeantId(mcTrack->parent()->geantId());
    }
    tinyMcTrack->setPtMc(mcMom.perp());
    tinyMcTrack->setPzMc(mcMom.z());
    tinyMcTrack->setEtaMc(mcMom.pseudoRapidity());
    tinyMcTrack->setPhiMc(mcMom.phi());
    tinyMcTrack->setNHitMc(mcTrack->tpcHits().size());
    tinyMcTrack->setNSvtHitMc(mcTrack->svtHits().size());
    tinyMcTrack->setNFtpcHitMc(mcTrack->ftpcHits().size());
    tinyMcTrack->setNBemcHitMc(mcTrack->bemcHits().size());
    tinyMcTrack->setNBprsHitMc(mcTrack->bprsHits().size());
    tinyMcTrack->setNBsmdeHitMc(mcTrack->bsmdeHits().size());
    tinyMcTrack->setNBsmdpHitMc(mcTrack->bsmdpHits().size());
    tinyMcTrack->setNEemcHitMc(mcTrack->eemcHits().size());
    tinyMcTrack->setNBprsHitMc(mcTrack->bprsHits().size());
    tinyMcTrack->setNBsmdeHitMc(mcTrack->bsmdeHits().size());
    tinyMcTrack->setNBsmdpHitMc(mcTrack->bsmdpHits().size());
    tinyMcTrack->setGeantId(mcTrack->geantId());

    tinyMcTrack->setNAssocGl(nAssocGl);
    tinyMcTrack->setNAssocPr(nAssocPr);

    // Fill Mc Emc constMcTrack Information
    // Find the 3 highest energy towers and store them
    // This is done by copying the hits to a vector for sorting
    // according to the hit energy (dE).  Then the first 3 values
    // i.e. the highest values, of dE are copied into the StTinyMcTrack
    // The total sum of dE for all Calorimeter hits is also computed and stored.
    // The softId of the hit with the highest tower is also stored.
    StPtrVecMcCalorimeterHit bemcHits = mcTrack->bemcHits();
    if (bemcHits.size()>0) {
      std::vector<StMcCalorimeterHit*> bemcHitsSorted(bemcHits.size());
      int  hiEnergyHitSoftId(-999);
      double sumEnergy(0);
      copy(bemcHits.begin(),bemcHits.end(),bemcHitsSorted.begin());
      sort(bemcHitsSorted.begin(),bemcHitsSorted.end(),StMcCalorimeterHitCompdE);
      if (Debug()>2) {
	cout << "fillMcTrackInfo() Check sorting of dE for StMcCalorimeterHits" << endl;
      }
      StEmcGeom* emcGeom = StEmcGeom::getEmcGeom("bemc");
      for (StMcCalorimeterHitIterator bhi=bemcHitsSorted.begin();
	   bhi!=bemcHitsSorted.end();
	   ++bhi) {
	StMcCalorimeterHit* bh = *bhi;
	float eta(0);
	emcGeom->getEta(bh->module(),bh->eta(),eta);
	sumEnergy += (*bhi)->dE()*scaleFactor(eta);
	if (Debug()>2) cout << bh->dE()*scaleFactor(eta) << endl;
      }
      // Fill top 3 towers into track class (energy and SoftId), but only if there are enough hits.
      size_t maxHits = 3;
      if (bemcHitsSorted.size()<maxHits) maxHits=bemcHitsSorted.size();
      for (size_t iCalHit=0; iCalHit<maxHits; ++iCalHit) {
	float eta(0);
	emcGeom->getEta(bemcHitsSorted[iCalHit]->module(),bemcHitsSorted[iCalHit]->eta(),eta);	
	tinyMcTrack->setEmcEnergyMcHit(bemcHitsSorted[iCalHit]->dE()*scaleFactor(eta),iCalHit);
	emcGeom->getId(bemcHitsSorted[iCalHit]->module(),
		       bemcHitsSorted[iCalHit]->eta(),
		       bemcHitsSorted[iCalHit]->sub(),hiEnergyHitSoftId);
	tinyMcTrack->setEmcSoftIdHiTowerMc(static_cast<Short_t>(hiEnergyHitSoftId),iCalHit);
      }
      // Fill the rest of the hits, from maxHits up to 3, with zeros
      for (size_t iCalHit2=maxHits; iCalHit2<3; ++iCalHit2) {
	tinyMcTrack->setEmcEnergyMcHit(-9,iCalHit2);	
	tinyMcTrack->setEmcSoftIdHiTowerMc(-9,iCalHit2);
	
      }
      tinyMcTrack->setEmcEnergyMcSum(sumEnergy);

      if (Debug()>1) {
	cout << "mc track, geant Id " << tinyMcTrack->geantId() << endl;
	cout << "parent  geantId    " << tinyMcTrack->parentGeantId() << endl;
	cout << "n Tpc Hits     " << tinyMcTrack->nHitMc() << endl;
	cout << "n Bemc Hits    " << tinyMcTrack->nBemcHitMc() << endl;
	cout << "3 bemc hit ene " << tinyMcTrack->emcEnergyMcHit(0)
	     << ", " << tinyMcTrack->emcEnergyMcHit(1)
	     << ", " << tinyMcTrack->emcEnergyMcHit(2) << endl;
	cout << "MC 3 Hi SoftIds  " << tinyMcTrack->emcSoftIdHiTowerMc(0)
	     << ", " << tinyMcTrack->emcSoftIdHiTowerMc(1)
	     << ", " << tinyMcTrack->emcSoftIdHiTowerMc(2) << endl;
	cout << "emc energy sum   " << tinyMcTrack->emcEnergyMcSum() << endl;
	cout << "MC trk momentum  " << tinyMcTrack->pMc() << endl;
	cout << "MC eta, phi      " << tinyMcTrack->etaMc() << ", " << tinyMcTrack->phiMc() << endl;
	float etaTow, phiTow;
	emcGeom->getEtaPhi(tinyMcTrack->emcSoftIdHiTowerMc(0),etaTow,phiTow);
	cout << "MC HiTow eta,phi " <<  etaTow << ", " << phiTow << endl;
	if (mEmcIndex[tinyMcTrack->emcSoftIdHiTowerMc(0)]) {
	  StEmcRawHit* rawHit = mEmcIndex[tinyMcTrack->emcSoftIdHiTowerMc(0)];
	  cout << "RC Ene for id " << rawHit->energy()
	       << " m= " << rawHit->module()
	       << " e= " << rawHit->eta()
	       << " s= " << rawHit->sub()
	       << endl;
	  //cout << "RC id rawHit  " << rawHit->softId(kBarrelEmcTowerId) << endl; //doesn't work?
	}
	else {
	  cout << "Soft Id Mc " << tinyMcTrack->emcSoftIdHiTowerMc(0) << " has no StEmcRawHit" << endl;
	}
      } // debug

    } // if the track has bemc hits
    //  if(stopR<999) cout << ">>stop r=" << stopR << endl;
  }// if (mcTrack)
  return;
}
/*
  given a mc track, returns a vector of matched associated pairs.
*/
//______________________________________________________________________________
StTrackPairInfo* StMiniMcMaker::findBestMatchedGlobal(const StMcTrack* mcTrack)
{
  pair<mcTrackMapIter,mcTrackMapIter> mcBounds = mMcTrackMap->equal_range((const StMcTrack*)mcTrack);
  StTrackPairInfo* candTrackPair = 0;  // used for finding the best matched track
  const StGlobalTrack* candTrack = 0;
  mcTrackMapIter mcMapIter = mcBounds.first;
  for ( ; mcMapIter != mcBounds.second; ++mcMapIter){
    StTrackPairInfo* assocPair = (*mcMapIter).second;
    const StGlobalTrack* globTrack = assocPair->partnerTrack();
    if (Debug() > 1) {
      cout << * assocPair << endl;
      cout << "globTrack FitPoints Tpc/FtpcE/W = " << globTrack->fitTraits().numberOfFitPoints(kTpcId) 
	   << "/" << globTrack->fitTraits().numberOfFitPoints(kFtpcEastId) 
	   << "/" << globTrack->fitTraits().numberOfFitPoints(kFtpcWestId) << endl; 
    }
    if (!globTrack || globTrack->flag()<=0) continue;
    if (globTrack->fitTraits().numberOfFitPoints(kTpcId)>=10 ||
	globTrack->fitTraits().numberOfFitPoints(kFtpcEastId)>=5 ||
	globTrack->fitTraits().numberOfFitPoints(kFtpcWestId)>=5) {
      if (!candTrackPair) {
	candTrackPair = assocPair;
	candTrack = globTrack;
      }
      else if (globTrack->fitTraits().numberOfFitPoints(kTpcId) > candTrack->fitTraits().numberOfFitPoints(kTpcId)) {
	candTrackPair = assocPair;
	candTrack = globTrack;
      }
      else if (globTrack->fitTraits().numberOfFitPoints(kFtpcEastId) > candTrack->fitTraits().numberOfFitPoints(kFtpcEastId)) {
	candTrackPair = assocPair;
	candTrack = globTrack;
      }
      else if (globTrack->fitTraits().numberOfFitPoints(kFtpcWestId) > candTrack->fitTraits().numberOfFitPoints(kFtpcWestId)) {
	candTrackPair = assocPair;
	candTrack = globTrack;
      }
      
    } // fit points requirement
  }// bounds loop
  return candTrackPair; // Note that candTrack might be zero, for example if only one track is matched and has 9 tpc fit pts.
}
//______________________________________________________________________________
PAIRVEC StMiniMcMaker::findMatchedRc(const StMcTrack* mcTrack)
{
  //
  // now find the associated tracks
  //
  pair<mcTrackMapIter,mcTrackMapIter> mcBounds = mMcTrackMap->equal_range((const StMcTrack*)mcTrack);
  
  PAIRVEC candPair;  // used for finding the best matched track
  
  mcTrackMapIter mcMapIter = mcBounds.first;
  for( ; mcMapIter != mcBounds.second; mcMapIter++){
    StTrackPairInfo* assocPair = (*mcMapIter).second; 
    
    const StGlobalTrack* glTrack  = assocPair->partnerTrack();
    
    //
    // primary tracks only
    //
    const StPrimaryTrack* prTrack=0;
    if(!(prTrack = isPrimaryTrack(glTrack))) continue; 
    
    // conspicuously absent is a pt cut
    
    //
    // share enough hits?
    //
    // if(assocPair->percentOfPairedTpcHits()<mSharedHitsCut) continue;
    
    //
    // bare minimum cut (fit hits, etc.)
    //
    if(!accept(glTrack) || !accept(prTrack)) continue; 
    
    //
    // same sign?
    //
    //if(!isSameSign(prTrack,mcTrack)) continue;
    
    //
    // ok, everything's ok.  save the global track (which is a primary trk).
    // we actually need both the global and primary track info
    //
    candPair.push_back(assocPair);
    
  } // map iter
  
  return candPair;
}

//______________________________________________________________________________
PAIRHIT StMiniMcMaker::findFirstLastHit(const StTrack* track)
{
  const StPtrVecHit& hits = track->detectorInfo()->hits(kTpcId);
  std::vector<const StTpcHit*> vec;
  // fill the vector
  //  cout << "\thits size " << hits.size() << endl;
  for(UInt_t i=0; i<hits.size(); i++){
    const StTpcHit* hit = dynamic_cast<const StTpcHit*>(hits[i]);
    if(!hit) continue;
    vec.push_back(hit);
  }
  sort(vec.begin(),vec.end(),hitCmp);
  if (vec.size()) {
    return PAIRHIT(vec[0],vec[vec.size()-1]);   
  }
  else {
    const StTpcHit* empty = 0;
    return PAIRHIT(empty,empty);
  }
}

//______________________________________________________________________________
PAIRHIT StMiniMcMaker::findFirstLastFitHit(const StTrack* track)
{
  const StPtrVecHit& hits = track->detectorInfo()->hits(kTpcId);
  std::vector<const StTpcHit*> vec;
  // fill the vector
  
  for(UInt_t i=0; i<hits.size(); i++){
    const StTpcHit* hit = dynamic_cast<const StTpcHit*>(hits[i]);
    if(!hit) continue;
    if(!hit->usedInFit()) continue;
    vec.push_back(hit);
  }
  sort(vec.begin(),vec.end(),hitCmp);
  if (vec.size()) {
    return PAIRHIT(vec[0],vec[vec.size()-1]);   
  }
  else {
    const StTpcHit* empty = 0;
    return PAIRHIT(empty,empty);
  }
}

//--------- SIMPLE HELPERS--------------

//______________________________________________________________________________
//  	bare minimum cuts to accept a raw mc track
Bool_t StMiniMcMaker::acceptRaw(const StMcTrack* mcTrack)
{
  return (1);
  // commented out the cuts below, as they made sense for
  // single particle spectra in the TPC, but minimc is now
  // more general, so as long as there is a valid track pointer
  // we take it.
  //return (mcTrack &&
	  //	  mcTrack->particleDefinition()->charge()!=0&&
          //fabs(mcTrack->momentum().pseudoRapidity())<=4.);	  
}

//______________________________________________________________________________
//  	cut on an mc track to determine if we should look 
//  	for a matched rc track.
Bool_t StMiniMcMaker::accept(const StMcTrack* mcTrack)
{
  return (mcTrack && mcTrack->tpcHits().size() >= 10);
}

//______________________________________________________________________________
//  	bare minimum cut to accept a rc track as valid
Bool_t StMiniMcMaker::accept(const StTrack* rcTrack)
{
  UInt_t nFitPoint = rcTrack->fitTraits().numberOfFitPoints(kUnknownId);
  return ( nFitPoint>=5  && rcTrack->flag()>0 );
}

//______________________________________________________________________________
//  		are the mc and rc track of the same sign?
Bool_t StMiniMcMaker::isSameSign(const StTrack* rcTrack,const StMcTrack* mcTrack)
{
  return (rcTrack->geometry()->charge()*
	  mcTrack->particleDefinition()->charge()>0);
}

//______________________________________________________________________________
//  		possible pt cut when looking for split, background rc tracks
Bool_t StMiniMcMaker::acceptPt(const StTrack* track)
{
  return (track->geometry()->momentum().perp()>=mMinPt &&
	  track->geometry()->momentum().perp()<=mMaxPt);
}

//______________________________________________________________________________
//  		possible pt cut on mc tracks
Bool_t StMiniMcMaker::acceptPt(const StMcTrack *track)
{
  return (track->momentum().perp()>=mMinPt &&
	  track->momentum().perp()<=mMaxPt);
}
//______________________________________________________________________________
//  		cut for debugging purposes
Bool_t StMiniMcMaker::acceptDebug(const StMcTrack* track)
{  
  return (track->momentum().pseudoRapidity() <= .1
	  && track->momentum().pseudoRapidity() >= 0 &&
	  (track->geantId()==9 || track->geantId()==12 || track->geantId()==15));
  
}

//______________________________________________________________________________
//  		cut for flow centrality definition
Bool_t StMiniMcMaker::acceptCentrality(const StTrack* track)
{
  return (fabs(track->geometry()->momentum().pseudoRapidity())<.75);
}

//______________________________________________________________________________
//  		cut for h- uncorrected centrality
Bool_t StMiniMcMaker::acceptUncorrected(const StTrack* track)
{
  return (
	  track->geometry()->charge()<0 &&
	  track->fitTraits().numberOfFitPoints(kTpcId)>=10 &&
	  fabs(track->geometry()->momentum().pseudoRapidity())<=0.5 &&
	  track->geometry()->helix().distance(*mRcVertexPos)<3 
	  );    
}

//______________________________________________________________________________
Bool_t StMiniMcMaker::acceptGlobals(const StTrack* track)
{
  return 1;    
}

//______________________________________________________________________________
Bool_t  StMiniMcMaker::acceptFTPC(const StTrack* prTrack)
{
  return 1;
}

//______________________________________________________________________________
//  		positive track flag
Bool_t StMiniMcMaker::ok(const StTrack* track)
{
  return (track && track->flag()>0);
}
//______________________________________________________________________________
//  		Good Global RC Tracks with fitpts > 20
Bool_t StMiniMcMaker::acceptGood20(const StTrack* track)
{
  UInt_t nFitPoint = track->fitTraits().numberOfFitPoints(kTpcId);
  return (track && nFitPoint >= 20);
}

//______________________________________________________________________________
//  		Good Global MC Tracks with fitpts > 20
Bool_t StMiniMcMaker::acceptGood20(const StMcTrack* track)
{
  return (track && track->tpcHits().size() >= 20);
}

//______________________________________________________________________________
//  		checks if the rc track is from the vertex
const StPrimaryTrack* StMiniMcMaker::isPrimaryTrack(const StTrack* glTrack)
{
  if(!glTrack) return 0;
  return dynamic_cast<const StPrimaryTrack*>(glTrack->node()->track(primary));
}

//______________________________________________________________________________
//  		checks if the mc track is from the vertex
Bool_t StMiniMcMaker::isPrimaryTrack(const StMcTrack* mcTrack)
{
  
  return(mcTrack->startVertex() == mMcEvent->primaryVertex());
  
}

//______________________________________________________________________________
//  		xy dca
Float_t StMiniMcMaker::computeXY(const StThreeVectorF* pos, const StTrack* track)
{
  //
  // find the distance between the center of the circle and pos.
  // if the radius of curvature > distance, then call 
  // it positive.
  //
  double xCenter = track->geometry()->helix().xcenter();
  double yCenter = track->geometry()->helix().ycenter();
  double radius  = 1.0/track->geometry()->helix().curvature();
  
  double dPosCenter 
    = TMath::Sqrt( (pos->x()-xCenter) * (pos->x()-xCenter) +
		   (pos->y()-yCenter) * (pos->y()-yCenter));
  
  return (Float_t) ( radius - dPosCenter );
}

//______________________________________________________________________________
//  		z dca from ben norman (no longer used)
Float_t StMiniMcMaker::computeZDca(const StThreeVectorF* point, const StTrack* track)
{
  const StPhysicalHelixD& helix = track->geometry()->helix();
  pairD path = helix.pathLength(point->perp());
  
  const StThreeVectorD& pos1 = helix.at(path.first);
  const StThreeVectorD& pos2 = helix.at(path.second);
  const StThreeVectorD dis1 = *point - pos1;
  const StThreeVectorD dis2 = *point - pos2;
  
  double dcaZ = (dis1.mag() < dis2.mag()) ? dis1.z() : dis2.z();
  if(std::isnan(dcaZ)) return 999;
  return dcaZ;
}
//______________________________________________________________________________
StDedxPidTraits* StMiniMcMaker::findDedxPidTraits(const StTrack* track)
{
  StDedxPidTraits* pid=0;
  StPtrVecTrackPidTraits traits = track->pidTraits(kTpcId);
  
  for (UInt_t i = 0; i < traits.size(); i++) {
    pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
    if (pid && pid->method() == kLikelihoodFitId) break;
  }
  return pid;
}  

//______________________________________________________________________________
//  		debugging
void  StMiniMcMaker::checkMerged(const StMcTrack* mergedMcTrack, Int_t mergedCommonHits,
			         const StTrack* track)
{
  
  if(!isPrimaryTrack(mergedMcTrack))
    cout << "NOT a MC PRIMARY track" << endl;
  
  cout << "rc key       : " << track->key() << endl;
  cout << "rc pr pt     : " 
       << track->geometry()->momentum().perp() << endl;
  cout << "rc pr flag   : " << track->flag() << endl;
  cout << "rc pr dca    : " 
       << track->geometry()->helix().distance(*mRcVertexPos) << endl;
  cout << "mc key       : " << mergedMcTrack->key() << endl;
  cout << "mc evt gen   : " << mergedMcTrack->eventGenLabel() << endl;
  cout << "mc pt : " 
       << mergedMcTrack->momentum().perp() << endl;
  cout << "mc eta: " << mergedMcTrack->momentum().pseudoRapidity() << endl; 
  cout << "mc hits : " 
       << mergedMcTrack->tpcHits().size() << endl;
  cout << "geant   : " << mergedMcTrack->geantId() << endl;
  cout << "common hits : "
       << mergedCommonHits << endl;
  cout << "rc all hits  : " 
       << track->detectorInfo()->numberOfPoints(kTpcId) << endl;
  cout << "rc fit hits  : "
       << track->fitTraits().numberOfFitPoints(kTpcId) << endl;
  
}

//______________________________________________________________________________
//  		debugging
void StMiniMcMaker::checkSplit(const StMcTrack* mcTrack, const StTrack* glTrack,
			  Int_t commonHits)
{
  // find the best rc match to this mc track
  //
  PAIRVEC testPair = findMatchedRc(mcTrack);
  sort(testPair.begin(), testPair.end(), sortCmp);
  
  cout << "#######################################" << endl;
  cout << "CHECK SPLIT:" << endl;
  cout << "mc pt : " 
       << mcTrack->momentum().perp() << endl;
  cout << "mc key: " 
       << mcTrack->key() << endl;
  if (mcTrack->particleDefinition()) {
    cout << "mc charge: " 
	 << mcTrack->particleDefinition()->charge() << endl;
  }
  cout << "mc eta : " 
       << mcTrack->momentum().pseudoRapidity() << endl;
  cout << "mc pts: " 
       << mcTrack->tpcHits().size() << endl;
  cout << "gl pt : " 
       << glTrack->geometry()->momentum().perp() << endl;
  cout << "all hits : " 
       << glTrack->detectorInfo()->numberOfPoints(kTpcId) << endl;
  cout << "fit pts  : " 
       << glTrack->fitTraits().numberOfFitPoints(kTpcId)<< endl;
  cout << "common hits : " << commonHits << endl;
  
  //  if(mcTrack->startVertex() == mMcEvent->primaryVertex()){
  //      cout << "YES mc track is a primary" << endl;
  //  }
  if(testPair.size() == 1) {
    cout << "ERROR in  split. not really a split track. " 
	 << "maybe due to pt cut?" << endl;
    cout << "mc start vertex : " << mcTrack->startVertex()->position() << endl;
    
  }
  cout << ">>Here are the rc tracks" << endl;
  for(unsigned int i=0; i<testPair.size(); i++){
    cout << "i: " << i << endl;
    const StGlobalTrack* testGlTrack = testPair[i]->partnerTrack();
    cout << "pt rc : " 
	 << testGlTrack->geometry()->momentum().perp() << endl;
    cout << "all hits    : " 
	 << testGlTrack-> detectorInfo()->numberOfPoints(kTpcId) << endl;
    cout << "fit pts     : " 
	 << testGlTrack->fitTraits().numberOfFitPoints(kTpcId)<< endl;
    cout << "common hits : " 
	 << testPair[i]->commonTpcHits() << endl;
  }
  cout << "#######################################" << endl;
  
}


//______________________________________________________________________________
void StMiniMcMaker::checkContam(const StMcTrack* mcTrack, const StGlobalTrack* glTrack,
			   Int_t commonHits)
{
  
  cout << "############## " << endl;
  cout << "CHECK CONTAM " << endl;
  
  if(mcTrack->startVertex() == mMcEvent->primaryVertex()){
    cout << "\tERROR mc track is a primary!\n" << endl;
  }
  const StPrimaryTrack* prTrack=isPrimaryTrack(glTrack);
  cout << "common hits : " << commonHits << endl 
       << "rc key : " << prTrack->key() << endl 
       << "mc key : " << mcTrack->key() << endl
       << "pr pt : " << prTrack->geometry()->momentum().perp() << endl 
       << "mc pt : " << mcTrack->momentum().perp() << endl
       << "fit pts : " << glTrack->fitTraits().numberOfFitPoints(kTpcId) 
       << endl 
       << "all pts : "<< glTrack->detectorInfo()->numberOfPoints(kTpcId)<< endl
       << "gl dca xy : " << computeXY(mRcVertexPos,glTrack) << endl;
  cout << ">>Here are all the mc tracks matched to this rc track" << endl;
  
  pair<rcTrackMapIter,rcTrackMapIter> rcBounds 
    = mRcTrackMap->equal_range(glTrack);
  
  rcTrackMapIter rcMapIter = rcBounds.first;
  
  for( ; rcMapIter != rcBounds.second; rcMapIter++){
    StTrackPairInfo* assocPair = (*rcMapIter).second; 
    const StMcTrack* mcCandTrack = assocPair->partnerMcTrack();
    
    cout << "common hits=" << assocPair->commonTpcHits() 
	 << ", is mc primary=" << (isPrimaryTrack(mcTrack)?"yes":"no") 
	 << ", mc key=" << mcCandTrack->key() 
	 << ", hits=" << mcCandTrack->tpcHits().size() << endl;
  }
  cout << ">>Here are the rc tracks matched to this mc track" << endl;
  
  PAIRVEC testPair = findMatchedRc(mcTrack);
  sort(testPair.begin(), testPair.end(), sortCmp);
  
  for(unsigned int i=0; i<testPair.size(); i++){
    cout << "i: " << i << endl;
    const StGlobalTrack* testGlTrack = testPair[i]->partnerTrack();
    
    cout << "common hits=" << testPair[i]->commonTpcHits()
	 << ", rc key=" << testGlTrack->key() << endl;
  }
  
}

//______________________________________________________________________________
size_t StMiniMcMaker::getIndex(size_t mult) 
{
  
  // note: this depends on the production
  //
  
  // P02gd, 2k2 data, Nch cuts
  if (mult >= 510) return 0;
  if (mult >= 431) return 1;
  if (mult >= 312) return 2;
  if (mult >= 217) return 3;
  if (mult >= 146) return 4;
  if (mult >= 94 ) return 5;
  if (mult >= 56 ) return 6;
  if (mult >= 30 ) return 7;
  if (mult >= 14 ) return 8;
  return 9;
}

//______________________________________________________________________________
void StMiniMcMaker::AppendMCDaughterTrack() 
{
    if (Debug())
        cout << "##StMiniMcMaker::AppendMCDaughterTrack()"<< endl;

//  std::vector<int> enteredGlobalTracks;
    const StPtrVecMcTrack& allmcTracks = mMcEvent->tracks();
    cout << "size of mcEvent->tracks() : "<< allmcTracks.size() << endl;

    Int_t nAppendMC(0);
    StMcTrackConstIterator allMcTrkIter = allmcTracks.begin();
    for (; allMcTrkIter != allmcTracks.end(); ++allMcTrkIter) {
        const StMcTrack* mcGlobTrack = *allMcTrkIter;
        if (isPrimaryTrack(mcGlobTrack)) continue;
        if (!acceptRaw(mcGlobTrack)) continue; // loose eta cut (4 units, so should include ftpc).
//      if (accept(mcGlobTrack) || mcGlobTrack->ftpcHits().size()>=5) { // 10 tpc hits or 5 ftpc hits
            if (Debug()>1)
                cout << "accepted mc global track, key "<< mcGlobTrack->key() << endl;
            // Ok, track is accepted, query the map for its reco tracks.

            StTinyMcTrack tinyMcTrack;
            fillMcTrackInfo(&tinyMcTrack, mcGlobTrack, 0, 0);
            mMiniMcEvent->addMcTrack(&tinyMcTrack);
            nAppendMC++;

//      }  // mc hits condition
    }  // end of global track match loop

    cout << "\tappended mc tracks: "<< nAppendMC << endl;
}

//______________________________________________________________________________
void StMiniMcMaker::dominatTkInfo(const StTrack* recTrack,int &dominatrackKey ,int& dominatrackHits,float& avgQuality) {
    // initialize return values.
    // dominatrack key initialized to nonsense, quality initialized to 0.
    // Note, I'm using shorts, which should be ok up to 32768, but if we
    // ever have track keys above this in an event, there will be trouble. 

  int DetectorList[kMaxDetectorId]={0};
  typedef std::map< int,float>  myMap_t;
  typedef myMap_t::const_iterator myIter_t;
  myMap_t  idTruths;
    
    const StPtrVecHit &recHits = recTrack->detectorInfo()->hits();	
// 		Loop to store all the mc track keys and quality of every reco hit on the track.
    int nHits = recHits.size();
    for (int hi=0;hi<nHits; hi++) {
	const StHit* rHit = recHits[hi]; 
        int id = rHit->idTruth(); if (!id) continue;
	int qa = rHit->qaTruth(); if (!qa) qa = 1;
        idTruths[id]+=qa;
    }
    int tkBest=0; float qaBest=0,qaSum=0;
    for (myIter_t it=idTruths.begin(); it!=idTruths.end();++it) {
       qaSum+=(*it).second;
       if ((*it).second<qaBest)	continue;
       tkBest=(*it).first; qaBest=(*it).second;
    }
    dominatrackKey = tkBest; avgQuality = 100*qaBest/(qaSum+1e-10);
    for (int hi=0;hi<nHits; hi++) {
	const StHit* rHit = recHits[hi]; 
        if (rHit->idTruth()!=tkBest) continue;
	DetectorList[rHit->detector()]++;
    }
    if (DetectorList[kTpcId] > 99) DetectorList[kTpcId] = 99;
    if (DetectorList[kSvtId] >  9) DetectorList[kSvtId] =  9;
    if (DetectorList[kSsdId] >  9) DetectorList[kSsdId] =  9;
    dominatrackHits = DetectorList[kTpcId] + 100*(DetectorList[kSvtId] + 10*DetectorList[kSsdId]);
    return;
}
/*
 * $Log: StMiniMcMaker.cxx,v $
 * Revision 1.50  2019/01/15 19:24:33  genevb
 * Kill some memory leaks (thanks, Coverity)
 *
 * Revision 1.49  2018/01/03 18:18:10  genevb
 * idTruths and keys moved from short to int
 *
 * Revision 1.48  2015/07/29 16:34:31  smirnovd
 * Do not store output from function call as it is not used anyway
 *
 * Revision 1.47  2015/07/29 16:34:24  smirnovd
 * Removed defined but not unused local typedefs
 *
 * Revision 1.46  2015/07/29 16:34:15  smirnovd
 * Added std:: to resolve ambiguity for isnan for g++ (4.8)
 *
 * Revision 1.45  2015/04/30 16:03:31  perev
 * Remove redundant automatic CVS comments
 *
 * Revision 1.44  2015/04/28 16:05:32  perev
 * We have changed a default dEdx method in StMuDSTmaker from
 * kTruncatedMeanId to kLikelihoodFitId since SL14a.
 *   With TruncatedMean we are using only 70% of available dE/dx measurements and this is reflected in nHitsDedx
 *   With LikelihoodFit we are using all available dE/dx measurements.
 * Default in StMiniMcEvent is still old (kTruncatedMeanId).
 * It is changed now to kLikelihoodFitId and to be back propagated to all releases >= SL14a
 * Yuri
 *
 * Revision 1.43  2014/07/28 17:20:11  jwebb
 * Explicit casts from (double) to (float) to satisfy c++ 11 compiler.
 *
 * Revision 1.42  2013/04/04 21:31:07  perev
 * The only MC Pairs added
 *
 * Revision 1.41  2012/05/25 18:35:30  perev
 * Wrong DcaGl fixed. Thanx #2362
 *
 * Revision 1.40  2012/03/15 23:37:36  perev
 * Uncorrected globals added(Chris)
 *
 * Revision 1.39  2011/10/05 23:07:50  perev
 * Commrnt++
 *
 * Revision 1.38  2011/07/19 19:18:05  perev
 * Error handling fixed
 *
 * Revision 1.37  2011/04/01 20:02:56  perev
 * IdTruth part rewritten
 *
 * Revision 1.36  2011/03/22 00:32:23  perev
 * Added impact,phi impact & trigger time
 *
 * Revision 1.35  2011/02/22 20:42:52  perev
 * now int parentParentGeantId
 *
 * Revision 1.34  2011/02/16 00:50:30  perev
 * mPdgId added
 *
 * Revision 1.33  2010/08/31 20:16:15  fisyak
 * Add track seedQuality
 *
 * Revision 1.32  2010/08/05 15:31:11  jwebb
 * Changed the check on valid towers during clustering to suppress the non-
 * error issued by StEmcGeom.
 *
 * Revision 1.31  2010/04/15 19:17:27  fisyak
 * Add corrections for AppendMCDaughterTrack from Masayuki Wada
 *
 * Revision 1.30  2010/01/27 21:28:10  perev
 * CleanUp
 *
 * Revision 1.29  2009/02/02 19:30:50  fisyak
 * Set common Hit as no.Tpc + 100*no.Svt + 1000*no.Ssd hits, add protection against empty emcCollection
 *
 * Revision 1.28  2008/07/17 22:50:52  calderon
 * Remove a cut in acceptRaw(const StMcTrack*) that checked on the pseudorapidity.
 * This cut was affecting heavy particles thrown flat in rapidity for embedding.
 *
 * Revision 1.27  2007/12/22 20:31:20  calderon
 * Storing of info of 3 EMC towers for each TinyRcTrack and TinyMcTrack.
 *
 * Revision 1.26  2007/04/26 04:08:05  perev
 * Hide StBFChain dependency
 *
 * Revision 1.25  2007/04/17 05:09:07  perev
 * GetTFile()==>StMaker. Jerome request
 *
 * Revision 1.24  2007/02/23 17:07:41  fisyak
 * Resolve bug #682
 *
 * Revision 1.23  2007/02/21 23:19:36  calderon
 * Remove the forcing of the Ghost loop to be "off" by checking the filename.
 * This caused problems for embedding when the Ghost loop was requested (the check
 * overrode the request).  Now, the check only displays a message in the log
 * saying that the Ghost loop is turned on.
 *
 * Revision 1.22  2006/08/05 01:07:45  calderon
 * Modified some debugging printouts.
 *
 * Revision 1.21  2006/07/24 19:04:11  calderon
 * Added parent key data member to StTinyMcTrack.
 * Added reco key data member to StTinyRcTrack.
 * Added code to fill in those data members to StMiniMcMaker.  Parent key for
 * MC tracks is only filled when track has a valid parent().
 *
 * Revision 1.20  2006/05/22 18:55:16  calderon
 * Changes from the original code by Bum to comply with STAR coding standards.
 * First thing is to change the name of the "Helper" file to something that is more in line with the file naming convention.
 * This does not fully solve all possible hiccups, because all the functions
 * in the "helper" file are defined in global scope.
 *
 * Revision 1.19  2005/09/29 15:53:08  fisyak
 * Persistent StMcEvent
 *
 * Revision 1.18  2004/07/27 19:34:34  jeromel
 * Patch for pileup. Not a primary => a parent but in pileup, not true anymore ...
 *
 * Revision 1.17  2004/05/03 23:28:39  perev
 * double delete of TTree fixed. TFile deletes it himself
 *
 * Revision 1.16  2004/03/31 23:44:36  calderon
 * Function to find the dominatrack, the number of hits belonging to the
 * dominatrack and the average hit quality of those hits (based on idTruth and
 * quality of StHit).
 *
 * Revision 1.15  2004/03/30 03:16:15  calderon
 * Modifications for running in bfc.
 *  - Changed to use StiIOInterface (IOMaker in normal mode, TreeMaker in bfc)
 *  - Cleaned up Init(), InitRun() to handle the changing file names.
 *  - Initialize lots of variables and pointers in constructor.
 *  - Delete some pointers in Finish (deleting the TTree causes a seg fault, though.)
 *  - Note that currently the StHits in the ITTF chain don't have a usedInFit() flag,
 *    so there will be many messages complaining about this.
 *  - Removed the mDebug data member, every Maker already has one, so change
 *    to use that throughout the package.
 *
 * Revision 1.14  2004/03/15 18:59:47  calderon
 * - Added support for encoded common hits.  Now the common hits of the TPC and
 * the SVT are stored, with the corresponding methods to decode and return these
 * values.
 * - Added protection for tracks with no particle definition.
 * - Added () around call to mcMergedPair[i] to make Insure++ happy.
 *
 * Revision 1.13  2004/01/26 13:59:26  calderon
 * Added the code to fill the global track matches of StMiniMcEvent.
 *
 * Revision 1.12  2003/09/02 17:58:43  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.11  2003/07/09 01:07:23  calderon
 * Addition of FTPC reference multiplicity
 * Addition of other multiplicity values for StMiniMcEvent
 * Changes to reflect the use of the setters and getters, no longer
 * access the data members directly.
 *
 * Revision 1.10  2003/05/14 00:12:20  calderon
 * The minimc replaces now whatever it finds between the first and last '.', not
 * just geant.root, in the creation of the output file name.
 *
 * Curvature, tan(lambda) and Covariance matrix diagonal elements are now stored
 * in rcTrack, for both primary and global tracks.
 *
 * Revision 1.9  2003/05/08 02:11:43  calderon
 * Set the data members for the Svt and Ftpc hits for constMcTrack and for the Svt and Ftpc
 * fit points for the RcTrack.
 * Use primary track for the fit points in all cases, not the global tracks.
 * Selection of West or East Ftpc is based on eta>1.8 or eta<1.8
 *
 * Revision 1.8  2002/06/28 22:15:12  calderon
 * Changes to deal with seg. faults in the file name handling:
 * Conventions:
 * StMiniMcMaker looks for the input file from the IO maker to figure out
 * if the file has changed.  This is done using TString::Contains() in Make().
 * Usually we will run one file at a time, but in order not to break Bum's scheme of being
 * able to process several files in one go, this is left as is.  However, for
 * embedding, the file name is not enough, in Eric's new scheme there are repeated
 * file names.  This is resolved by adding a prefix to the output file name.  However,
 * this prefix should not be overwritten, so the current code only replaces the
 * string inside the output file name pertaining to the input file name, and leaves
 * the prefix of the output file intact.  This was done for embedding looking for
 * st_physics, and here is where the problem arose: hijing files begin with a different
 * prefix.  To solve this problem, the input file name prefix is now an input parameter
 * in the macro.
 *
 * StMiniEmbed.C and StMiniHijing.C now conform to this convention.  StMiniEmbed.C
 * did not change its prototype, because all embedding files have st_phyics as prefix.
 * StMiniHijing.C changed its prototype, now it takes as an input argument the prefix,
 * but in order not to break Jenn's scripts if she was already using this macro,
 * this parameter was added at the end and defaults to "rcf", which is appropriate
 * for hijing files reconstructed in rcf.
 *
 * Revision 1.7  2002/06/27 17:30:58  jeromel
 * Bug fix. NULL+1 caused a crash ...
 *
 * Revision 1.6  2002/06/11 19:09:35  calderon
 * Bug fix: the filename that was set in the macro was being overwritten
 * in InitRun, so the emb80x string which was added to the filename was lost.
 * This was fixed by not replacing the filename in InitRun and only replacing
 * the current filename starting from st_physics.
 * and $Id: StMiniMcMaker.cxx,v 1.50 2019/01/15 19:24:33 genevb Exp $ plus header comments for the macros
 *
 * Revision 1.4  2002/06/06 23:22:34  calderon
 * Changes from Jenn:
 * -Add needed libs in StMiniHijing.C
 * -Properly do an InitRun(int runnumber) method
 *  
*/
