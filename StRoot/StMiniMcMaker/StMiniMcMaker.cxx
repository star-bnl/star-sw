/**
 * $Id: StMiniMcMaker.cxx,v 1.5 2002/06/07 02:22:00 calderon Exp $
 * \file  StMiniMcMaker.cxx
 * \brief Code to fill the StMiniMcEvent classes from StEvent, StMcEvent and StAssociationMaker
 * 
 *
 * \author Bum Choi, Manuel Calderon de la Barca Sanchez
 * \date   March 2001
 * $Log: StMiniMcMaker.cxx,v $
 * Revision 1.5  2002/06/07 02:22:00  calderon
 * Protection against empty vector in findFirstLastHit
 * $Log$ and $Id$ plus header comments for the macros
 *
 * Revision 1.4  2002/06/06 23:22:34  calderon
 * Changes from Jenn:
 * -Add needed libs in StMiniHijing.C
 * -Properly do an InitRun(int runnumber) method
 *  
 */
#include "StMiniMcMaker.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TMath.h"
#include "TRandom.h"



#include <iostream>
#include <assert.h>
#include "StMiniMcEvent/StMiniMcEvent.h"

#include "StMessMgr.h"
#include "PhysicalConstants.h"
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"
#include "StIOMaker/StIOMaker.h"
#include "StParticleDefinition.hh"

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
#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StuRefMult.hh"
#include "StTpcDedxPidAlgorithm.h"
#include "StuProbabilityPidAlgorithm.h"

#include "Helper.h"

// increasing order
inline bool hitCmp(StTpcHit* p1, StTpcHit* p2){
  return (p1->position().perp()<p2->position().perp());
}


ClassImp(StMiniMcMaker)

//---------CONSTRUCTORS, ETC--------

StMiniMcMaker::StMiniMcMaker(const Char_t *name, const Char_t *title)
  : 
  StMaker(name,title), 
  mOutDir("./"), 
  mGhost(kFALSE), 
  mDebug(kFALSE),
  mMinPt(0),mMaxPt(99999),
  mBField(-999),
  mNSplit(0),mNRc(0),mNGhost(0),mNContam(0)
    
{
    
  // mParameterFileName = "/auto/data05/snelling/analysis/cvs/PIDTable.root";
  // mParameterFileName = "/auto/pdsfdv05/starhipt/cbum/PIDTable.root";

}

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
void
StMiniMcMaker::Clear(Option_t* opt)
{
  StMaker::Clear();
}

/*
  called at the end
*/
Int_t
StMiniMcMaker::Finish()
{
  cout << "###StMiniMcMaker::Finish()" << endl;

  closeFile();

  cout << "\trc=" << mNRc << endl
       << "\tmatched=" << mNMatched << endl
       << "\tsplit=" << mNSplit << endl
       << "\tcontam=" << mNContam << endl
       << "\tghost=" << mNGhost << endl;



  return StMaker::Finish();
}
/*
  
 */

Int_t
StMiniMcMaker::InitRun(int runID) {
  cout << "###StMiniMcMaker::InitRun()" << endl;

  cout << "\tpt cut : " << mMinPt << " , " << mMaxPt << endl;

  mIOMaker = (StIOMaker*)GetMaker("IO");
  if(mIOMaker) mInFileName = strrchr(mIOMaker->GetFile(),'/')+1;

  //
  // instantiate the event object here (embedding or simulation?)
  //
  if(mDebug) cout << "\tCreating StMiniMcEvent..." << endl;
  mMiniMcEvent =  new StMiniMcEvent();
  if(mGhost) {
    cout << "\tGhost loop on" << endl;
    // double check that we really want the ghost flag
    if(mInFileName.Contains("st_physics")){ // probably not
      mGhost = kFALSE;
      cout << "\tApparently we're looking at real data. " <<endl
	   << "\tTurning off the ghost flag" << endl;
    }
  }

  //
  // init the tpc dedx algo once
  //
  mTpcDedxAlgo = new StTpcDedxPidAlgorithm;
    
  //
  // create file, trees, etc.
  //
  Int_t stat = openFile();

  return stat + StMaker::Init();

}   
    
Int_t
StMiniMcMaker::Init()
{
  //Moved everything important to InitRun(int)
  cout << "###StMiniMcMaker::Init()" << endl;

  cout << "\tpt cut : " << mMinPt << " , " << mMaxPt << endl;

  return StMaker::Init();
}   

/*
  Make called every event
 */

Int_t
StMiniMcMaker::Make()
{
  if(mDebug) cout << "###StMiniMcMaker::Make()" << endl;
  
  Int_t stat=0;
  //
  // if it's a new file, then close the old one and open a new one
  //
  TString curFileName;
  if(mIOMaker) curFileName = strrchr(mIOMaker->GetFile(),'/')+1;
  if(mInFileName!=curFileName){
    if(mDebug) {
      cout << "\tNew file found : " << curFileName << endl
	   << "\tReplacing " << mInFileName << endl;
    }
    closeFile();
    mInFileName = curFileName;
    stat = openFile();
    if(!stat) return stat;
  }
  
  //
  // initialize StEvent, StMcEvent, and StAssociationMaker
  //
  mRcEvent = (StEvent*) GetDataSet("StEvent");
  if(!mRcEvent) return kStOk; // last event apparently
  mMcEvent = ((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
  if(!mMcEvent) return kStErr;
  mRun = (StRun*) GetDataSet("StRun");
  if(!mRun) cout << "Cannot get StRun" << endl;

  //
  // association
  //
  Bool_t assOk = initAssociation();
  if(!assOk) {
    gMessMgr->Warning() << "Association problems " << endm;
    return kStErr;
  }
  //
  // vertex
  //
  Bool_t vtxOk = initVertex();
  if(!vtxOk) {
    cout << "\t\t----No primary vertex---- " << endl;
    return kStOk;
  }

  //
  // loop over the tracks
  //
  trackLoop();

  //
  // fill the tree and clear all the tracks
  //
  mMiniMcTree->Fill();
  mMiniMcEvent->Clear();

  return kStOk;
}

/*
  check if all the association stuff is there
 */

Bool_t
StMiniMcMaker::initAssociation()
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

Bool_t
StMiniMcMaker::initVertex()
{

  if(!mRcEvent->primaryVertex(0)) {
        cout << "\tno primary vertex from stevent" << endl;
	return kFALSE;
  }
  if(!mMcEvent->primaryVertex()){
        cout << "\tno primary vertex from stmcevent" << endl;
        return kFALSE;
  }

  mRcVertexPos = &mRcEvent->primaryVertex(0)->position();
  mMcVertexPos = &mMcEvent->primaryVertex()->position();

  if(mDebug){
    cout
      << "----------vertex info---------------------\n"
      << "Position of primary vertex from StEvent: \n"
      << *mRcVertexPos << endl;
    cout
      << "Position of primary vertex from StMcEvent: "<<endl
      << *mMcVertexPos << endl;
  }
  //
  // if there was no primary vertex before embedding,
  // the mc vertex position for each coordinate is equal
  //
  return !((isnan(mRcVertexPos->x()) || isnan(mRcVertexPos->y()) ||
	    (mMcVertexPos->x() == mMcVertexPos->y() &&
	     mMcVertexPos->y() == mMcVertexPos->z() &&
	     mMcVertexPos->z() == mMcVertexPos->x()))) ;
}
  
/*

 */
void 
StMiniMcMaker::trackLoop()
{
  if(mDebug) cout << "##StMiniMcMaker::trackLoop()" << endl;

  Int_t nMatched(0), nAcceptedRaw(0),nAccepted(0), 
    nMerged(0), nSplit(0), nContam(0), nGhost(0), nContamNew(0);

  RCFOUNDMAP rcFoundMap; // to find split tracks
  MCFOUNDMAP mcFoundMap; // dont look for a rc match to mc tracks 
                         // already flagged as merged or matched
  //
  // create the StMiniMcPair class which will hold all
  // enum Category types.
  //
  StMiniMcPair* miniMcPair      = new StMiniMcPair;
  StContamPair* contamPair      = new StContamPair;
  StTinyMcTrack* tinyMcTrack    = new StTinyMcTrack;

  //
  // loop over mc tracks.
  //

  const StPtrVecMcTrack& mcTracks = mMcEvent->primaryVertex()->daughters();

  cout << "size of mctracks : " << mcTracks.size() << endl;

  StMcTrackConstIterator mcTrkIter = mcTracks.begin();
  for( ; mcTrkIter != mcTracks.end(); mcTrkIter++){
    StMcTrack* mcTrack = *mcTrkIter;

    // DEBUG
    if(!mcTrack) { cout << "No mc track? " << endl; continue; }

    //if(!acceptPt(mcTrack)) continue; // pt cut?
    if(!acceptRaw(mcTrack)) continue; // loose eta cuts, etc

    nAcceptedRaw++;
    
    Int_t nAssocGl = mMcTrackMap->count(mcTrack);
    Int_t nAssocPr = 0;  // value maybe reset below.

    //
    // minimum requirement to accept the mc track and search a rc match
    //
    if(accept(mcTrack)){

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
	    

	  StGlobalTrack* glTrack   = (*iterBestMatchPair)->partnerTrack();
	  StPrimaryTrack* prTrack  = isPrimaryTrack(glTrack);
	  	
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
	  vector<UInt_t> nAssocGlVec; // # of rc globals matched to a merged mc cand
	  vector<UInt_t> nAssocPrVec; // # of rc primaries matched to a merged mc 
	  //
	  // loop over the mc tracks associated with this rc track
	  //
	  
	  for( ; rcMapIter != rcBounds.second; rcMapIter++){
	    StTrackPairInfo* assocPair = (*rcMapIter).second; 
	    StMcTrack* mcCandTrack = assocPair->partnerMcTrack();

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

	  if(mDebug==2 && mcMergedPair.size()>1) {
	    cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" << endl;
	    cout << "MERGED" << endl;
	  }

	  Bool_t foundBest = kFALSE;
	  Bool_t isBestContam = kFALSE; // the best 'best matched' mc track
	                                // is not a primary mc 
	  for(unsigned int i=0; i<mcMergedPair.size(); i++){
	    StMcTrack* mergedMcTrack = mcMergedPair[i]->partnerMcTrack();
	    UInt_t mergedCommonHits = mcMergedPair[i]->commonTpcHits();
	   	    
	    if(mDebug==2 && mcMergedPair.size()>1) {
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
		fillTrackPairInfo(miniMcPair, mergedMcTrack, 
				  prTrack, glTrack, 
				  mergedCommonHits, nAssocMc,
				  nAssocGlVec[i], nAssocPrVec[i],
				  isBestContam);
		mMiniMcEvent->addTrackPair(miniMcPair,MATCHED);
	      }
	      rcFoundMap[prTrack->key()]=1; // the value is meaningless
	      nMatched++;
	      foundBest = kTRUE;

	    }
	    else{
	      // 02/02/02 rc pt cut
	      if(acceptPt(glTrack) || acceptPt(prTrack)){
		fillTrackPairInfo(miniMcPair,mergedMcTrack,prTrack,glTrack,
				  mergedCommonHits, nAssocMc,nAssocGlVec[i], 
				  nAssocPrVec[i]);
		 mMiniMcEvent->addTrackPair(miniMcPair,MERGED);
	      }
	      if(mDebug==2 && acceptDebug(mergedMcTrack)) 
		cout << "YES! satisfies cuts" << endl;
	      
	      nMerged++; 

	    }
	    
	    //
	    // flag this mc track so we dont process it again.
	    //
	    mcFoundMap[mergedMcTrack->key()]=1;
	
	  } // 'merged' pair loop

	  if(mDebug==2 && mcMergedPair.size()>1) 
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
      fillMcTrackInfo(tinyMcTrack,mcTrack,nAssocGl,nAssocPr);
      mMiniMcEvent->addMcTrack(tinyMcTrack);
    }
  } // mc track iter

  //
  // need to loop over the rc primary tracks to get
  // the event centrality values, etc.
  // also look for 'split tracks','ghost tracks'...
  //
  Int_t nGoodTrackEta(0), nUncorrected(0);

  const StSPtrVecPrimaryTrack& prTracks = 
    mRcEvent->primaryVertex(0)->daughters();
  
  for(UInt_t i=0; i<prTracks.size(); i++){
    StPrimaryTrack* prTrack = prTracks[i];
    
    //
    // check for positive flag
    //
    if(!ok(prTrack)) continue; 

    StGlobalTrack* glTrack 
      = static_cast<StGlobalTrack*>(prTrack->node()->track(global));

    //
    // centrality 
    //
    if(acceptCentrality(prTrack)) nGoodTrackEta++;

    //
    // uncorrected negative primaries
    // 02/25/02 no longer used.  uses manuels function in fillEventInfo
    if(acceptUncorrected(prTrack)) nUncorrected++;
    
    //
    // rc pt cut ?
    // 
    if(!acceptPt(glTrack) && !acceptPt(prTrack)) continue;

    //
    // minimum cut
    //
    if(!accept(glTrack) || !accept(prTrack)) continue; 

    mNRc++;

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
	//StMcTrack* mcCandTrack = assocPair->partnerMcTrack();

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
	
	StMcTrack* mcTrack = (*iterBestMatchPair)->partnerMcTrack();
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
	  
	  fillTrackPairInfo(miniMcPair,mcTrack,prTrack,glTrack,
			    commonHits, nAssocMc, nAssocGl, nAssocPr);
	  mMiniMcEvent->addTrackPair(miniMcPair,SPLIT);
	  
	  nSplit++; 
	  
	  //
	  // reality check.  
	  //	  
	  if(mDebug==2) checkSplit(mcTrack,glTrack,commonHits);
	}
	else{ // no, it's best matched to a non primary, contamination
	  
	  fillTrackPairInfo(contamPair,mcTrack,
			    prTrack,glTrack,commonHits,
			    nAssocMc,nAssocGl,nAssocPr);
	  mMiniMcEvent->addTrackPair(contamPair,CONTAM);
	  
	  if(mDebug==2) checkContam(mcTrack,glTrack,commonHits);
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
      fillRcTrackInfo(miniMcPair,
		      prTrack,glTrack,nAssocMc);
      mMiniMcEvent->addTrackPair(miniMcPair,GHOST); 
      nGhost++;
      if(mDebug) {
	cout << "#############" << endl;
	cout << "GHOST!" << endl;
	cout << "pr pt: " << prTrack->geometry()->momentum().perp() << endl
	     << "fit hits : " << glTrack->fitTraits().numberOfFitPoints(kTpcId)
	     << endl;
      }
    }
  }
  //
  // fill all the event info
  // 

  fillEventInfo(nGoodTrackEta);
  
  delete miniMcPair;
  delete contamPair;
  delete tinyMcTrack;

  // delete the most probable pid functor
  // 
  //  delete mPidAlgo;

  cout << "\tall rc tracks: " << prTracks.size() << endl;
  cout << "\tn good eta   : " << nGoodTrackEta << endl;
  cout << "\tcentrality   : " << mMiniMcEvent->mCentrality  << endl;
  cout << "\tuncorrected  : " << nUncorrected << endl;
  cout << "\tall mc tracks: " << mcTracks.size() << endl;
  cout << "\taccepted raw : " << nAcceptedRaw << endl;
  cout << "\taccepted mc  : " << nAccepted << endl;
  cout << "\tmatched rc   : " << nMatched << endl;
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

}

/*
  Create the output root file and the TTree
 */

Int_t
StMiniMcMaker::openFile()
{
  cout << "###StMiniMcMaker::openFile()" << endl;
  
  //
  // for the output root file, replace geant.root with minimc.root
  //
  cout << "Infilename = " << mInFileName << endl;
  TString outFileName(mInFileName);
  outFileName.ReplaceAll("geant.root","minimc.root");
  outFileName.Prepend(mOutDir + "/");

  mMiniMcDST = 0;
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
  if(mDebug) cout << "##Creating the top level tree..." << endl;

  mMiniMcTree = new TTree("StMiniMcTree","StMiniMcTree");
  if(!mMiniMcTree){
    gMessMgr->Error() << "Cannot create StMiniMcTree" << endm;
    return kStErr;
  }
#if ROOT_VERSION_CODE >= ROOT_VERSION(3,01,05)
  mMiniMcTree->SetBranchStyle(0);
#endif  
  Int_t bufSZ = 64000; // make this bigger?
  mMiniMcTree->Branch("StMiniMcEvent","StMiniMcEvent",&mMiniMcEvent, bufSZ,1);
  mMiniMcTree->SetAutoSave(10000000); // 10 MB
  
  cout << "##...done" << endl;

  return kStOk;
}

/*
  close file and write.
 */

Int_t
StMiniMcMaker::closeFile()
{
  cout << "###StMiniMcMaker::closeFile()" << endl;  
  cout << "\tWriting " << mOutFileName << endl;

  if(mMiniMcDST && mMiniMcDST->IsOpen()){
    mMiniMcDST->Write();
    mMiniMcDST->Close();
  }

  cout << "\t...done\n";

  return kStOk;
}
  
/*
  
 */

void
StMiniMcMaker::fillEventInfo(Int_t nGoodTrackEta)
{
  mMiniMcEvent->mEventId = (Int_t) mRcEvent->id();
  mMiniMcEvent->mRunId   = (Int_t) mRcEvent->runId();
  mMiniMcEvent->mOriginMult  = 
    (Int_t)mRcEvent->primaryVertex(0)->numberOfDaughters();
  mMiniMcEvent->mCentralMult = nGoodTrackEta;

  
  mMiniMcEvent->mNUncorrectedNegativePrimaries = 
    uncorrectedNumberOfNegativePrimaries(*mRcEvent);
  
  mMiniMcEvent->mNUncorrectedPrimaries = 
    uncorrectedNumberOfPrimaries(*mRcEvent);

  mMiniMcEvent->setCentrality(nGoodTrackEta);
  mMiniMcEvent->mMcMult      = mMcEvent->numberOfPrimaryTracks();
  
  mMiniMcEvent->mVertexX     = mRcVertexPos->x();
  mMiniMcEvent->mVertexY     = mRcVertexPos->y();
  mMiniMcEvent->mVertexZ     = mRcVertexPos->z();

  mMiniMcEvent->mMcVertexX     = mMcVertexPos->x();
  mMiniMcEvent->mMcVertexY     = mMcVertexPos->y();
  mMiniMcEvent->mMcVertexZ     = mMcVertexPos->z();

  mMiniMcEvent->mMagField    = static_cast<Float_t>(mRcEvent->runInfo()->magneticField());
  
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
  
  mMiniMcEvent->mCTB = ctb;
  mMiniMcEvent->mZDCe = zdce;
  mMiniMcEvent->mZDCw = zdcw;

}
  
void
StMiniMcMaker::fillTrackPairInfo(StMiniMcPair* miniMcPair,
				 const StMcTrack* mcTrack, 
				 const StTrack* prTrack, 
				 const StTrack* glTrack,
				 Int_t commonHits,
				 Int_t nAssocMc, Int_t nAssocGl, 
				 Int_t nAssocPr, Bool_t isBestContam)
{

  if(mcTrack) fillMcTrackInfo(miniMcPair,mcTrack,nAssocGl,nAssocPr);

  if(prTrack) fillRcTrackInfo(miniMcPair,prTrack,glTrack,nAssocMc);
  
  // common association info
  miniMcPair->setNCommonHit(commonHits);
  miniMcPair->setIsBestContam(isBestContam);

  // special case for contam pairs
  //
  StContamPair* contamPair = dynamic_cast<StContamPair*>(miniMcPair);
  if(contamPair){
    contamPair->setParentGeantId(mcTrack->parent()->geantId());
    contamPair->setPtMcParent(mcTrack->parent()->momentum().perp());
    contamPair->setGeantProcess(mcTrack->startVertex()->geantProcess());
    contamPair->setEtaMcParent(mcTrack->parent()->pseudoRapidity());
    contamPair->setStartX(mcTrack->startVertex()->position().x());
    contamPair->setStartY(mcTrack->startVertex()->position().y());
    contamPair->setStartZ(mcTrack->startVertex()->position().z());  

    Short_t parentParentGeantId=0;
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
  }


}

/*

 */
void
StMiniMcMaker::fillRcTrackInfo(StTinyRcTrack* tinyRcTrack,
			       const StTrack* prTrack,
			       const StTrack* glTrack,
			       Int_t nAssocMc)
{

  
  const StThreeVectorF& prMom = prTrack->geometry()->momentum();
  const StThreeVectorF& glMom = glTrack->geometry()->momentum();
  
  const StPhysicalHelixD& glHelix = glTrack->geometry()->helix();
  const StPhysicalHelixD& prHelix = prTrack->geometry()->helix();

  tinyRcTrack->setPtPr(prMom.perp());
  tinyRcTrack->setPzPr(prMom.z());
  tinyRcTrack->setEtaPr(prMom.pseudoRapidity());
  tinyRcTrack->setPhiPr(prMom.phi());
  
  tinyRcTrack->setPtGl(glMom.perp());
  tinyRcTrack->setPzGl(glMom.z());
  tinyRcTrack->setEtaGl(glMom.pseudoRapidity());
  tinyRcTrack->setPhiGl(glMom.phi()); 

  tinyRcTrack->setChi2Pr(prTrack->fitTraits().chi2());
  tinyRcTrack->setFlag(prTrack->flag());

  StDedxPidTraits* pid = findDedxPidTraits(prTrack);
  float meanDedx = (pid) ? pid->mean() : -999;
  tinyRcTrack->setDedx(meanDedx);
  
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
  tinyRcTrack->setDcaZGl(dcaz(glHelix,*mRcVertexPos,glTrack));
    
  tinyRcTrack->setDcaPr(prTrack->impactParameter());
  tinyRcTrack->setDcaXYPr(computeXY(mRcVertexPos,prTrack));
  //tinyRcTrack->setDcaZPr(computeZDca(mRcVertexPos,prTrack));
  tinyRcTrack->setDcaZPr(dcaz(prHelix,*mRcVertexPos));


  //
  // pid stuff from the flow maker
  //
  /*
  prTrack->pidTraits(*mTpcDedxAlgo);       // initialize

  Float_t nSigma = 0;
  
  if(prTrack->geometry()->charge()>0){
      
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StPionPlus::instance());
    tinyRcTrack->setPidPion(Int_t(nSigma*1000)/1000.0);
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StProton::instance());
    tinyRcTrack->setPidProton(Int_t(nSigma*1000)/1000.0);
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StKaonPlus::instance());
    tinyRcTrack->setPidKaon(Int_t(nSigma*1000)/1000.0);
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StPositron::instance());
    tinyRcTrack->setPidElectron(Int_t(nSigma*1000)/1000.0);
    
    }
  else {
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StPionMinus::instance());
    tinyRcTrack->setPidPion(Int_t(nSigma*1000)/1000.0);
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StAntiProton::instance());
    tinyRcTrack->setPidProton(Int_t(nSigma*1000)/1000.0);
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StKaonMinus::instance());
    tinyRcTrack->setPidKaon(Int_t(nSigma*1000)/1000.0);
    nSigma = (Float_t)mTpcDedxAlgo->numberOfSigma(StElectron::instance());
    tinyRcTrack->setPidElectron(Int_t(nSigma*1000)/1000.0);
  }
  */

  //
  // most probable pid
  //
  //const StParticleDefinition* def = prTrack->pidTraits(*mPidAlgo);
  //def->charge(); // does nothing.

  //tinyRcTrack->setMostLikelihoodPID = mPidAlgo->mostLikelihoodParticleGeantID();
  //tinyRcTrack->setMostLikelihoodProb = mPidAlgo->mostLikelihoodProbability();
  //tinyRcTrack->setExtrapTag = (Int_t)(mPidAlgo->isExtrap()) ? 0 : 1;

 
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
  else{
    cout << "Error: no hits?" << endl;
    cout << "tpc points : " << glTrack->detectorInfo()->numberOfPoints(kTpcId) << endl;
  }
  if (fitHits.first) {
    tinyRcTrack->setFirstFitPadrow(fitHits.first->padrow());
    tinyRcTrack->setLastFitPadrow(fitHits.second->padrow());
  }
  else {
    cout << "Error: no hit with usedInFit()>0" << endl;
    cout << "fit pts :" << glTrack->fitTraits().numberOfFitPoints(kTpcId) << endl;
  }
  
  tinyRcTrack->setFitPts(glTrack->fitTraits().numberOfFitPoints(kTpcId));
  short nDedxPts = (pid) ? pid->numberOfPoints() : 0;
  tinyRcTrack->setDedxPts(nDedxPts);
  tinyRcTrack->setAllPts(glTrack->detectorInfo()->numberOfPoints(kTpcId));
  tinyRcTrack->setCharge(glTrack->geometry()->charge());

  tinyRcTrack->setNAssocMc(nAssocMc);
  tinyRcTrack->setNPossible(glTrack->numberOfPossiblePoints(kTpcId));

  return;
}

/*
 */

void 
StMiniMcMaker::fillMcTrackInfo(StTinyMcTrack* tinyMcTrack,
			       const StMcTrack* mcTrack,
			       Int_t nAssocGl, Int_t nAssocPr)
{

  const StThreeVectorF& mcMom = mcTrack->momentum();
    
  // calculate the curvature
  // the mc field was preset !
  
  //  Float_t curvMc = .003*mBField/mcTrack->momentum().perp();
  
  tinyMcTrack->setPtMc(mcMom.perp());
  tinyMcTrack->setPzMc(mcMom.z());
  tinyMcTrack->setEtaMc(mcMom.pseudoRapidity());
  tinyMcTrack->setPhiMc(mcMom.phi());
  tinyMcTrack->setNHitMc(mcTrack->tpcHits().size());
  tinyMcTrack->setGeantId(mcTrack->geantId());
  tinyMcTrack->setChargeMc(static_cast<short>(mcTrack->particleDefinition()->charge()));

  tinyMcTrack->setNAssocGl(nAssocGl);
  tinyMcTrack->setNAssocPr(nAssocPr);

  float stopR=(mcTrack->stopVertex()) ? mcTrack->stopVertex()->position().perp() : 999;
  tinyMcTrack->setStopR(stopR);

  //  if(stopR<999) cout << ">>stop r=" << stopR << endl;

} 

/*
  given a mc track, returns a vector of matched associated pairs.
 */

PAIRVEC
StMiniMcMaker::findMatchedRc(StMcTrack* mcTrack)
{
  //
    // now find the associated tracks
    //
    pair<mcTrackMapIter,mcTrackMapIter> mcBounds 
      = mMcTrackMap->equal_range(mcTrack);
    
    PAIRVEC candPair;  // used for finding the best matched track

    mcTrackMapIter mcMapIter = mcBounds.first;
    for( ; mcMapIter != mcBounds.second; mcMapIter++){
      StTrackPairInfo* assocPair = (*mcMapIter).second; 

      StGlobalTrack* glTrack  = assocPair->partnerTrack();
      
      //
      // primary tracks only
      //
      StPrimaryTrack* prTrack=0;
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

/*
  
*/
PAIRHIT
StMiniMcMaker::findFirstLastHit(const StTrack* track)
{
  const StPtrVecHit& hits = track->detectorInfo()->hits(kTpcId);
  vector<StTpcHit*> vec;
  // fill the vector
  //  cout << "\thits size " << hits.size() << endl;
  for(UInt_t i=0; i<hits.size(); i++){
    StTpcHit* hit = dynamic_cast<StTpcHit*>(hits[i]);
    if(!hit) continue;
      vec.push_back(hit);
  }
  sort(vec.begin(),vec.end(),hitCmp);
  if (vec.size()) {
      return PAIRHIT(vec[0],vec[vec.size()-1]);   
  }
  else {
      StTpcHit* empty = 0;
      return PAIRHIT(empty,empty);
  }
}

PAIRHIT
StMiniMcMaker::findFirstLastFitHit(const StTrack* track)
{
  const StPtrVecHit& hits = track->detectorInfo()->hits(kTpcId);
  vector<StTpcHit*> vec;
  // fill the vector

  for(UInt_t i=0; i<hits.size(); i++){
    StTpcHit* hit = dynamic_cast<StTpcHit*>(hits[i]);
    if(!hit) continue;
    if(!hit->usedInFit()) continue;
    vec.push_back(hit);
  }
  sort(vec.begin(),vec.end(),hitCmp);
  if (vec.size()) {
      return PAIRHIT(vec[0],vec[vec.size()-1]);   
  }
  else {
      StTpcHit* empty = 0;
      return PAIRHIT(empty,empty);
  }
}

//--------- SIMPLE HELPERS--------------

/*
  bare minimum cuts to accept a raw mc track
 */
Bool_t
StMiniMcMaker::acceptRaw(StMcTrack* mcTrack)
{
  return (mcTrack &&
//	  mcTrack->particleDefinition()->charge()!=0&&
	  fabs(mcTrack->momentum().pseudoRapidity())<=2.);	  
}

/*
  cut on an mc track to determine if we should look 
  for a matched rc track.
 */
Bool_t
StMiniMcMaker::accept(StMcTrack* mcTrack){
  return (mcTrack && mcTrack->tpcHits().size() >= 10);
}

/*
  bare minimum cut to accept a rc track as valid
 */
Bool_t
StMiniMcMaker::accept(StTrack* rcTrack){
  UInt_t nFitPoint = rcTrack->fitTraits().numberOfFitPoints(kTpcId);
  return ( rcTrack &&
	   rcTrack->impactParameter() < 3. &&
	   nFitPoint>=3                    &&
	   rcTrack->flag()>0               &&
	   rcTrack->geometry()->momentum().perp() < 40 );
}

/*
  are the mc and rc track of the same sign?
 */
inline Bool_t
StMiniMcMaker::isSameSign(StTrack* rcTrack,StMcTrack* mcTrack){
  return (rcTrack->geometry()->charge()*
	  mcTrack->particleDefinition()->charge()>0);
}

/*
  possible pt cut when looking for split, background rc tracks
 */
inline Bool_t 
StMiniMcMaker::acceptPt(StTrack* track){
  return (track->geometry()->momentum().perp()>=mMinPt &&
	  track->geometry()->momentum().perp()<=mMaxPt);
}

/*
  possible pt cut on mc tracks
 */
inline Bool_t
StMiniMcMaker::acceptPt(StMcTrack *track){
  return (track->momentum().perp()>=mMinPt &&
	  track->momentum().perp()<=mMaxPt);
}
/*
  cut for debugging purposes
 */
inline Bool_t
StMiniMcMaker::acceptDebug(StMcTrack* track)
{  
  return (track->momentum().pseudoRapidity() <= .1
	  && track->momentum().pseudoRapidity() >= 0 &&
	  (track->geantId()==9 || track->geantId()==12 || track->geantId()==15));

}

/*
  cut for flow centrality definition
 */
inline Bool_t
StMiniMcMaker::acceptCentrality(StTrack* track)
{
  return (fabs(track->geometry()->momentum().pseudoRapidity())<.75);
}

/*
  cut for h- uncorrected centrality
 */
Bool_t
StMiniMcMaker::acceptUncorrected(StTrack* track)
{
  return (
	  track->geometry()->charge()<0 &&
	  track->fitTraits().numberOfFitPoints(kTpcId)>=10 &&
	  fabs(track->geometry()->momentum().pseudoRapidity())<=0.5 &&
	  track->geometry()->helix().distance(*mRcVertexPos)<3 
	  );    
}

/*
  positive track flag
 */
inline Bool_t
StMiniMcMaker::ok(StTrack* track)
{
  return (track && track->flag()>0);
}

/*
  checks if the rc track is from the vertex
 */
StPrimaryTrack*
StMiniMcMaker::isPrimaryTrack(StTrack* glTrack)
{
  if(!glTrack) return 0;
  return dynamic_cast<StPrimaryTrack*>(glTrack->node()->track(primary));
}
  
/*
  checks if the mc track is from the vertex
 */
inline Bool_t
StMiniMcMaker::isPrimaryTrack(StMcTrack* mcTrack)
{

  return(mcTrack->startVertex() == mMcEvent->primaryVertex());

}

/*
  xy dca
 */

Float_t
StMiniMcMaker::computeXY(const StThreeVectorF* pos, const StTrack* track)
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

/*
  just find the xy and z projection of the 3d dca
*/
/*
pair<Float_t,Float_t>
StMiniMcMaker::computeProj(const StThreeVectorF* pos, const StTrack* track)
{
  //
  // compute the 3d dca
  //
  
  Double_t s = track->geometry()->helix().pathLength(*pos);

  StThreeVectorD dcaPos = track->geometry()->helix().at(s);

  StThreeVectorD distance = dcaPos - *pos;

  return pair<Float_t,Float_t>((Float_t) distance.perp(), 
			       (Float_t)distance.z());  
}
*/
/*
  z dca from ben norman (no longer used)
*/

Float_t
StMiniMcMaker::computeZDca(const StThreeVectorF* point, const StTrack* track)
{
  const StPhysicalHelixD& helix = track->geometry()->helix();
  pairD path = helix.pathLength(point->perp());

  const StThreeVectorD& pos1 = helix.at(path.first);
  const StThreeVectorD& pos2 = helix.at(path.second);
  const StThreeVectorD dis1 = *point - pos1;
  const StThreeVectorD dis2 = *point - pos2;
  
  double dcaZ = (dis1.mag() < dis2.mag()) ? dis1.z() : dis2.z();
  if(isnan(dcaZ)) return 999;
  return dcaZ;


}

/*
  
 */
StDedxPidTraits*
StMiniMcMaker::findDedxPidTraits(const StTrack* track)
{
  StDedxPidTraits* pid=0;
  StPtrVecTrackPidTraits traits = track->pidTraits(kTpcId);
    
  for (UInt_t i = 0; i < traits.size(); i++) {
    pid = dynamic_cast<StDedxPidTraits*>(traits[i]);
    if (pid && pid->method() == kTruncatedMeanId) break;
  }
  return pid;
}  

/*
  debugging
 */

void 
StMiniMcMaker::checkMerged(StMcTrack* mergedMcTrack, Int_t mergedCommonHits,
			   StTrack* track)
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

/*
  debugging
 */

void 
StMiniMcMaker::checkSplit(StMcTrack* mcTrack, StTrack* glTrack,
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
  cout << "mc charge: " 
       << mcTrack->particleDefinition()->charge() << endl;
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
    StGlobalTrack* testGlTrack = testPair[i]->partnerTrack();
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


void 
StMiniMcMaker::checkContam(StMcTrack* mcTrack, StGlobalTrack* glTrack,
			   Int_t commonHits)
{
  
  cout << "############## " << endl;
  cout << "CHECK CONTAM " << endl;

  if(mcTrack->startVertex() == mMcEvent->primaryVertex()){
    cout << "\tERROR mc track is a primary!\n" << endl;
  }
  StPrimaryTrack* prTrack=isPrimaryTrack(glTrack);
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
    StMcTrack* mcCandTrack = assocPair->partnerMcTrack();
  
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
    StGlobalTrack* testGlTrack = testPair[i]->partnerTrack();

    cout << "common hits=" << testPair[i]->commonTpcHits()
	 << ", rc key=" << testGlTrack->key() << endl;
  }
  
}
  
//
// $Log $
//


  









