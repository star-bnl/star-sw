/**
 * $Id: StMiniMcMaker.cxx,v 1.18 2004/07/27 19:34:34 jeromel Exp $
 * \file  StMiniMcMaker.cxx
 * \brief Code to fill the StMiniMcEvent classes from StEvent, StMcEvent and StAssociationMaker
 * 
 *
 * \author Bum Choi, Manuel Calderon de la Barca Sanchez
 * \date   March 2001
 * $Log: StMiniMcMaker.cxx,v $
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
 * Set the data members for the Svt and Ftpc hits for McTrack and for the Svt and Ftpc
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
 *
 * Revision 1.5  2002/06/07 02:22:00  calderon
 * Protection against empty vector in findFirstLastHit
 * $Log: StMiniMcMaker.cxx,v $
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
 * Set the data members for the Svt and Ftpc hits for McTrack and for the Svt and Ftpc
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
 * and $Id: StMiniMcMaker.cxx,v 1.18 2004/07/27 19:34:34 jeromel Exp $ plus header comments for the macros
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



#include "Stiostream.h"
#include <assert.h>
#include "StMiniMcEvent/StMiniMcEvent.h"
#include "StMiniMcEvent/StMiniMcPair.h"
#include "StMiniMcEvent/StContamPair.h"

#include "StMessMgr.h"
#include "PhysicalConstants.h"
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"
#include "StIOMaker/StIOMaker.h"
#include "StParticleDefinition.hh"
#include "StMatrixF.hh"
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


static int StMiniMcMakerErrorCount=0;

//helper funtion prototypes
void dominatrackInfo(const StTrack*, short&, short&, float&);

// increasing order
inline bool hitCmp(StTpcHit* p1, StTpcHit* p2){
  return (p1->position().perp()<p2->position().perp());
}


ClassImp(StMiniMcMaker)

//---------CONSTRUCTORS, ETC--------

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
  mRun(0),
  mRcHitMap(0),
  mRcTrackMap(0),
  mMcTrackMap(0),
  mRcVertexPos(0),
  mMcVertexPos(0),
  mTpcDedxAlgo(0),
  mPidAlgo(0),
  mGhost(kTRUE), 
  mMinPt(0),mMaxPt(99999),
  mNSplit(0),mNRc(0),mNGhost(0),mNContam(0),
  mNMatched(0),mNMatGlob(0)
    
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

  cout << "\treconstr.  = " << mNRc << endl
       << "\tmatched    = " << mNMatched << endl
       << "\tsplit      = " << mNSplit << endl
       << "\tcontam.    = " << mNContam << endl
       << "\tghost      = " << mNGhost << endl
       << "\tmat global = " << mNMatGlob << endl;

  if (Debug()) cout << "deleting mMiniMcEvent" << endl;
  delete mMiniMcEvent;
  mMiniMcEvent = 0;
  // for some reason, the tree doesn't like to get deleted here...
//   if (Debug()) cout << "deleting mMiniMcTree" << endl;
//   delete mMiniMcTree;
//   mMiniMcTree = 0;
  if (Debug()) cout << "deleting mMiniMcDST" << endl;
  delete mMiniMcDST;
  mMiniMcDST = 0;
  
  return StMaker::Finish();
}
/*
  
 */

Int_t
StMiniMcMaker::InitRun(int runID) {
  cout << "###StMiniMcMaker::InitRun()" << endl;

  cout << "\tpt cut : " << mMinPt << " , " << mMaxPt << endl;

  mIOMaker = (StIOInterFace*)GetMaker("IO");
  if (!mIOMaker) {
      cout << "No StIOMaker found, trying StTreeMaker" << endl;
      mIOMaker = (StIOInterFace*) GetMaker("outputStream");
  }
  assert(mIOMaker);
  Int_t stat=0;
  //
  // if it's a new file, then close the old one and open a new one
  //
  TString curFileName;
  if(mIOMaker){
      if( ! strrchr(mIOMaker->GetFile(),'/')){
	  curFileName = mIOMaker->GetFile();
      }
      else {
	  curFileName = strrchr(mIOMaker->GetFile(),'/')+1;
      }
  }
  if (Debug()) {
      cout << "Current File Name (StIO) " << curFileName << endl;
      cout << "Cached  File Name (MiniMcMk) " << mInFileName << endl;
  }
  if(mMiniMcEvent && mInFileName.Contains(curFileName)) return kStOK;

  if(Debug()) {
      cout << "\tNew file found : " << curFileName << endl
	   << "\tReplacing " << mInFileName << endl;
  }
  closeFile();
  int fileBeginIndex = mInFileName.Last('/');
  mInFileName.Remove(0,fileBeginIndex+1);
  if (Debug()) cout << "New InFileName = " << mInFileName << endl;
  
  
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
  if (!mTpcDedxAlgo)
      mTpcDedxAlgo = new StTpcDedxPidAlgorithm;
    
  //
  // create file, trees, etc.
  //
  stat = openFile();

  return stat + StMaker::InitRun(runID);

}   
    
Int_t
StMiniMcMaker::Init()
{
  //Moved everything important to InitRun(int)
  cout << "###StMiniMcMaker::Init()" << endl;

  cout << "\tpt cut : " << mMinPt << " , " << mMaxPt << endl;

  mIOMaker = (StIOInterFace*)GetMaker("IO");
  if (!mIOMaker) {
      cout << "No StIOMaker found, trying StTreeMaker from bfc." << endl;
      mIOMaker = (StIOInterFace*) GetMaker("outputStream");
  }
  assert(mIOMaker);
  TString curFileName;
  if(mIOMaker){
      if( ! strrchr(mIOMaker->GetFile(),'/')){
	  curFileName = mIOMaker->GetFile();
      }
      else {
	  curFileName = strrchr(mIOMaker->GetFile(),'/')+1;
      }
      if(!mInFileName.Contains(curFileName)){
	  cout << "StMiniMcMaker::Init \tNew file found : " << curFileName << endl;
	  mInFileName = curFileName;
	  int fileBeginIndex = mInFileName.Last('/');
	  mInFileName.Remove(0,fileBeginIndex+1);
	  cout << "Caching the File Name (path removed) = " << mInFileName << endl;
      }

  }
  else {
      cout << "Couldn't find IO Maker!!" << endl;
      return kStFatal;
  }
  
  return StMaker::Init();
}   

/*
  Make called every event
 */

Int_t
StMiniMcMaker::Make()
{
  if(Debug()) cout << "###StMiniMcMaker::Make()" << endl;
  
  
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

  if(Debug()){
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
  StMiniMcPair* miniMcPair      = new StMiniMcPair;
  StContamPair* contamPair      = new StContamPair;
  StTinyMcTrack* tinyMcTrack    = new StTinyMcTrack;


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
  vector<int> enteredGlobalTracks;
  const StPtrVecMcTrack& allmcTracks = mMcEvent->tracks();
  cout << "size of mcEvent->tracks() : " << allmcTracks.size() << endl;
  
  StMcTrackConstIterator allMcTrkIter = allmcTracks.begin();
  for ( ; allMcTrkIter != allmcTracks.end(); ++allMcTrkIter) {
      StMcTrack* mcGlobTrack = *allMcTrkIter;
      if(!acceptRaw(mcGlobTrack)) continue; // loose eta cut (4 units, so should include ftpc).
      if(accept(mcGlobTrack) || mcGlobTrack->ftpcHits().size()>=5) { // 10 tpc hits or 5 ftpc hits
	  // Ok, track is accepted, query the map for its reco tracks.
	  StTrackPairInfo* candTrackPair = findBestMatchedGlobal(mcGlobTrack);
	  if (candTrackPair) {
	      // ok, found a match! Enter into the array and store the glob id
	      // to only enter a glob track once
	      StGlobalTrack* glTrack = candTrackPair->partnerTrack();
	      if (find(enteredGlobalTracks.begin(),enteredGlobalTracks.end(),glTrack->key())!=enteredGlobalTracks.end()) continue; //if it's already matched, skip it.
	      fillTrackPairInfo(miniMcPair, mcGlobTrack,
				0, glTrack, 
				candTrackPair->commonTpcHits()+((candTrackPair->commonSvtHits())*100), mRcTrackMap->count(glTrack),
				mMcTrackMap->count(mcGlobTrack), 0,
				kTRUE);
	      mMiniMcEvent->addTrackPair(miniMcPair,MATGLOB);
	      
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
    StMcTrack* mcTrack = *mcTrkIter;

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

	  if(Debug()==2 && mcMergedPair.size()>1) {
	    cout << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%" << endl;
	    cout << "MERGED" << endl;
	  }

	  Bool_t foundBest = kFALSE;
	  Bool_t isBestContam = kFALSE; // the best 'best matched' mc track
	                                // is not a primary mc 
	  for(unsigned int i=0; i<mcMergedPair.size(); i++){
	    StMcTrack* mergedMcTrack = (mcMergedPair[i])->partnerMcTrack();
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
		fillTrackPairInfo(miniMcPair, mergedMcTrack, 
				  prTrack, glTrack, 
				  mergedCommonHits+((mcMergedPair[i]->commonSvtHits())*100), nAssocMc,
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
				  mergedCommonHits+((mcMergedPair[i]->commonSvtHits())*100), nAssocMc,nAssocGlVec[i], 
				  nAssocPrVec[i]);
		 mMiniMcEvent->addTrackPair(miniMcPair,MERGED);
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
      fillMcTrackInfo(tinyMcTrack,mcTrack,nAssocGl,nAssocPr);
      mMiniMcEvent->addMcTrack(tinyMcTrack);
    }
  } // mc track iter

  //nSplit(0), nContam(0), nGhost(0), nContamNew(0);
  
  //
  // need to loop over the rc primary tracks to get
  // the event centrality values, etc.
  // also look for 'split tracks','ghost tracks'...
  //
  Int_t nGoodTrackEta(0), nFtpcWUncorrected(0), nFtpcEUncorrected(0);

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
			    commonHits+(((*iterBestMatchPair)->commonSvtHits())*100), nAssocMc, nAssocGl, nAssocPr);
	  mMiniMcEvent->addTrackPair(miniMcPair,SPLIT);
	  
	  nSplit++; 
	  
	  //
	  // reality check.  
	  //	  
	  if(Debug()==2) checkSplit(mcTrack,glTrack,commonHits);
	}
	else{ // no, it's best matched to a non primary, contamination
	  
	  fillTrackPairInfo(contamPair,mcTrack,
			    prTrack,glTrack,commonHits+(((*iterBestMatchPair)->commonSvtHits())*100),
			    nAssocMc,nAssocGl,nAssocPr);
	  mMiniMcEvent->addTrackPair(contamPair,CONTAM);
	  
	  if(Debug()==2) checkContam(mcTrack,glTrack,commonHits);
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
		nMcFtpcENch, nMcFtpcWNch,nFtpcEUncorrected,nFtpcWUncorrected);
  
  delete miniMcPair;
  delete contamPair;
  delete tinyMcTrack;

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

/*
  Create the output root file and the TTree
 */

Int_t
StMiniMcMaker::openFile()
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
    delete mMiniMcDST; mMiniMcDST=0;
  }
  mMiniMcTree=0;  //It is deleted by TFile::Close
  cout << "\t...done\n";

  return kStOk;
}
  
/*
  
 */

void
StMiniMcMaker::fillEventInfo(Int_t nGoodTrackEta, Int_t nRcGlobal, Int_t nRcGoodGlobal20,
			     Int_t nMcGlobal, Int_t nMcGoodGlobal20,
			     Int_t nMcNch, Int_t nMcHminus, Int_t nMcFtpcENch, Int_t nMcFtpcWNch, Int_t nFtpcEUncorrected, 
			     Int_t nFtpcWUncorrected)
{
  mMiniMcEvent->setEventId((Int_t) mRcEvent->id());
  mMiniMcEvent->setRunId((Int_t) mRcEvent->runId());
  mMiniMcEvent->setOriginMult((Int_t)mRcEvent->primaryVertex(0)->numberOfDaughters());
  mMiniMcEvent->setCentralMult(nGoodTrackEta);

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

  mMiniMcEvent->setNFtpcWUncorrectedPrimaries(nFtpcWUncorrected);
  mMiniMcEvent->setNFtpcEUncorrectedPrimaries(nFtpcEUncorrected);

  mMiniMcEvent->setCentrality(getIndex((size_t) mMiniMcEvent->nUncorrectedPrimaries()));
  mMiniMcEvent->setMcMult(mMcEvent->numberOfPrimaryTracks());
  
  mMiniMcEvent->setVertexX(mRcVertexPos->x());
  mMiniMcEvent->setVertexY(mRcVertexPos->y());
  mMiniMcEvent->setVertexZ(mRcVertexPos->z());

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

  if(prTrack || glTrack) fillRcTrackInfo(miniMcPair,prTrack,glTrack,nAssocMc);
  
  // common association info
  miniMcPair->setNCommonHit(commonHits);
  miniMcPair->setIsBestContam(isBestContam);
  // 
  short aeonFlux(-999),aeonFluxHits(0);
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
     const StMcTrack*    mcTParent = mcTrack->parent();
     
     if (mcTParent){
	contamPair->setParentGeantId(mcTrack->parent()->geantId());
	contamPair->setPtMcParent(mcTrack->parent()->momentum().perp());
	contamPair->setEtaMcParent(mcTrack->parent()->pseudoRapidity());
	contamPair->setGeantProcess(mcTrack->startVertex()->geantProcess());

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
/*

 */
void
StMiniMcMaker::fillRcTrackInfo(StTinyRcTrack* tinyRcTrack,
			       const StTrack* prTrack,
			       const StTrack* glTrack,
			       Int_t nAssocMc)
{

  if (!glTrack) {
      cout << "Error StMiniMcMaker::fillRcTrackInfo, glTrack pointer is zero " << glTrack << endl;
      return;
  }
  const StThreeVectorF& glMom = glTrack->geometry()->momentum();
  
  const StPhysicalHelixD& glHelix = glTrack->geometry()->helix();

  StMatrixF gCM = glTrack->fitTraits().covariantMatrix();
  Float_t errorGl[5] = {gCM(1,1),gCM(2,2),gCM(3,3),gCM(4,4),gCM(5,5)};
  
  
  tinyRcTrack->setPtGl(glMom.perp());
  tinyRcTrack->setPzGl(glMom.z());
  tinyRcTrack->setEtaGl(glMom.pseudoRapidity());
  tinyRcTrack->setPhiGl(glMom.phi()); 
  tinyRcTrack->setCurvGl(glTrack->geometry()->curvature());
  tinyRcTrack->setTanLGl(tan(glTrack->geometry()->dipAngle()));
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
  tinyRcTrack->setDcaZGl(dcaz(glHelix,*mRcVertexPos,glTrack));
  
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
  tinyRcTrack->setFitSvt(glTrack->fitTraits().numberOfFitPoints(kSvtId));
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
      // with the introduction of the global track branch,
      // having the primary track pointer here is optional. 
      const StThreeVectorF& prMom = prTrack->geometry()->momentum();
      const StPhysicalHelixD& prHelix = prTrack->geometry()->helix();
      StMatrixF pCM = prTrack->fitTraits().covariantMatrix();
      Float_t errorPr[5] = {pCM(1,1),pCM(2,2),pCM(3,3),pCM(4,4),pCM(5,5)};
      
      tinyRcTrack->setPtPr(prMom.perp());
      tinyRcTrack->setPzPr(prMom.z()); 
      tinyRcTrack->setEtaPr(prMom.pseudoRapidity());
      tinyRcTrack->setPhiPr(prMom.phi());
      tinyRcTrack->setCurvPr(prTrack->geometry()->curvature());
      tinyRcTrack->setTanLPr(tan(prTrack->geometry()->dipAngle()));
      tinyRcTrack->setErrPr(errorPr);
      tinyRcTrack->setChi2Pr(prTrack->fitTraits().chi2());
      tinyRcTrack->setFlag(prTrack->flag());
      tinyRcTrack->setDcaPr(prTrack->impactParameter());
      tinyRcTrack->setDcaXYPr(computeXY(mRcVertexPos,prTrack));
      //tinyRcTrack->setDcaZPr(computeZDca(mRcVertexPos,prTrack));
      tinyRcTrack->setDcaZPr(dcaz(prHelix,*mRcVertexPos));
      tinyRcTrack->setFitPts(prTrack->fitTraits().numberOfFitPoints(kTpcId));
      tinyRcTrack->setFitSvt(prTrack->fitTraits().numberOfFitPoints(kSvtId));
      size_t ftpcFitPts = 0;
      if (tinyRcTrack->etaGl()>1.8)
	  ftpcFitPts = prTrack->fitTraits().numberOfFitPoints(kFtpcWestId);
      if (tinyRcTrack->etaGl()<-1.8)
	  ftpcFitPts = prTrack->fitTraits().numberOfFitPoints(kFtpcEastId);
	tinyRcTrack->setFitFtpc(ftpcFitPts);
	
  }
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
    
  tinyMcTrack->setKey(mcTrack->key());
  tinyMcTrack->setPtMc(mcMom.perp());
  tinyMcTrack->setPzMc(mcMom.z());
  tinyMcTrack->setEtaMc(mcMom.pseudoRapidity());
  tinyMcTrack->setPhiMc(mcMom.phi());
  tinyMcTrack->setNHitMc(mcTrack->tpcHits().size());
  tinyMcTrack->setNSvtHitMc(mcTrack->svtHits().size());
  tinyMcTrack->setNFtpcHitMc(mcTrack->ftpcHits().size());
  tinyMcTrack->setGeantId(mcTrack->geantId());
  short chargeMc = -9999;
  if (mcTrack->particleDefinition()) chargeMc = static_cast<short>(mcTrack->particleDefinition()->charge());
  tinyMcTrack->setChargeMc(chargeMc);

  tinyMcTrack->setNAssocGl(nAssocGl);
  tinyMcTrack->setNAssocPr(nAssocPr);

  float stopR=(mcTrack->stopVertex()) ? mcTrack->stopVertex()->position().perp() : 999;
  tinyMcTrack->setStopR(stopR);

  //  if(stopR<999) cout << ">>stop r=" << stopR << endl;

} 

/*
  given a mc track, returns a vector of matched associated pairs.
 */
StTrackPairInfo*
StMiniMcMaker::findBestMatchedGlobal(StMcTrack* mcTrack)
{
    pair<mcTrackMapIter,mcTrackMapIter> mcBounds 
      = mMcTrackMap->equal_range(mcTrack);
    StTrackPairInfo* candTrackPair = 0;  // used for finding the best matched track
    StGlobalTrack* candTrack = 0;
    mcTrackMapIter mcMapIter = mcBounds.first;
    for ( ; mcMapIter != mcBounds.second; ++mcMapIter){
	StTrackPairInfo* assocPair = (*mcMapIter).second;
	StGlobalTrack* globTrack = assocPair->partnerTrack();
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
	  fabs(mcTrack->momentum().pseudoRapidity())<=4.);	  
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

Bool_t 
StMiniMcMaker::acceptFTPC(StTrack* prTrack)
{
  if(!prTrack) return false;
  StTrack* glTrack=prTrack->node()->track(global);

  return (prTrack &&
          glTrack->geometry()->helix().distance(*mRcVertexPos)<3&&
          prTrack->geometry()->momentum().perp() < 3 &&
          glTrack->fitTraits().numberOfFitPoints(kTpcId) >=5 &&
          fabs(prTrack->geometry()->momentum().pseudoRapidity())>=2.8 &&
          fabs(prTrack->geometry()->momentum().pseudoRapidity())<3.8
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
  Good Global RC Tracks with fitpts > 20
 */
inline Bool_t
StMiniMcMaker::acceptGood20(StTrack* track)
{
  UInt_t nFitPoint = track->fitTraits().numberOfFitPoints(kTpcId);
  return (track && nFitPoint >= 20);
}

/*
  Good Global MC Tracks with fitpts > 20
 */
inline Bool_t
StMiniMcMaker::acceptGood20(StMcTrack* track)
{
  return (track && track->tpcHits().size() >= 20);
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

size_t StMiniMcMaker::getIndex(size_t mult) {

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
  
//
// $Log $
//


  









