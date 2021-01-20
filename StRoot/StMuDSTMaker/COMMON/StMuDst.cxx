/***************************************************************************
 *
 * $Id: StMuDst.cxx,v 1.70 2019/02/21 13:32:54 jdb Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 ***************************************************************************/

#include <map>

#include "StMuDst.h"

#include "StContainers.h"
#include "StEvent/StEventTypes.h"
#include "StEventUtilities/StGoodTrigger.h"
#include "StarClassLibrary/StTimer.hh"
#include "StMuDstMaker.h"
#include "StMuEvent.h"
#include "StMuPrimaryVertex.h"
#include "StMuRpsCollection.h"
#include "StMuMtdCollection.h"
#include "StMuTrack.h"
#include "StMuDebug.h"
#include "StMuEmcUtil.h"
#include "StMuFmsUtil.h"
#include "StMuPmdUtil.h"
#include "StMuMcVertex.h"
#include "StMuMcTrack.h"
#include "KFParticle/KFParticle.h"
#include "KFParticle/KFVertex.h"
///dongx
#include "StBTofCollection.h"
#include "StBTofRawHit.h"
#include "StBTofHeader.h"
#include "StBTofPidTraits.h"
#include "StMuBTofHit.h"
#include "StETofCollection.h"         // fseck
#include "StMuETofCollection.h"       // fseck
#include "StMuETofHeader.h"           // fseck
#include "StMuETofDigi.h"             // fseck
#include "StMuETofHit.h"              // fseck
#include "StMuEpdHitCollection.h"     // MALisa
#include "StMuEpdHit.h"              // MALisa
#include "StMuMtdHit.h"
#include "StMuMtdHeader.h"
#include "TClonesArray.h"
#include "TTree.h"
#ifndef __NO_STRANGE_MUDST__
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#endif
#include "StMuMcVertex.h"
#include "StMuMcTrack.h"
Int_t StMuDst::MinNoTpcMcHits = 15;
Int_t StMuDst::MinNoTpcRcHits = 15;
Double_t StMuDst::fgerMax = 0;    // 50 um
Double_t StMuDst::fgdca3Dmax = 0; // 50 cm
Double_t StMuDst::fgVxXmin = 0, StMuDst::fgVxXmax = 0, StMuDst::fgVxYmin = 0, StMuDst::fgVxYmax = 0;
Double_t StMuDst::fgVxZmin = 0, StMuDst::fgVxZmax = 0, StMuDst::fgVxRmax = 0;
PicoVtxMode StMuDst::mVtxMode = NotSet; // This should always be ::NotSet, do not change it, see ::Init()
Float_t   StMuDst::mTpcVpdVzDiffCut = 10;
//#define __HIST_PV__
#ifdef __HIST_PV__
static TH1F *hists[3] = {0};
static TH2F *pVrZ = 0;
static TH2F *pVxy = 0;
#endif /* __HIST_PV__ */
StMuDst *StMuDst::fgMuDst = 0;
ClassImp(StMuDst);
//________________________________________________________________________________

StMuDst::StMuDst() {
  DEBUGMESSAGE("");
  fgMuDst = this;
  unset();
  mCurrVertexId                 = -2;
  mCurrPrimaryTracks       = 0;
  SetMaxTrackDca(0);
  SetMaxVertexTransError(0);
  SetVxXYrange(0,0,0,0);
  SetVxZrange(0,0);
  SetVxRmax(0);
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::unset() {
    arrays        = 0;
#ifndef __NO_STRANGE_MUDST__
    strangeArrays = 0;
#endif
    mcArrays = 0;
    emcArrays     = 0;
    fmsArrays     = 0;
    pmdArrays     = 0;
    tofArrays     = 0;
    btofArrays    = 0;   // dongx
    mtdArrays     = 0;   // dongx
    etofArrays    = 0;   // jdb
    epdArrays     = 0;   // MALisa
    fgtArrays     = 0;
    mMuEmcCollectionArray = 0;
    mMuEmcCollection = 0; 
	mMuFmsCollection = 0;
    mMuPmdCollectionArray = 0;
    mMuPmdCollection = 0;
    mEmcCollection = 0;
	mFmsCollection = 0;
    eztArrays      = 0;
    mtdArrays = 0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::set(StMuDstMaker* maker) {
  DEBUGMESSAGE2("");
  if (!maker) { DEBUGVALUE(maker); return;}
  arrays        = maker->mArrays;
#ifndef __NO_STRANGE_MUDST__
  strangeArrays = maker->mStrangeArrays;
#endif
  mcArrays = maker->mMCArrays;
  emcArrays     = maker->mEmcArrays;
  fmsArrays     = maker->mFmsArrays;
  pmdArrays     = maker->mPmdArrays;
  tofArrays     = maker->mTofArrays;
  btofArrays    = maker->mBTofArrays;    // dongx
  etofArrays    = maker->mETofArrays;    // jdb
  epdArrays     = maker->mEpdArrays;    // MALisa
  mtdArrays     = maker->mMtdArrays;
  fgtArrays     = maker->mFgtArrays;


    mMuEmcCollectionArray = maker->mEmcCollectionArray;
  mMuEmcCollection      = maker->mEmcCollection;
  mMuFmsCollection      = maker->mFmsCollection;
   mMuPmdCollectionArray = maker->mPmdCollectionArray;
  mMuPmdCollection = maker->mPmdCollection;
  eztArrays     = maker->mEztArrays;

#ifndef __NO_STRANGE_MUDST__
  StStrangeEvMuDst* ev = strangeEvent();
  Int_t nV0s = v0s()->GetEntriesFast(); for (Int_t i=0;i<nV0s; i++) v0s(i)->SetEvent(ev); // set the pointer to the StStrangeEvMuDst which is not read from disk
  Int_t nXis = xis()->GetEntriesFast(); for (Int_t i=0;i<nXis; i++) xis(i)->SetEvent(ev); // set the pointer to the StStrangeEvMuDst which is not read from disk
  //  Int_t nKinks = kinks()->GetEntriesFast(); for (Int_t i=0;i<nKinks; i++) kinks(i)->SetEvent(ev);
#endif
  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::ResetMaps() {
  mMcVx2McTkRMap.clear();
  mMcVx2McParentTkMap.clear();
  mId2McTkMap.clear();
  mId2McVxMap.clear();
  mId2McVxRMap.clear();
  mId2RcVxMap.clear();
  mIndxRcTk2IdMap.clear();
  mIndxKFTk2IdMap.clear();
  mRcVx2RcTkMap.clear();
  mRcVx2McVxMap.clear();
  mMcVx2RcVxMap.clear();
  mRcVxsVec.clear();
  mRecoVxVec.clear();
  mCloneVxVec.clear();
  mGhostVxVec.clear();
  mLostVxVec.clear();
  mIdVx2KFVxMap.clear();
  mKFVx2RcVxMap.clear();
  mRcVx2KFVxMap.clear();
  mKFVx2McVxMap.clear();
  mMcVx2KFVxMap.clear();
  mIdMc2RcTkMap.clear();
  mIdGlobalId2IdPrimaryTrackMap.clear();
  mIdGlobal2IdPrimaryTrackMap.clear();
  mIdMc2IdRcTracksMap.clear();
  mIdMc2IdRcVerticesMap.clear();
  mMcTrack2GlobalTrackMap.clear();
  mMcTrack2PrimaryTrackMap.clear();
  mMcTrack2KFParticleMap.clear();
  mIdGlTk2IndxMap.clear();
  mIdPrTk2IndxMap.clear();
  mIdPrVx2IndxMap.clear();
  mIdKFTk2IndxMap.clear();
  mIdKFVx2IndxMap.clear();
}
//-----------------------------------------------------------------------
void StMuDst::set(TClonesArray** theArrays, 
#ifndef __NO_STRANGE_MUDST__
		  TClonesArray** theStrangeArrays, 
#endif
		  TClonesArray** theMCArrays, 
		  TClonesArray** theEmcArrays,
		  TClonesArray** theFmsArrays,
		  TClonesArray** thePmdArrays,
		  TClonesArray** theTofArrays,
		  TClonesArray** theBTofArrays,    // dongx
		  TClonesArray** theETofArrays,    // jdb
                  TClonesArray** theEpdArrays,     // MALisa
		  TClonesArray** theMTDArrays,
		  TClonesArray** theFgtArrays,
		  TClonesArray** theEztArrays,
                  TClonesArray* emc_arr,
		  StMuEmcCollection *emc,
 		  StMuFmsCollection *fms,		  
                  TClonesArray* pmd_arr,
		  StMuPmdCollection *pmd)
{
  // I don't understand why this method is still needed,
  // but cannot comile dictionary  when it is removed
  DEBUGMESSAGE2("");
  arrays        = theArrays;
#ifndef __NO_STRANGE_MUDST__
  strangeArrays = theStrangeArrays;
#endif
  mcArrays = theMCArrays;
  emcArrays     = theEmcArrays;   
  fmsArrays     = theFmsArrays;
  fgtArrays     = theFgtArrays;
  pmdArrays     = thePmdArrays;
  tofArrays     = theTofArrays;
  btofArrays    = theBTofArrays;    // dongx
  etofArrays    = theETofArrays;    // jdb
  epdArrays     = theEpdArrays;     // MALisa
  mMuEmcCollectionArray = emc_arr;  
  mMuEmcCollection = emc; 
  mMuFmsCollection = fms;  
  mMuPmdCollectionArray = pmd_arr;
  mMuPmdCollection = pmd;
  eztArrays     = theEztArrays;
    mtdArrays = theMTDArrays;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::collectVertexTracks() {
  if (mCurrPrimaryTracks == 0)
    mCurrPrimaryTracks = new TObjArray();
  Int_t n_track = arrays[muPrimary]->GetEntriesFast();
  mCurrPrimaryTracks->Clear();
  for (Int_t i_track = 0; i_track < n_track; i_track++) {
    if (((StMuTrack*)arrays[muPrimary]->UncheckedAt(i_track))->vertexIndex() == mCurrVertexId)
	mCurrPrimaryTracks->AddLast(arrays[muPrimary]->UncheckedAt(i_track));
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::setVertexIndex(Int_t vtx_id) {
  if (instance()->mCurrVertexId == vtx_id)  
     return;
  instance()->mCurrVertexId = vtx_id;
  instance()->collectVertexTracks();  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrays
  fixTrackIndices( instance()->arrays[muPrimary], instance()->arrays[muGlobal] );  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixTrackIndices(TClonesArray* primary, TClonesArray* global) {
  /// NOTE: this method does not work for productions with FTPC from SL04d
  ///       up to SL05g, because StFtpcTrackToStEvent generates duplicate 
  ///       track keys
  ///
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrays)

  if ( !(global&&primary) ) return;
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  static Int_t warningPrinted = 0;
  if (!warningPrinted) {
     LOG_WARN << "WARNING: You are using " << __PRETTY_FUNCTION__ 
              << " which does not work properly " 
                 " for productions with FTPC >= SL04d and <= SL05g" << endm;
     warningPrinted = 1;
  }
  Int_t nGlobals = global->GetEntriesFast();
  Int_t nPrimaries = primary->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<Short_t,UShort_t> globalIndex;

  for (Int_t i=0; i<nGlobals; i++) {
    StMuTrack *g = (StMuTrack*) global->UncheckedAt(i);
    if (g) {
      globalIndex[g->id()] = i+1;
      globalTracks(i)->setIndex2Global(i);
    }
  }
  // set the indices for the primary tracks
  DEBUGVALUE2(primary->GetEntriesFast());
  for (Int_t i=0; i<nPrimaries; i++) {
    StMuTrack *p = (StMuTrack*) primary->UncheckedAt(i);
    if (p) {
      if (globalIndex[p->id()]) 
        p->setIndex2Global( globalIndex[ p->id() ]-1 );
      else
        p->setIndex2Global(-1);
    }
  }
  DEBUGVALUE2(timer.elapsedTime());
}

void StMuDst::fixTrackIndicesG(Int_t mult) {
	/// Match global track index to primary track
	//mult = 0 means there is just a single vertex in the event, mult>0 means there are multiple...

	if (mult==0){
		if(!(fabs(event()->primaryVertexPosition().x()) < 1.e-5 && fabs(event()->primaryVertexPosition().y()) < 1.e-5 && fabs(event()->primaryVertexPosition().z()) < 1.e-5)){   
			Int_t startpos = 0;
			Int_t tid, pid;
			if(!globalTracks()) return;
			for (Int_t i=0;i<globalTracks()->GetEntriesFast();i++){
				tid = globalTracks(i)->id();
				globalTracks(i)->setIndex2Global(-2);
				if(!primaryTracks()) return;
				for(Int_t j=startpos;j<primaryTracks()->GetEntriesFast();j++){
					pid = primaryTracks(j)->id();
					if(pid==tid) {
						globalTracks(i)->setIndex2Global(j);
						if (j==startpos) startpos++;
						break;
					}
					else if (pid > tid) break; 
				}
			}
		}
		return;
	}
	//New MuDsts with multiple vertices....	
	if(!primaryVertices()) return;
	const Int_t Nvert = primaryVertices()->GetEntriesFast();
	if(!Nvert) return;
	Int_t curVer =  currentVertexIndex();
	if (curVer < 0) curVer = 0;
	Int_t startpos[Nvert];
	for(Int_t i=0;i<Nvert;i++) startpos[i]=0;	
	Int_t tid, pid;
	if(!globalTracks()) return;

	for (Int_t i=0;i<globalTracks()->GetEntriesFast();i++){
		tid = globalTracks(i)->id();
		globalTracks(i)->setIndex2Global(-2);
		globalTracks(i)->setVertexIndex(-2);			
		//Scan through vertices
		for(Int_t j=0;j<Nvert;j++){
			if(globalTracks(i)->index2Global() >= 0) break;
			setVertexIndex(j);
			if(!primaryTracks()) continue;
			for(Int_t k=startpos[j];k<primaryTracks()->GetEntriesFast();k++){
				pid = primaryTracks(k)->id();
				if(pid==tid) {
					globalTracks(i)->setIndex2Global(k);
					globalTracks(i)->setVertexIndex(j);
					if (k==startpos[j]) startpos[j]++;
					break;
				}
				else if (pid > tid) break; 
			}
		}
	}
	setVertexIndex(curVer);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixTofTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrays
    fixTofTrackIndices( btofArrays[muBTofHit], arrays[muPrimary], arrays[muGlobal] );  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixTofTrackIndices(TClonesArray* btofHit, TClonesArray* primary, TClonesArray* global) {

  if ( !(primary&&global&&btofHit) ) return;
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  Int_t nPrimarys = primary->GetEntriesFast();
  Int_t nGlobals = global->GetEntriesFast();
  Int_t nBTofHits = btofHit->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<Short_t,UShort_t> tofIndex;
  map<Short_t,UShort_t> globalIndex;
  map<Short_t,UShort_t> primaryIndex;

  for (Int_t i=0; i<nBTofHits; i++) {
    StMuBTofHit *t = (StMuBTofHit*) btofHit->UncheckedAt(i);
    if (t) {
      tofIndex[t->associatedTrackId()] = i+1;  // starting from 1
    }
  }

  for (Int_t i=0; i<nGlobals; i++) {
    StMuTrack *g = (StMuTrack*) global->UncheckedAt(i);
    if (g) {
      globalIndex[g->id()] = i+1;

      if(tofIndex[g->id()])
        g->setIndex2BTofHit( tofIndex[g->id()]-1 );
      else
        g->setIndex2BTofHit(-1);
    }
  }
  for (Int_t i=0; i<nPrimarys; i++) {
    StMuTrack *p = (StMuTrack*) primary->UncheckedAt(i);
    if (p) {
      primaryIndex[p->id()] = i+1;

      if(tofIndex[p->id()])
        p->setIndex2BTofHit( tofIndex[p->id()]-1 );
      else
        p->setIndex2BTofHit(-1);
    }
  }

  /// set the indices for BTofHits
  for (Int_t i=0; i<nBTofHits; i++) {
    StMuBTofHit *t = (StMuBTofHit*) btofHit->UncheckedAt(i);
    if (t) {
      if(globalIndex[t->associatedTrackId()])
        t->setIndex2Global( globalIndex[t->associatedTrackId()]-1 );
      else
        t->setIndex2Global(-1);

      if(primaryIndex[t->associatedTrackId()])
        t->setIndex2Primary( primaryIndex[t->associatedTrackId()]-1 );
      else
        t->setIndex2Primary(-1);
    }
  }

  DEBUGVALUE2(timer.elapsedTime());
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixETofTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrays
    fixETofTrackIndices( etofArrays[muETofHit], arrays[muPrimary], arrays[muGlobal] );  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixETofTrackIndices( TClonesArray* etofHit, TClonesArray* primary, TClonesArray* global ) {

  if( !( primary && global && etofHit ) ) return;
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  int nPrimaries = primary->GetEntriesFast();
  int nGlobals   = global->GetEntriesFast();
  int nETofHits  = etofHit->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<short,unsigned short> etofIndex;
  map<short,unsigned short> globalIndex;
  map<short,unsigned short> primaryIndex;

  for( int i=0; i<nETofHits; i++ ) {
    StMuETofHit* t = (StMuETofHit*) etofHit->UncheckedAt(i);
    if( t ) {
      etofIndex[t->associatedTrackId()] = i+1;  // starting from 1
    }
  }

  for( int i=0; i<nGlobals; i++ ) {
    StMuTrack* g = (StMuTrack*) global->UncheckedAt(i);
    if( g ) {
      globalIndex[g->id()] = i+1;

      if( etofIndex[g->id()] ) {
        g->setIndex2ETofHit( etofIndex[g->id()]-1 );
      }
      else {
        g->setIndex2ETofHit( -1 );
      }
    }
  }
  for( int i=0; i<nPrimaries; i++ ) {
    StMuTrack* p = (StMuTrack*) primary->UncheckedAt(i);
    if( p ) {
      primaryIndex[p->id()] = i+1;

      if( etofIndex[p->id()] ) {
        p->setIndex2ETofHit( etofIndex[p->id()]-1 );
      }
      else {
        p->setIndex2ETofHit( -1 );
      }
    }
  }

  /// set the indices for ETofHits
  for( int i=0; i<nETofHits; i++ ) {
    StMuETofHit* t = (StMuETofHit*) etofHit->UncheckedAt( i );
    if( t ) {
      if( globalIndex[t->associatedTrackId()] )
        t->setIndex2Global( globalIndex[t->associatedTrackId()]-1 );
      else
        t->setIndex2Global( -1 );

      if(primaryIndex[t->associatedTrackId()])
        t->setIndex2Primary( primaryIndex[t->associatedTrackId()]-1 );
      else
        t->setIndex2Primary( -1 );
    }
  }

  DEBUGVALUE2(timer.elapsedTime());
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixMtdTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrays
    fixMtdTrackIndices( mtdArrays[muMTDHit], arrays[muPrimary], arrays[muGlobal] );  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixMtdTrackIndices(TClonesArray* mtdHit, TClonesArray* primary, TClonesArray* global) {

  if ( !(primary&&global&&mtdHit) ) return;
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

 Int_t nPrimarys = primary->GetEntriesFast();
  Int_t nGlobals = global->GetEntriesFast();
  Int_t nMtdHits = mtdHit->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<Short_t,UShort_t> mtdIndex;
  map<Short_t,UShort_t> globalIndex;
  map<Short_t,UShort_t> primaryIndex;

  for (Int_t i=0; i<nMtdHits; i++) {
    StMuMtdHit *t = (StMuMtdHit*) mtdHit->UncheckedAt(i);
    if (t) {
      mtdIndex[t->associatedTrackKey()] = i+1;  // starting from 1
    }
  }

  for (Int_t i=0; i<nGlobals; i++) {
    StMuTrack *g = (StMuTrack*) global->UncheckedAt(i);
    if (g) {
      globalIndex[g->id()] = i+1;

      if(mtdIndex[g->id()])
        g->setIndex2MtdHit( mtdIndex[g->id()]-1 );
      else
        g->setIndex2MtdHit(-1);
    }
  }
  for (Int_t i=0; i<nPrimarys; i++) {
    StMuTrack *p = (StMuTrack*) primary->UncheckedAt(i);
    if (p) {
      primaryIndex[p->id()] = i+1;

      if(mtdIndex[p->id()])
        p->setIndex2MtdHit( mtdIndex[p->id()]-1 );
      else
        p->setIndex2MtdHit(-1);
    }
  }

  /// set the indices for MtdHits
  for (Int_t i=0; i<nMtdHits; i++) {
    StMuMtdHit *t = (StMuMtdHit*) mtdHit->UncheckedAt(i);
    if (t) {
      if(globalIndex[t->associatedTrackKey()])
        t->setIndex2Global( globalIndex[t->associatedTrackKey()]-1 );
      else
        t->setIndex2Global(-1);

      if(primaryIndex[t->associatedTrackKey()])
        t->setIndex2Primary( primaryIndex[t->associatedTrackKey()]-1 );
      else
        t->setIndex2Primary(-1);
    }
  }

  DEBUGVALUE2(timer.elapsedTime());
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::setMtdArray(StMtdCollection *mtd_coll) {
  /// reset MTD hit array and header when running StMtdHitMaker on muDst
  /// in afterburner mode

  mtdArrays[muMTDHit]->Clear();
  StMuMtdCollection mMTD(*mtd_coll);
  for(size_t i=0; i < (size_t)mMTD.hitsPresent(); i++) 
    {
      StMuMtdHit* mtdHit = (StMuMtdHit*)mMTD.MtdHit(i);
      new((*mtdArrays[muMTDHit])[i]) StMuMtdHit(*mtdHit);
    }

  StMuMtdHeader *mtdHead = mMTD.mtdHeader();
  if(mtdHead)
    {
      mtdArrays[muMTDHeader]->Clear();
      new((*mtdArrays[muMTDHeader])[0]) StMuMtdHeader(*mtdHead);
    }
}

//-----------------------------------------------------------------------
void StMuDst::setETofArray( const StETofCollection* etof_coll ) {
  /// reset ETOF digi/hit array and header when running eTOF related Makers
  /// on muDst in afterburner mode

  etofArrays[ muETofDigi ]->Clear();
  etofArrays[ muETofHit  ]->Clear();
  StMuETofCollection muETofColl( etof_coll );

  for( size_t i=0; i < (size_t) muETofColl.digisPresent(); i++ ) {
    StMuETofDigi* etofDigi = ( StMuETofDigi* ) muETofColl.etofDigi( i );
    new( ( *etofArrays[ muETofDigi ] )[ i ] ) StMuETofDigi( *etofDigi );
  }

  for( size_t i=0; i < (size_t) muETofColl.hitsPresent(); i++ ) {
    StMuETofHit* etofHit = ( StMuETofHit* ) muETofColl.etofHit( i );
    new( ( *etofArrays[ muETofHit ] )[ i ] ) StMuETofHit( *etofHit );
  }

  StMuETofHeader* etofHead = muETofColl.etofHeader();
  if( etofHead ) {
    etofArrays[ muETofHeader ]->Clear();
    new( ( *etofArrays[ muETofHeader ] )[ 0 ] ) StMuETofHeader( *etofHead );
  }
}

//-----------------------------------------------------------------------
void StMuDst::addETofHit( const StMuETofHit* hit ) {
  /// add etof hit to the hit array in muDst
  if( hit ) {
    unsigned int index = etofArrays[ muETofHit ]->GetEntriesFast();
    LOG_DEBUG << "hit will be added as index: " << index << endm;

    new( ( *etofArrays[ muETofHit ] )[ index ] ) StMuETofHit( *hit );
  }
  LOG_DEBUG << "done. -> new array size: " << etofArrays[ muETofHit ]->GetEntriesFast() << endm;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StEvent* StMuDst::createStEvent() {
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  StMuEvent* mu = event(); 
  if(!mu) return NULL;
  StEvent* ev = new StEvent();

  ev->setInfo( new StEventInfo(mu->eventInfo()) );
  ev->setRunInfo( new StRunInfo(mu->runInfo()) );
  ev->setSummary( new StEventSummary(mu->eventSummary()) );
  //   ev->setSoftwareMonitor(SoftwareMonitor*);
  //   ev->setTpcHitCollection(StTpcHitCollection*);
  //   ev->setFtpcHitCollection(StFtpcHitCollection*);
  //   ev->setSvtHitCollection(StSvtHitCollection*);
  //   ev->setSsdHitCollection(StSsdHitCollection*);
  //   ev->setEmcCollection(StEmcCollection*);
  //   ev->setRichCollection(StRichCollection*);
  //   ev->setTofCollection(StTofCollection*);
  //ev->setTofCollection( new StTofCollection() );
  StTriggerDetectorCollection *trg_coll=new StTriggerDetectorCollection();
  trg_coll->vpd()=mu->vpdTriggerDetector();
  trg_coll->bbc()=mu->bbcTriggerDetector();
  trg_coll->ctb()=mu->ctbTriggerDetector();
  trg_coll->emc()=mu->emcTriggerDetector();
  trg_coll->fpd()=mu->fpdTriggerDetector();
  trg_coll->zdc()=mu->zdcTriggerDetector();
  ev->setTriggerDetectorCollection(trg_coll);

  ev->setFpdCollection( new StFpdCollection(mu->fpdCollection()) );
  // ev->setTriggerDetectorCollection(muStTriggerDetectorCollection*); <<< WE DON'T WANT THAT
  ev->setL0Trigger ( new StL0Trigger(mu->l0Trigger()) );
  //   ev->setL1Trigger ( new StL0Trigger(mu->l0Trigger()) );
  ev->setL3Trigger ( new StL3Trigger() );
  
  StPrimaryVertex* vp  = new StPrimaryVertex();  
  ev->addPrimaryVertex(vp);
  vp->setPosition( mu->eventSummary().primaryVertexPosition() );

  Int_t nGlobals = arrays[muGlobal]->GetEntriesFast();

  StSPtrVecTrackNode &trackNodes = ev->trackNodes();
  TArrayI global_indices(nGlobals); // Temporary array to keep track of index numbers on trackNodes

  // add global tracks to tracknodes
  for (Int_t i=0; i<nGlobals; i++) {
    if(globalTracks(i)) {
      StTrackNode *node = new StTrackNode();
      node->addTrack(createStTrack(globalTracks(i)));
      trackNodes.push_back(node);
      global_indices[i]=trackNodes.size()-1;
    }
    else {
      global_indices[i]=-1;
    }
  }

  /// add primary tracks and primary vertex
  ///
  /// This only uses the deafult vertex and tracks in case 
  /// of multiple primary vertixes.

  TObjArray *prim_tracks=primaryTracks();

  Int_t nPrimaries = prim_tracks->GetEntriesFast();
  for (Int_t i=0; i<nPrimaries; i++) if(primaryTracks(i)) {
    StTrack* t = createStTrack((StMuTrack*)prim_tracks->At(i));
    Int_t global_idx=primaryTracks(i)->index2Global();
    if (global_idx >= 0 && global_indices[global_idx] >= 0) 
      trackNodes[global_indices[global_idx]]->addTrack( t );
    else {
      StTrackNode *node=new StTrackNode();
      node->addTrack(t);
      trackNodes.push_back(node);
    }
    vp->addDaughter( t );
  }

  /// do the same excercise for the l3 tracks
  /// we do this later
  /// we do this later
  /// we do this later
  
  // add detector states
  Int_t nStates = arrays[muState]->GetEntriesFast();
  for (Int_t i=0; i<nStates; i++) {
      StDetectorState* det = new StDetectorState(*detectorStates(i));
      ev->addDetectorState(det);
  }
  

  // now get the EMC stuff and put it in the StEvent
  static StMuEmcUtil* mEmcUtil = new StMuEmcUtil();
  StMuEmcCollection *emc = muEmcCollection();
  if(emc) { // transform to StEvent format and fill it
    StEmcCollection *EMC = mEmcUtil->getEmc(emc);
    if(EMC) ev->setEmcCollection(EMC);
  }
  // now get the FMS stuff and put it in the StEvent
  static StMuFmsUtil* mFmsUtil = new StMuFmsUtil();
  StMuFmsCollection *fms = muFmsCollection();
  if(fms) { // transform to StEvent format and fill it
     StFmsCollection *FMS = mFmsUtil->getFms(fms);
     if(FMS) ev->setFmsCollection(FMS);
  }
  // now get the PMD stuff and put it in the StEvent
  static StMuPmdUtil* mPmdUtil = new StMuPmdUtil();
  StMuPmdCollection *pmd = pmdCollection();
  if(pmd) { // transform to StEvent format and fill it
    StPhmdCollection *PMD = mPmdUtil->getPmd(pmd);
    if(PMD) ev->setPhmdCollection(PMD);
  }

// now get tof (after fix from Xin)
  StTofCollection *tofcoll = new StTofCollection();
  ev->setTofCollection(tofcoll);
  Int_t nTofData = tofArrays[muTofData]->GetEntriesFast();
  for(Int_t i=0;i<nTofData;i++) {
    StTofData *aData;
    if(tofData(i)) {
      UShort_t id = tofData(i)->dataIndex();
      UShort_t adc = tofData(i)->adc();
      UShort_t tdc = tofData(i)->tdc();
      Short_t tc = tofData(i)->tc();
      UShort_t sc = tofData(i)->sc();
      // run 5 - dongx
      aData = new StTofData(id, adc, tdc, tc, sc, 0, 0);
    } else {
      aData = new StTofData(0, 0, 0, 0, 0, 0, 0);
    }
    tofcoll->addData(aData);
  }
  // run 5 - dongx
  Int_t nTofRawData = tofArrays[muTofRawData]->GetEntriesFast();
  for(Int_t i=0;i<nTofRawData;i++) {
    StTofRawData *aRawData;
    if(tofRawData(i)) {
      UShort_t tray = tofRawData(i)->tray();
      UShort_t leteFlag = tofRawData(i)->leteFlag();
      UShort_t channel = tofRawData(i)->channel();
      UInt_t tdc = tofRawData(i)->tdc();
      UInt_t triggertime = tofRawData(i)->triggertime();
      UShort_t quality = tofRawData(i)->quality();
      aRawData = new StTofRawData(leteFlag,tray,channel,tdc,triggertime,quality);
    } else {
      aRawData = new StTofRawData(0, 0, 0, 0, 0, 0);
    }
    tofcoll->addRawData(aRawData);
  }

  // now create, fill the StBTofCollection - dongx
  StBTofCollection *btofcoll = new StBTofCollection();
  ev->setBTofCollection(btofcoll);
  Int_t nBTofRawHits = btofArrays[muBTofRawHit]->GetEntriesFast();
  for(Int_t i=0;i<nBTofRawHits;i++) {
    StBTofRawHit *aRawHit;
    if(btofRawHit(i)) {
      aRawHit = new StBTofRawHit(*(btofRawHit(i)));
    } else {
      aRawHit = new StBTofRawHit();
    }
    btofcoll->addRawHit(aRawHit);
  }
  if(btofHeader()) btofcoll->setHeader(new StBTofHeader(*(btofHeader())));
  // now create, fill and add new StTriggerIdCollection to the StEvent
  StTriggerIdCollection* triggerIdCollection = new StTriggerIdCollection();
  StTriggerId triggerId;
  triggerId = mu->triggerIdCollection().l1();
  if ( !StMuTriggerIdCollection::isEmpty( triggerId ) ) triggerIdCollection->setL1( new StTriggerId( triggerId ) );
  triggerId = mu->triggerIdCollection().l2();
  if ( !StMuTriggerIdCollection::isEmpty( triggerId ) ) triggerIdCollection->setL2( new StTriggerId( triggerId ) );
  triggerId = mu->triggerIdCollection().l3();
  if ( !StMuTriggerIdCollection::isEmpty( triggerId ) ) triggerIdCollection->setL3( new StTriggerId( triggerId ) );
  triggerId = mu->triggerIdCollection().nominal();
  if ( !StMuTriggerIdCollection::isEmpty( triggerId ) ) triggerIdCollection->setNominal( new StTriggerId( triggerId ) );
  ev->setTriggerIdCollection( triggerIdCollection );
    
  
  DEBUGVALUE2(timer.elapsedTime());
  return ev;
}

#include "StarClassLibrary/SystemOfUnits.h"
#include "StarClassLibrary/PhysicalConstants.h"
StTrackGeometry* StMuDst::trackGeometry(Int_t q, StPhysicalHelixD* h) {
  static StPhysicalHelixD nullHelix;
  StHelixModel* model=0; 
  if (nullHelix==*h) 			return 0;
  if (fabs(h->curvature()) > 100)	return 0;
  if (fabs(h->origin().x())>1000)	return 0;
  if (fabs(h->origin().y())>1000)	return 0;
  if (fabs(h->origin().z())>1000)	return 0;

  model = new StHelixModel(q, h->phase()+h->h()*pi/2, h->curvature(), h->dipAngle(), h->origin(), 
			     h->momentumAt(0,event()->runInfo().magneticField()*kilogauss), h->h());
  return model;
}

StTrack* StMuDst::createStTrack(const StMuTrack* track) {
  StTrack* t=0;
  StTrackGeometry *tg;
  if (track->bad()) return 0;

  if (track->type() == primary) t = new StPrimaryTrack();
  if (track->type() == global)  t = new StGlobalTrack();
  assert(t);
  t->setFlag( track->flag() );
  t->setFlagExtension( track->flagExtension() );
  t->setKey( track->id() );
  
  StPhysicalHelixD helix;
  helix = track->helix(); 
  tg = trackGeometry( track->charge(), &helix );
  if (tg) t->setGeometry( tg );
  helix = track->outerHelix();
  tg = trackGeometry( track->charge(), &helix );
  if (tg) t->setOuterGeometry( tg );

  t->setIdTruth(track->idTruth(), track->qaTruth());
  t->setIdParentVx( track->idParentVx() );
  t->setLength(track->length());
  t->setImpactParameter(track->dca().mag());
  t->addPidTraits(new StDedxPidTraits(kTpcId, kTruncatedMeanId, track->nHitsDedx(), track->dEdx(),0));
  Float_t a[2],b[15];
  a[0]=track->chi2();
  a[1]=0;
  memset(b,0,15*sizeof(Float_t));

  StTrackFitTraits traits(0,0,a,b);
  traits.setNumberOfFitPoints(track->nHitsFit(kTpcId),kTpcId);
  traits.setNumberOfFitPoints(track->nHitsFit(kFtpcEastId),kFtpcEastId);
  traits.setNumberOfFitPoints(track->nHitsFit(kFtpcWestId),kFtpcWestId);
  traits.setNumberOfFitPoints(track->nHitsFit(kSvtId),kSvtId);
  traits.setNumberOfFitPoints(track->nHitsFit(kSsdId),kSsdId);
  // Set flag for primary tracks, but not if data is old-style
  // Old style data has +1 for vertex built-in for Ftpc (not sure about Tpc)
  if (track->type() == primary && track->mNHitsFitTpc != 255)
    traits.setPrimaryVertexUsedInFit(kTRUE);
  t->setFitTraits(traits);

  t->setNumberOfPossiblePoints(track->nHitsPoss(kTpcId),kTpcId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kFtpcEastId),kFtpcEastId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kFtpcWestId),kFtpcWestId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kSvtId),kSvtId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kSsdId),kSsdId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kSstId), kSstId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kPxlId), kPxlId);
  t->setNumberOfPossiblePoints(track->nHitsPoss(kIstId), kIstId);

  // set the topology map
  t->setTopologyMap( track->topologyMap() );

  // set the btofPidTraits - dongx
  t->addPidTraits(track->btofPidTraits().createBTofPidTraits());

  // set the etofPidTraits - fseck
  t->addPidTraits(track->etofPidTraits().createETofPidTraits());

  return t;
}

void StMuDst::Print(Option_t *option) const {
  StMuEvent *event = 0;
  if ((event = StMuDst::event())) {
    cout << "++++++++++++++ MuDst run " << event->runId() << " event " << event->eventId() << " ++++++++++++++" << endl;
  }
  else 
    cout << "No event structure (StMuEvent) found!" << endl;
  if (StMuDst::primaryTracks()) {
    cout << "PrimaryVertices " << numberOfPrimaryVertices();
    cout << "\tPrimaryTracks " << numberOfPrimaryTracks();
  } else {
    cout << "Current PrimaryVertex is not set" << endl;
  }
  cout << "\tGlobalTracks "  << numberOfGlobalTracks();
  cout << "\tCovPrimTrack "  << covPrimTrack()->GetEntriesFast();
  cout << "\tCovGlobTrack "  << covGlobTrack()->GetEntriesFast();
  cout << "\tKFVertices "    << KFVertices()->GetEntriesFast();
  cout << "\tKFTracks "      << KFTracks()->GetEntriesFast();
  cout << "\t" << StMuArrays::mcArrayTypes[0] << " " << mcVertices()->GetEntriesFast();
  cout << "\t" << StMuArrays::mcArrayTypes[1] << " " << mcTracks()->GetEntriesFast();
  cout << endl;
  if (mCurrVertexId != 0)
    cout << "( note vtx_id " << mCurrVertexId << " ) " ; 

#ifndef __NO_STRANGE_MUDST__
  if (numberOfV0s() ||  numberOfXis() || numberOfKinks()) 
    cout << numberOfV0s() << " V0s, " << numberOfXis() << " Xis " 
	 << numberOfKinks() << " kinks" << endl;
#endif
  Int_t i = 0;
  if (muEmcCollection())    {cout << "\tEMC"; i++;}
  if (muFmsCollection())    {cout << "\tFMS"; i++;}
  if (pmdCollection())      {cout << "\tPMD"; i++;}
  if (numberOfTofHit())     {cout << "\tTOF"; i++;}
  if (i) cout <<  " data presen" << endl;
}

void StMuDst::printVertices()  {
  if (numberOfPrimaryVertices() == 0) {
    cout << "No vertices stored (for older data, check StMuEvent)" << endl;
    return;
  }
  cout << "+++++++++ vertex list ( " << numberOfPrimaryVertices() << " entries )" << endl << endl;
  for (UInt_t i_vtx = 0; i_vtx < numberOfPrimaryVertices(); i_vtx++) {
    primaryVertex(i_vtx)->Print();
  }
}

void StMuDst::printPrimaryTracks() {
  if (numberOfPrimaryTracks() == 0) {
    cout << "No primary tracks found!" << endl;
    return;
  }
  cout << "+++++++++ primary track list ( " << numberOfPrimaryTracks() << " entries )" << endl << endl;
  for (UInt_t i_trk = 0; i_trk < numberOfPrimaryTracks(); i_trk++) {
    primaryTracks(i_trk)->Print();
  }
}

void StMuDst::printAllPrimaryTracks() {
  UInt_t N = allPrimaryTracks()->GetEntriesFast();
  if (N == 0) {
    cout << "No primary tracks found!" << endl;
    return;
  }
  cout << "+++++++++ all primary track list ( " << N << " entries )" << endl << endl;
  for (UInt_t i_trk = 0; i_trk < N; i_trk++) {
    ((StMuTrack *)  allPrimaryTracks()->UncheckedAt(i_trk))->Print();
  }
}

void StMuDst::printGlobalTracks()  {
  if (numberOfGlobalTracks() == 0) {
    cout << "No global tracks found!" << endl;
    return;
  }
  cout << "+++++++++ global track list ( " << numberOfGlobalTracks() << " entries )" << endl << endl;
  for (UInt_t i_trk = 0; i_trk < numberOfGlobalTracks(); i_trk++) {
    globalTracks(i_trk)->Print();
  }
}
//________________________________________________________________________________
void StMuDst::printKFVertices() {
  Int_t N = numberOfKFVertices();
  cout << "+++++++++ KF Vertices list ( " << N << " entries )" << endl << endl;
  for (Int_t i = 0; i < N; i++) if (KFvertex(i)) cout << *((KFParticleBase*)KFvertex(i)) << endl;
}
//________________________________________________________________________________
void StMuDst::printKFTracks() {
  Int_t N = numberOfKFTracks();
  cout << "+++++++++ KF Tracks list ( " << N << " entries )" << endl << endl;
  for (Int_t i = 0; i < N; i++) if (KFtrack(i)) cout << *KFtrack(i) << endl;
}
//________________________________________________________________________________
void StMuDst::printMcVertices() {
  Int_t N = numberOfMcVertices();
  cout << "+++++++++ MC Vertices list ( " << N << " entries )" << endl << endl;
  //  for (Int_t i = 0; i < N; i++) if (MCvertex(i)) cout << *MCvertex(i) << endl;
  for (Int_t i = 1; i <= N; i++) PrintMcVx(i);
}
//________________________________________________________________________________
void StMuDst::printMcTracks() {
  Int_t N = numberOfMcTracks();
  cout << "+++++++++ MC Tracks list ( " << N << " entries )" << endl << endl;
  for (Int_t i = 0; i < N; i++) if (MCtrack(i)) cout << *MCtrack(i) << endl;
}
//________________________________________________________________________________
void StMuDst::PrintMcVx(UInt_t idVx) {
  if (! mcVertices()) return;
  if (idVx <= 0 || idVx > numberOfMcVertices()) return;
  StMuMcVertex *mcVertex = MCvertex(idVx-1);	
  if (! mcVertex) return;
  if (mcTracks()) {
    UInt_t iMcTk = mcVertex->IdParTrk();
    if (iMcTk > 0 && iMcTk <= numberOfMcTracks()) {
      StMuMcTrack *mcTrack = MCtrack(iMcTk-1);
      if (mcTrack) {
	cout << *mcVertex << "\t" << *mcTrack << "\t" << mcTrack->GeName() << endl;
	return;
      }
    }
    cout << *mcVertex << endl;
  }
}
//________________________________________________________________________________
// Build maps --------------------------------------------------------------------------------
//________________________________________________________________________________
Bool_t StMuDst::Accept(const StMuTrack *gTrack) {
  if (! gTrack)            return kFALSE;
#ifndef __RC__
  //  if (! gTrack->idTruth()) return kFALSE;
#endif /* ! __RC__ */
  if (! gTrack->charge())  return kFALSE;
  if (  gTrack->flag() < 100 ||  gTrack->flag()%100 == 11) return kFALSE; // bad fit or short track pointing to EEMC
  if (  gTrack->flag() > 1000) return kFALSE;  // pile up track in TPC
  if (  gTrack->nHitsFit() < MinNoTpcRcHits) return kFALSE;
  //  if (  gTrack->qaTruth() < 90) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t StMuDst::Accept(const StMuPrimaryVertex *RcVx) {
  if (! RcVx) return kFALSE;
  if (RcVx->noTracks() <= 0) return kFALSE;
  return kTRUE;
} 
//________________________________________________________________________________
Bool_t StMuDst::Accept(const StMuMcVertex *McVx) {
  if (! McVx) return kFALSE;
  Int_t n = McVx2McTkR().count((StMuMcVertex *)McVx);
  if (n < 2) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Bool_t StMuDst::Accept(const StMuMcTrack *McTrack) {
  if (! McTrack) return kFALSE;
  if (McTrack->No_tpc_hit() < MinNoTpcMcHits) return kFALSE;
  return kTRUE;
}
//________________________________________________________________________________
Int_t StMuDst::eventId() {return event()->eventId();}
//________________________________________________________________________________
multimap<StMuMcVertex *,StMuMcTrack *> &StMuDst::McVx2McTkR() {// Reconstructable Mc Tracks
  if (!mMcVx2McTkRMap.size()) {
    for (auto x : Id2McTk()) {
      Int_t Id = x.first;
      StMuMcTrack *McTrack = x.second;
      if (! Id || ! McTrack) {
	cout << "Illegal Mc Track Id or McTrack" << endl;
	continue;
      }
      Int_t IdVx = McTrack->IdVx();
      if (! IdVx) {
	cout << "Illegal IdVx:" << *McTrack << " rejected." << endl;
	continue;
      }
      Int_t n  = IdMc2RcTk().count(Id);
      if (! n) continue;
      if (! Accept(McTrack)) continue;
      StMuMcVertex *mcVx = Id2McVx()[IdVx];
      if (! mcVx) {
	cout << "Missing vertex of origin"; // PrP(*McTrack);
	continue;
      }
      mMcVx2McTkRMap.insert(pair<StMuMcVertex *,StMuMcTrack *>(mcVx,McTrack));
    }
  }
  return *&mMcVx2McTkRMap;
}
//________________________________________________________________________________
map<StMuMcVertex *,StMuMcTrack *>           &StMuDst::McVx2McParentTk() { 
  if (! mMcVx2McParentTkMap.size()) {
    for (auto x : Id2McVx()) {
      Int_t IdVx = x.first; 
      StMuMcVertex *McVx = x.second;
      if (! IdVx || ! McVx) {
	cout << "Illegal Idx or McVx ==> rejected." << endl;
	continue;
      }
      Int_t IdParTk = McVx->IdParTrk();
      if (! IdParTk) continue;
      StMuMcTrack *McTrack = Id2McTk()[IdParTk];
      //      PrPP2D(*McTrack,*McVx);
      mMcVx2McParentTkMap[McVx] = McTrack;
    }
  }
  return *&mMcVx2McParentTkMap;
}
//________________________________________________________________________________
map<Int_t,StMuMcTrack *>                    &StMuDst::Id2McTk() { // 
  if (! mId2McTkMap.size()) {
    //  Id => McTk
    for (UInt_t m = 0; m < numberOfMcTracks(); m++) {
      StMuMcTrack *McTrack = MCtrack(m);
      if (! McTrack) continue;
      
      Int_t Id = McTrack->Id();
      if (! Id) {
	cout << "Illegal Id:" << *McTrack << " rejected." << endl;
	continue;
      }
      mId2McTkMap[Id] = McTrack;
    }
  }
  return *&mId2McTkMap;
}
//________________________________________________________________________________
map<Int_t,StMuMcVertex *>                   &StMuDst::Id2McVx() { // All Mc Vx, StMuMcVertex *McVx = Id2McVx[Id]();
  if (! mId2McVxMap.size()) {
    // IdVx => McVx
    for (UInt_t m = 0; m < numberOfMcVertices();  m++) {
      StMuMcVertex *McVx = MCvertex(m);
      if (! McVx) continue;
      //      PrPPD(*McVx);
      Int_t Id = McVx->Id();
      if (! Id) {
	cout << "Illegal Id:" << *McVx << " rejected." << endl;
	continue;
      }
      mId2McVxMap[Id] = McVx;
    }
  }
  return *&mId2McVxMap;
}
//________________________________________________________________________________
map<Int_t,StMuMcVertex *>                   &StMuDst::Id2McVxR() {// Reconstructable, i.e. contains > 1 Reconstructable Mc Tracks
  if (! mId2McVxRMap.size()) {
    for (auto x : McVx2McTkR()) {
      if (! x.first) continue;
      Int_t n = McVx2McTkR().count(x.first);
      if (n < 2) continue;
      Int_t Id = x.first->Id();
      mId2McVxRMap[Id] = x.first; 
    }
  }
  return *&mId2McVxRMap;
}
//________________________________________________________________________________
map<Int_t,StMuPrimaryVertex*>               &StMuDst::Id2RcVx() {
  if (! mId2RcVxMap.size()) {
    for (UInt_t l = 0; l < numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *RcVx = primaryVertex(l);
      if (! Accept(RcVx)) continue;
      //      PrPPD(*RcVx);
      Int_t Id = RcVx->id();
      mId2RcVxMap[Id] = RcVx;
    }
  }
  return *&mId2RcVxMap;
}
//________________________________________________________________________________
map<Int_t,Int_t>                            &StMuDst::IndxRcTk2Id() {
  if (! mIndxRcTk2IdMap.size()) {
    for (UInt_t kg = 0; kg < numberOfGlobalTracks(); kg++) {
      StMuTrack *gTrack = globalTracks(kg);
      if (! gTrack) continue;
      mIndxRcTk2IdMap.insert(pair<Int_t,Int_t>(gTrack->id(),kg));
    }
  }
  return *&mIndxRcTk2IdMap;
}
//________________________________________________________________________________
map<Int_t,Int_t>                            &StMuDst::IndxKFTk2Id() {
  if (! mIndxKFTk2IdMap.size()) {
    for (UInt_t m = 0; m < numberOfKFTracks(); m++) {
      KFParticle *particle = KFtrack(m);
      if (! particle) continue;
      mIndxKFTk2IdMap.insert(pair<Int_t,Int_t>(particle->Id(),m));
#if 0
      PrPPD(*particle);
#endif
    }
  }
  return *&mIndxKFTk2IdMap;
}
//________________________________________________________________________________
multimap<StMuPrimaryVertex*, StMuTrack *>   &StMuDst::RcVx2RcTk() {
  if (! mRcVx2RcTkMap.size()) {
    for (UInt_t k = 0; k < numberOfPrimaryTracks(); k++) {
      StMuTrack *pTrack = (StMuTrack *) primaryTracks()->UncheckedAt(k);
      if (! Accept(pTrack)) continue;
      Int_t IdVx = pTrack->vertexIndex();
      StMuPrimaryVertex *RcVx = Id2RcVx()[IdVx];
      mRcVx2RcTkMap.insert(make_pair(RcVx,pTrack));      
    }
  }
  return *&mRcVx2RcTkMap;
}
//________________________________________________________________________________
map<StMuPrimaryVertex*,StMuMcVertex *>      &StMuDst::RcVx2McVx() {
  if (! mRcVx2McVxMap.size()) {
    for (UInt_t l = 0; l < numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *RcVx = primaryVertex(l);
      if (! Accept(RcVx)) continue;
      //      PrPPD(*RcVx);
      Int_t IdMc = RcVx->idTruth();
      if (IdMc > 0 && IdMc <= (Int_t) numberOfMcTracks()) {
	StMuMcVertex *McVx = Id2McVx()[IdMc]; 
	if (McVx) {
	  //	PrPPD(*McVx);
	  mRcVx2McVxMap[RcVx] = McVx;
	}
      }
    }
  }
  return *&mRcVx2McVxMap;
}
//________________________________________________________________________________
multimap<StMuMcVertex *,StMuPrimaryVertex*> &StMuDst::McVx2RcVx() {
  if (! mMcVx2RcVxMap.size()) {
    for (UInt_t l = 0; l < numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *RcVx = primaryVertex(l);
      if (! Accept(RcVx)) continue;
      //      PrPPD(*RcVx);
      Int_t IdMc = RcVx->idTruth();
      StMuMcVertex *McVx = Id2McVx()[IdMc]; 
      if (McVx) {
	//	PrPPD(*McVx);
	mMcVx2RcVxMap.insert(make_pair(McVx,RcVx));
      }
    }
  }
  return *&mMcVx2RcVxMap;
}
//________________________________________________________________________________
vector<StMuPrimaryVertex *>                 &StMuDst::RcVxs() {  // All accepted RcVx
  if (! mRcVxsVec.size()) {
    for (UInt_t l = 0; l < numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *RcVx = primaryVertex(l);
      if (! Accept(RcVx)) continue;
      //      PrPPD(*RcVx);
      mRcVxsVec.push_back(RcVx); 
    }
  }
  return *&mRcVxsVec;
}
//________________________________________________________________________________
vector<StMuPrimaryVertex *>                 &StMuDst::RecoVx() {  //  1 to 1 Mc to Rc match
  if (! mRecoVxVec.size()) {
    for (auto x: Id2McVxR()) {
      if (! x.first) continue;
      StMuMcVertex *McVx = x.second;
      if (! McVx) continue;
      pair<
      multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator,
	multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator> ret = McVx2RcVx().equal_range(McVx);
      Int_t ncount = 0;
      StMuPrimaryVertex *RcVx = 0;
      for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	StMuPrimaryVertex *aRcVx = it->second;
	if (! aRcVx) continue;
	ncount++;
	if (! RcVx) {RcVx = aRcVx;}
	else if (RcVx->ranking() < aRcVx->ranking()) {RcVx = aRcVx;}
      }
      if (ncount != 0) {
	for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	  StMuPrimaryVertex *aRcVx = it->second;
	  if (RcVx == aRcVx)  mRecoVxVec.push_back(aRcVx);
	}
      }
    }
  }
  return *&mRecoVxVec;
}
//________________________________________________________________________________
vector<StMuPrimaryVertex *>                 &StMuDst::CloneVx() { //  1 to many (>1) Mc to Rc match
  if (! mCloneVxVec.size()) {
    for (auto x: Id2McVxR()) {
      if (! x.first) continue;
      StMuMcVertex *McVx = x.second;
      if (! McVx) continue;
      pair<
      multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator,
	multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator> ret = McVx2RcVx().equal_range(McVx);
      Int_t ncount = 0;
      StMuPrimaryVertex *RcVx = 0;
      for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	StMuPrimaryVertex *aRcVx = it->second;
	if (! aRcVx) continue;
	ncount++;
	if (! RcVx) {RcVx = aRcVx;}
	else if (RcVx->ranking() < aRcVx->ranking()) {RcVx = aRcVx;}
      }
      if (ncount != 0) {
	for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	  StMuPrimaryVertex *aRcVx = it->second;
	  if (RcVx != aRcVx)  mCloneVxVec.push_back(aRcVx);
	}
      }
    }
  }
  return *&mCloneVxVec;
}
//________________________________________________________________________________
vector<StMuPrimaryVertex *>                 &StMuDst::GhostVx() { //  no Mc match
  if (! mGhostVxVec.size()) {
    for (UInt_t l = 0; l < numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *RcVx = primaryVertex(l);
      if (! Accept(RcVx)) continue;
      //      PrPPD(*RcVx);
      Int_t IdMc = RcVx->idTruth();
      if (! IdMc && IdMc <= (Int_t) numberOfMcVertices()) {
	mGhostVxVec.push_back(RcVx);
      }
    }
  }
  return *&mGhostVxVec;
}
//________________________________________________________________________________
vector<StMuMcVertex *>                      &StMuDst::LostVx() {  //  no Rc match
  if (! mLostVxVec.size()) {
    for (auto x: Id2McVxR()) {
      if (! x.first) continue;
      StMuMcVertex *McVx = x.second;
      if (! McVx) continue;
      pair<
      multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator,
	multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator> ret = McVx2RcVx().equal_range(McVx);
      Int_t ncount = 0;
      StMuPrimaryVertex *RcVx = 0;
      for (multimap<StMuMcVertex *,StMuPrimaryVertex*>::iterator it=ret.first; it != ret.second; ++it) {
	StMuPrimaryVertex *aRcVx = it->second;
	if (! aRcVx) continue;
	ncount++;
	if (! RcVx) {RcVx = aRcVx;}
	else if (RcVx->ranking() < aRcVx->ranking()) {RcVx = aRcVx;}
      }
      if (ncount == 0) {
	mLostVxVec.push_back(McVx); 
      }
    }
  }
  return *&mLostVxVec;
}
//________________________________________________________________________________
map<Int_t,KFParticle*>                      &StMuDst::IdVx2KFVx() { // 
  if (! mIdVx2KFVxMap.size()) {
    for (UInt_t m = 0; m < numberOfKFVertices(); m++) {
      KFParticle *KFVx = (KFParticle *) KFvertex(m);
      if (! KFVx) continue;
      //      PrPPD(*KFVx);
#if 0
      if (doPrint && Debug() > 1) {
	cout << "NDaughters = " << KFVx->NDaughters() << endl;
	for (Int_t i = 0; i < KFVx->NDaughters(); i++) {
	  cout << "\t" << KFVx->DaughterIds()[i];
	  if (i%10 == 9) cout << endl;
	}
	cout << endl;
      }
#endif
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      mIdVx2KFVxMap[IdVx] = KFVx;
    }
  }
  return *&mIdVx2KFVxMap;
}
//________________________________________________________________________________
map<KFParticle*,StMuPrimaryVertex*>         &StMuDst::KFVx2RcVx() {
  if (! mKFVx2RcVxMap.size()) {
    for (UInt_t m = 0; m < numberOfKFTracks(); m++) {
      KFParticle *KFVx = KFtrack(m);
      if (! KFVx) continue;
      if (! KFVx->NDaughters()) continue;
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      StMuPrimaryVertex* RcVx = Id2RcVx()[IdVx];
      if (RcVx) {
	mKFVx2RcVxMap[KFVx] = RcVx;
      }
    }
  }
  return *&mKFVx2RcVxMap;
}
//________________________________________________________________________________
multimap<StMuPrimaryVertex*,KFParticle*>    &StMuDst::RcVx2KFVx() {
  if (! mRcVx2KFVxMap.size()) {
    for (UInt_t m = 0; m < numberOfKFVertices(); m++) {
      KFParticle *KFVx = (KFParticle *) KFvertex(m);
      if (! KFVx) continue;
      //      PrPPD(*KFVx);
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      StMuPrimaryVertex* RcVx = Id2RcVx()[IdVx];
      if (RcVx) {
	//	PrPPD(*RcVx);
	mRcVx2KFVxMap.insert(pair<StMuPrimaryVertex*,KFParticle*>(RcVx,KFVx));
      }
    }
  }
  return *&mRcVx2KFVxMap;
}
//________________________________________________________________________________
map<KFParticle*,StMuMcVertex *>             &StMuDst::KFVx2McVx() { 
  if (! mKFVx2McVxMap.size()) {
    for (UInt_t m = 0; m < numberOfKFVertices(); m++) {
      KFParticle *KFVx = (KFParticle *) KFvertex(m);
      if (! KFVx) continue;
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      StMuMcVertex *mcVx = Id2McVx()[IdVx];
      if (mcVx) {
	mKFVx2McVxMap[KFVx] = mcVx;
      }
    }
  }
  return *&mKFVx2McVxMap;
}
//________________________________________________________________________________
multimap<StMuMcVertex*,KFParticle*>         &StMuDst::McVx2KFVx() { 
  if (! mMcVx2KFVxMap.size()) {
    for (UInt_t m = 0; m < numberOfKFVertices(); m++) {
      KFParticle *KFVx = (KFParticle *) KFvertex(m);
      if (! KFVx) continue;
      Int_t IdVx = KFVx->Id(); // Rc Vertex Id
      StMuPrimaryVertex* RcVx = Id2RcVx()[IdVx];
      StMuMcVertex *McVx = RcVx2McVx()[RcVx];
      if (McVx) {
	//	PrPPD(*McVx);
	mMcVx2KFVxMap.insert(pair<StMuMcVertex *,KFParticle *>(McVx,KFVx));
      }
    }
  }
  return *&mMcVx2KFVxMap;
}
//________________________________________________________________________________
multimap<Int_t,StMuTrack *>                 &StMuDst::IdMc2RcTk() { // Reconstucted Track to IdTruth
  if (! mIdMc2RcTkMap.size()) {
    // Id (Truth, IdMcTk) => gTrack
    for (UInt_t kg = 0; kg < numberOfGlobalTracks(); kg++) {
      StMuTrack *gTrack = (StMuTrack *) globalTracks()->UncheckedAt(kg);
      if (! Accept(gTrack)) continue;
      Int_t IdTruth = gTrack->idTruth();
      if (! IdTruth && IdTruth <= (Int_t) numberOfMcTracks()) continue;
      mIdMc2RcTkMap.insert(pair<Int_t,StMuTrack *>(IdTruth,gTrack));
    }
  }
  return *&mIdMc2RcTkMap;
}
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdGlobalId2IdPrimaryTrack() {
  if (! mIdGlobalId2IdPrimaryTrackMap.size()) {
    for (UInt_t k = 0; k < numberOfPrimaryTracks(); k++) {
      StMuTrack *pTrack = (StMuTrack *) array(muPrimary)->UncheckedAt(k);
      if (! Accept(pTrack)) continue;
      Int_t l = pTrack->vertexIndex();
      if (l < 0) continue;
      StMuPrimaryVertex *Vtx = primaryVertex(l);
      if (! Vtx) continue; // ??????
      if (Vtx->idTruth() != 1) continue;
      Int_t kg = pTrack->index2Global();
      mIdGlobalId2IdPrimaryTrackMap.insert(pair<Int_t,Int_t>(kg,k));
    }
  }
  return *&mIdGlobalId2IdPrimaryTrackMap;
}
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdGlobal2IdPrimaryTrack() {
  if (! mIdGlobal2IdPrimaryTrackMap.size()) {
    for (UInt_t k = 0; k < numberOfPrimaryTracks(); k++) {
      StMuTrack *pTrack = (StMuTrack *) array(muPrimary)->UncheckedAt(k);
      if (! Accept(pTrack)) continue;
      Int_t l = pTrack->vertexIndex();
      if (l < 0) continue;
      StMuPrimaryVertex *Vtx = primaryVertex(l);
      if (! Vtx) continue; // ??????
      if (Vtx->idTruth() != 1) continue;
      Int_t kg = pTrack->index2Global();
      mIdGlobal2IdPrimaryTrackMap.insert(pair<Int_t,Int_t>(kg+1,k+1));
    }
  }
  return *&mIdGlobal2IdPrimaryTrackMap;
}
//________________________________________________________________________________
multimap<Int_t,Int_t> &StMuDst::IdMc2IdRcTracks() {
  if (! mIdMc2IdRcTracksMap.size()) {
    for (UInt_t kg = 0; kg < numberOfGlobalTracks(); kg++) {
      StMuTrack *gTrack = globalTracks(kg);
      if (! Accept(gTrack)) continue;
      //      gTrack->Print();
      // Check Mc
      if (gTrack->idTruth() < 0 || gTrack->idTruth() > (Int_t) numberOfMcTracks()) {
	cout << "Illegal idTruth " << gTrack->idTruth() << " The track is ignored" << endl;
	continue;
      }
      StMuMcTrack *mcTrack = MCtrack(gTrack->idTruth()-1);
      if (mcTrack->Id() != gTrack->idTruth()) {
	cout << "Mismatched idTruth " << gTrack->idTruth() << " and mcTrack Id " <<  mcTrack->Id() 
	     << " The track is ignored" <<  endl;
      }
      Int_t idMcVx = mcTrack->IdVx();
      while (idMcVx != 1) {
	StMuMcVertex *mcVertex = MCvertex(idMcVx-1);
	Int_t idMcTrack = mcVertex->IdParTrk();
	if (! idMcTrack) break;
	StMuMcTrack *mcTrackP = MCtrack(idMcTrack-1);
	idMcVx = mcTrackP->IdVx();
	if (! idMcVx) break;
      }
      if (idMcVx != 1) continue;
      //      if (Debug()) mcTrack->Print();
      mIdMc2IdRcTracksMap.insert(pair<Int_t,Int_t>(gTrack->idTruth()-1,kg)); // Id shifted by 1
    }
  }
  return *&mIdMc2IdRcTracksMap;
}
//________________________________________________________________________________
multimap<Int_t,Int_t> &StMuDst::IdMc2IdRcVertices() {
  if (! mIdMc2IdRcVerticesMap.size()) {
    for (UInt_t l = 0; l < numberOfPrimaryVertices(); l++) {
      StMuPrimaryVertex *Vtx = primaryVertex(l);
      if (! Accept(Vtx)) continue;
      //      Vtx->Print();
      // Check Mc
      if (Vtx->idTruth() < 0 || Vtx->idTruth() > (Int_t) numberOfMcVertices()) {
	cout << "Illegal idTruth " << Vtx->idTruth() << " The track is ignored" << endl;
	continue;
      }
      StMuMcVertex *mcVertex = MCvertex(Vtx->idTruth()-1);
      if (mcVertex->Id() != Vtx->idTruth()) {
	cout << "Mismatched idTruth " << Vtx->idTruth() << " and mcVertex Id " <<  mcVertex->Id() 
	     << " The vertex is ignored" <<  endl;
      }
#if 0
      if (Debug()) {
	cout << Form("%4i",l) << *Vtx;
	PrintMcVx(Vtx->idTruth(),MuMcVertices,MuMcTracks);
      }
#endif
      mIdMc2IdRcVerticesMap.insert(pair<Int_t,Int_t>(Vtx->idTruth()-1,l)); // Id shifted by 1
    }
  }
  return *&mIdMc2IdRcVerticesMap;
}
//________________________________________________________________________________
multimap<StMuMcTrack*,StMuTrack *>                 &StMuDst::McTrack2GlobalTrack() { // McTrack to Reconstucted global Track
  if (! mMcTrack2GlobalTrackMap.size()) {
    // Id (Truth, IdMcTk) => gTrack
    for (UInt_t kg = 0; kg < numberOfGlobalTracks(); kg++) {
      StMuTrack *gTrack = (StMuTrack *) globalTracks()->UncheckedAt(kg);
      if (! Accept(gTrack)) continue;
      Int_t IdTruth = gTrack->idTruth();
      if (! IdTruth || IdTruth > (Int_t) numberOfMcTracks()) continue;
      StMuMcTrack *mcTrack = MCtrack(IdTruth-1);
      if (mcTrack) mMcTrack2GlobalTrackMap.insert(pair<StMuMcTrack*,StMuTrack *>(mcTrack,gTrack));
    }
  }
  return *&mMcTrack2GlobalTrackMap;
}
//________________________________________________________________________________
multimap<StMuMcTrack*,StMuTrack *>                 &StMuDst::McTrack2PrimaryTrack() { // McTrack to Reconstucted primary Track
  if (! mMcTrack2PrimaryTrackMap.size()) {
    // Id (Truth, IdMcTk) => pTrack
    Int_t n_ptracks = arrays[muPrimary]->GetEntriesFast();
    for (Int_t k = 0; k < n_ptracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) arrays[muPrimary]->UncheckedAt(k);
      if (! Accept(pTrack)) continue;
      Int_t IdTruth = pTrack->idTruth();
      if (! IdTruth || IdTruth > (Int_t) numberOfMcTracks()) continue;
      StMuMcTrack *mcTrack = MCtrack(IdTruth-1);
      if (mcTrack) mMcTrack2PrimaryTrackMap.insert(pair<StMuMcTrack*,StMuTrack *>(mcTrack,pTrack));
    }
  }
  return *&mMcTrack2PrimaryTrackMap;
}
//________________________________________________________________________________
multimap<StMuMcTrack*,KFParticle *>                 &StMuDst::McTrack2KFParticle() { // McTrack to Reconstucted KFParticle
  if (! mMcTrack2KFParticleMap.size()) {
    for (UInt_t k = 0; k < numberOfKFTracks(); k++) {
      KFParticle *pTrack = KFtrack(k);
      if (! pTrack) continue;
      Int_t IdTruth = pTrack->IdTruth();
      if (! IdTruth || IdTruth > (Int_t) numberOfMcTracks()) continue;
      StMuMcTrack *mcTrack = MCtrack(IdTruth-1);
      if (mcTrack) mMcTrack2KFParticleMap.insert(pair<StMuMcTrack*,KFParticle *>(mcTrack,pTrack));
    }
  }
  return *&mMcTrack2KFParticleMap;
}
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdGlTk2Indx() {
  if (! mIdGlTk2IndxMap.size()) {
    Int_t N = numberOfGlobalTracks();
    for (Int_t kg = 0; kg < N; kg++) {
      StMuTrack *gTrack = (StMuTrack *) globalTracks()->UncheckedAt(kg);
      if (! gTrack) continue;
      mIdGlTk2IndxMap[gTrack->id()] = kg+1; // ! shift by 1 in order to pick up missing entries
    }
  }
  return *&mIdGlTk2IndxMap;
}
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdPrTk2Indx() {
  if (! mIdPrTk2IndxMap.size()) {
    Int_t n_ptracks = arrays[muPrimary]->GetEntriesFast();
    for (Int_t k = 0; k < n_ptracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) arrays[muPrimary]->UncheckedAt(k);
      if (! pTrack) continue;
      mIdPrTk2IndxMap[pTrack->id()] = k+1; // ! shift by 1 in order to pick up missing entries
    }
  }
  return *&mIdPrTk2IndxMap;
}
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdPrVx2Indx() {
  if (! mIdPrVx2IndxMap.size()) {
    Int_t N = primaryVertices()->GetEntriesFast();
      for (Int_t l = 0; l < N; l++) {
	const StMuPrimaryVertex* pV =  primaryVertex(l);
	if (! pV) continue;
	mIdPrVx2IndxMap[pV->id()] = l+1; // ! shift by 1 in order to pick up missing entries
      }
  }
  return *&mIdPrVx2IndxMap;
}
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdKFTk2Indx() {
  if (! mIdKFTk2IndxMap.size()) {
    Int_t N = numberOfKFTracks();
    for (Int_t k = 0; k < N; k++) {
      KFParticle*                 kftrack = KFtrack(k);
      if (! kftrack) continue;
      mIdKFTk2IndxMap[kftrack->Id()] = k+1; // ! shift by 1 in order to pick up missing entries
    }
  }
  return *&mIdKFTk2IndxMap;
}  
//________________________________________________________________________________
map<Int_t,Int_t> &StMuDst::IdKFVx2Indx() {
  if (! mIdGlTk2IndxMap.size()) {
    Int_t N = numberOfKFVertices();
    for (Int_t k = 0; k < N; k++) {
      const KFVertex* kfv = KFvertex(k);
      if (! kfv) continue;
      mIdGlTk2IndxMap[kfv->Id()] = k+1; // ! shift by 1 in order to pick up missing entries
    }
  }
  return *&mIdGlTk2IndxMap;
}
//________________________________________________________________________________
Bool_t StMuDst::IsGoodTrigger() const {
  if ( StGoodTrigger::instance()) {
    return StGoodTrigger::instance()->IsGood(&(event()->triggerIdCollection().nominal()));
  } 
  return kTRUE;
}
//________________________________________________________________________________
/**
 * Selects a primary vertex from `muDst` vertex collection according to the
 * vertex selection mode `mVtxMode` specified by the user. The mode must be
 * set with StMaker::SetAttr("PicoVtxMode", "your_desired_vtx_mode") as by
 * default the selection mode is `PicoVtxMode::NotSet`.
 *
 * Returns `true` if the user has specified a valid vertex selection mode and
 * a valid vertex satisfying the corresponding predefined conditions is found in
 * the muDst vertex collection.
 *
 * Returns `false` otherwise.
 */
Bool_t StMuDst::selectVertex() {
  if (! numberOfPrimaryVertices()) return kFALSE;
  Bool_t selectedVertex = kFALSE;
  //  if (! IsGoodTrigger()) return kFALSE;
  for (UInt_t iVtx = 0; iVtx < numberOfPrimaryVertices(); ++iVtx)       {
    StMuPrimaryVertex* vtx = primaryVertex(iVtx);
    if (!vtx) continue;
    StThreeVectorD E(vtx->posError());
    if (E.perp() > 999.0) continue;
    StThreeVectorD V(vtx->position());
#ifdef __HIST_PV__
    if (pVrZ) {
      pVrZ->Fill(V.z(),V.perp());
      pVxy->Fill(V.y(),V.x());
    }
#endif /* __HIST_PV__ */
    if (! numberOfMcVertices()) { // No cutss for MC event
      /* Cuts:
	 1.  -0.3 < X < 0.1 и -0.27 < Y < -0.13. Maksym
	 2.  sqrt(sigma_X**2 + sigma_Y**2) < 0.0050 cm
	 3.  const Char_t *triggersC = "520001, 520011, 520021, 520031, 520041, 520051"
	 4.  dca3D < 50 cm 
      */
      if (fgVxXmin < fgVxXmax && ! (fgVxXmin < V.x() && V.x() < fgVxXmax)) {continue;}
      if (fgVxYmin < fgVxYmax && ! (fgVxYmin < V.y() && V.y() < fgVxYmax)) {continue;}
      if (fgVxZmin < fgVxZmax && ! (fgVxZmin < V.z() && V.z() < fgVxZmax)) {continue;}
      if (fgVxRmax > 0 &&  V.perp() > fgVxRmax)                            {continue;}
      StThreeVectorD E(primaryVertex()->posError());
      const Double_t er = E.perp();
      if (fgerMax > 0 && er > fgerMax) {continue;}
    }
    // We save primary tracks associated with the selected primary vertex only
    // don't use StMuTrack::primary(), it returns primary tracks associated with
    // all vertices
    if (mVtxMode == PicoVtxMode::Default)  {
      // choose the default vertex, i.e. the first vertex
      setVertexIndex(iVtx);
      selectedVertex = kTRUE;
    } else if (mVtxMode == PicoVtxMode::Vpd || mVtxMode == PicoVtxMode::VpdOrDefault)  {
      StBTofHeader const* mBTofHeader = btofHeader();
      Float_t vzVpd = -999;
      if (mBTofHeader && TMath::Abs(mBTofHeader->vpdVz()) < 200) vzVpd = mBTofHeader->vpdVz();
      if (mVtxMode == PicoVtxMode::Vpd && vzVpd < 200) continue;
      if (vzVpd >= -200 && mTpcVpdVzDiffCut > 0 && TMath::Abs(vzVpd - vtx->position().z()) >= mTpcVpdVzDiffCut) continue;
      setVertexIndex(iVtx);
      selectedVertex = kTRUE;
      break;
    } else {// default case
      LOG_ERROR << "Pico Vtx Mode not set!" << endm;
    }
  }
  // Retrun false if selected vertex is not valid
  return selectedVertex;
}
//________________________________________________________________________________
void StMuDst::SetMaxTrackDca(Double_t cut) {
  fgdca3Dmax = cut;
  LOG_INFO << "StMuDst::SetMaxTrackDca = " << fgdca3Dmax << endm;
}
//________________________________________________________________________________
void StMuDst::SetMaxVertexTransError(Double_t cut) {
  fgerMax = cut;
  LOG_INFO << "StMuDst::SetMaxVertexTransError = " << fgerMax << endm;
}
//________________________________________________________________________________
void StMuDst::SetVxXYrange(Double_t xmin, Double_t xmax, Double_t ymin, Double_t ymax) {
  fgVxXmin = xmin;
  fgVxXmax = xmax;
  fgVxYmin = ymin;
  fgVxYmax = ymax;
  LOG_INFO << "StMuDst::SetVxXYrange for PV: x in [" 
	   << fgVxXmin << "," << fgVxXmax <<"], y in [" 
	   << fgVxYmin << "," << fgVxYmax << "]" << endm;
}
//________________________________________________________________________________
void StMuDst::SetVxZrange(Double_t zmin, Double_t zmax) {
  fgVxZmin = zmin;
  fgVxZmax = zmax;
  LOG_INFO << "StMuDst::SetVxZrange for PV: z in [" 
	   << fgVxZmin << "," << fgVxZmax <<"]" << endm;
}
//________________________________________________________________________________
void StMuDst::SetVxRmax(Double_t rmax) {
  fgVxRmax = rmax;
  LOG_INFO << "StMuDst::SetVxRmax for PV: rho < " << fgVxRmax << endm;
}
//________________________________________________________________________________
// Get the index number of the current primary vertex 
Int_t StMuDst::currentVertexIndex() {return instance()->mCurrVertexId; }
// returns pointer to the n-th TClonesArray 
TClonesArray* StMuDst::array(Int_t type) { return instance()->arrays[type]; }
#ifndef __NO_STRANGE_MUDST__
  // returns pointer to the n-th TClonesArray from the strangeness arrays
TClonesArray* StMuDst::strangeArray(Int_t type) { return instance()->strangeArrays[type]; }
#endif
TClonesArray* StMuDst::mcArray(Int_t type) { return instance()->mcArrays[type]; }
TClonesArray* StMuDst::mcVertices()      { return instance()->mcArray(0);}
TClonesArray* StMuDst::mcTracks()        { return instance()->mcArray(1);}
  // returns pointer to the n-th TClonesArray from the emc arrays
TClonesArray* StMuDst::emcArray(Int_t type) { return instance()->emcArrays[type]; }
   // returns pointer to the n-th TClonesArray from the fms arrays
TClonesArray* StMuDst::fmsArray(Int_t type) { return instance()->fmsArrays[type]; }
    // returns pointer to the n-th TClonesArray from the pmd arrays
TClonesArray* StMuDst::pmdArray(Int_t type) { return instance()->pmdArrays[type]; }
  // returns pointer to the n-th TClonesArray from the tof arrays
TClonesArray* StMuDst::tofArray(Int_t type) { return instance()->tofArrays[type]; }
  // returns pointer to the n-th TClonesArray from the btof arrays // dongx
TClonesArray* StMuDst::btofArray(Int_t type) { return instance()->btofArrays[type]; }
  /// returns pointer to the n-th TClonesArray from the etof arrays // FS
TClonesArray* StMuDst::etofArray(int type) { return instance()->etofArrays[type]; }
  // returns pointer to the n-th TClonesArray from the mtd arrays
TClonesArray* StMuDst::mtdArray(Int_t type) { return instance()->mtdArrays[type]; }
  // returns pointer to the n-th TClonesArray from the fgt arrays
TClonesArray* StMuDst::fgtArray(Int_t type) { return instance()->fgtArrays[type]; }
  // returns pointer to the n-th TClonesArray from the ezt arrays
TClonesArray* StMuDst::eztArray(int type) { return instance()->eztArrays[type]; }
  // returns pointer to the EpdHitCollection
TClonesArray* StMuDst::epdHits() { return instance()->epdArrays[muEpdHit]; }  // MALisa
  // returns pointer to the primary vertex list
TClonesArray* StMuDst::primaryVertices() { return instance()->arrays[muPrimaryVertex]; }
TClonesArray* StMuDst::allPrimaryTracks() { return instance()->arrays[muPrimary]; } 
  // returns pointer to a list of tracks belonging to the selected primary vertex
TObjArray* StMuDst::primaryTracks() { return instance()->mCurrPrimaryTracks; } 
  // returns pointer to the global tracks list
TObjArray* StMuDst::globalTracks() { return instance()->arrays[muGlobal]; }
  // returns pointer to the other tracks list (all tracks that are not flagged as primary of global)
TClonesArray* StMuDst::otherTracks() { return instance()->arrays[muOther]; }
  // returns pointer to the l3Tracks list
TClonesArray* StMuDst::l3Tracks() { return instance()->arrays[muL3]; }
  // returns pointer to the list of rich spectra
TClonesArray* StMuDst::richSpectra() { return instance()->arrays[muRich]; }
  // returns pointer to the list of detector states
TClonesArray* StMuDst::detectorStates() { return instance()->arrays[muState]; }
  // returns pointer to list of accepted l3 algorithms 
TClonesArray* StMuDst::l3AlgoAccept() { return instance()->arrays[muAccept]; }
  // returns pointer to list rejected l3 algorithms 
TClonesArray* StMuDst::l3AlgoReject() { return instance()->arrays[muReject]; }
TClonesArray* StMuDst::covGlobTrack() {return instance()->arrays[muCovGlobTrack];}
TClonesArray* StMuDst::covPrimTrack() {return instance()->arrays[muCovPrimTrack];}
TClonesArray* StMuDst::KFTracks() {return instance()->arrays[muKFTracks];}
TClonesArray* StMuDst::KFVertices() {return instance()->arrays[muKFVertices];}

  // returns pointer to current StMuEvent (class holding the event wise information, e.g. event number, run number)
StMuEvent* StMuDst::event() { return (StMuEvent*)instance()->arrays[muEvent]->UncheckedAt(0); }
// return pointer to current primary vertex
StMuPrimaryVertex* StMuDst::primaryVertex() { return (StMuPrimaryVertex*)instance()->arrays[muPrimaryVertex]->UncheckedAt(instance()->mCurrVertexId); }
  // return pointer to i-th primary vertex
StMuPrimaryVertex* StMuDst::primaryVertex(Int_t i) { return (StMuPrimaryVertex*)instance()->arrays[muPrimaryVertex]->UncheckedAt(i); }
  // return pointer to i-th primary track 
StMuTrack* StMuDst::primaryTracks(Int_t i) { return (StMuTrack*)instance()->mCurrPrimaryTracks->UncheckedAt(i); }
  // return pointer to i-th global track 
StMuTrack* StMuDst::globalTracks(Int_t i) { return (StMuTrack*)instance()->arrays[muGlobal]->UncheckedAt(i); }
  // return pointer to i-th other track  (track that is not flagged as primary of global)
StMuTrack* StMuDst::otherTracks(Int_t i) { return (StMuTrack*)instance()->arrays[muOther]->UncheckedAt(i); }
  // return pointer to i-th l3 track
StMuTrack* StMuDst::l3Tracks(Int_t i) { return (StMuTrack*)instance()->arrays[muL3]->UncheckedAt(i); }
  // returns pointer to i-th StRichSpectra
StRichSpectra* StMuDst::richSpectra(Int_t i) { return (StRichSpectra*)instance()->arrays[muRich]->UncheckedAt(i); }
  // returns pointer to i-th StDetectorState
StDetectorState* StMuDst::detectorStates(Int_t i) { return (StDetectorState*)instance()->arrays[muState]->UncheckedAt(i); }
  // returns pointer to i-th accepted StL3AlgorithmInfo
StL3AlgorithmInfo* StMuDst::l3AlgoAccept(Int_t i) { return (StL3AlgorithmInfo*)instance()->arrays[muAccept]->UncheckedAt(i); }
  // returns pointer to i-th rejected StL3AlgorithmInfo
StL3AlgorithmInfo* StMuDst::l3AlgoReject(Int_t i) { return (StL3AlgorithmInfo*)instance()->arrays[muReject]->UncheckedAt(i); }
  //returns pp2pp infomation
StMuRpsCollection* StMuDst::RpsCollection() { return (StMuRpsCollection*)instance()->arrays[mupp2pp]->UncheckedAt(0); }
StMuMtdCollection* StMuDst::MtdCollection() { return (StMuMtdCollection*)instance()->arrays[muMtd]->UncheckedAt(0); }

StDcaGeometry* StMuDst::covGlobTracks(Int_t i) { return (StDcaGeometry*)instance()->arrays[muCovGlobTrack]->UncheckedAt(i); }
StMuPrimaryTrackCovariance* StMuDst::covPrimTracks(Int_t i) { return (StMuPrimaryTrackCovariance*)instance()->arrays[muCovPrimTrack]->UncheckedAt(i); }
KFParticle* StMuDst::KFtrack(Int_t i)  { return (KFParticle*) KFTracks()->UncheckedAt(i); }
KFVertex* StMuDst::KFvertex(Int_t i) { return (KFVertex*)   KFVertices()->UncheckedAt(i); }
StMuMcTrack* StMuDst::MCtrack(Int_t i)  { return (StMuMcTrack*) mcTracks()->UncheckedAt(i); }
StMuMcVertex* StMuDst::MCvertex(Int_t i) { return (StMuMcVertex*)   mcVertices()->UncheckedAt(i); }
 
#ifndef __NO_STRANGE_MUDST__
  // returns pointer to current StStrangeEvMuDst (class holding the event wise information, e.g. event number, run number)
StStrangeEvMuDst* StMuDst::strangeEvent() { return (StStrangeEvMuDst*)instance()->strangeArrays[smuEv]->UncheckedAt(0); }
  // returns pointer to MC version of current StStrangeEvMuDst
StStrangeEvMuDst* StMuDst::strangeEventMc() { return (StStrangeEvMuDst*)instance()->strangeArrays[smuEvMc]->UncheckedAt(0); }
  // returns pointer to the v0 list
TClonesArray* StMuDst::v0s() { return instance()->strangeArrays[smuV0]; }
  // returns pointer to the mc v0 list
TClonesArray* StMuDst::v0sMc() { return instance()->strangeArrays[smuV0Mc]; }
  // returns pointer to the v0 association list
TClonesArray* StMuDst::v0Assoc() { return instance()->strangeArrays[smuV0Assoc]; }
  // returns pointer to the xi list
TClonesArray* StMuDst::xis() { return instance()->strangeArrays[smuXi]; }
  // returns pointer to the mc xi list
TClonesArray* StMuDst::xisMc() { return instance()->strangeArrays[smuXiMc]; }
  // returns pointer to the xi association list
TClonesArray* StMuDst::xiAssoc() { return instance()->strangeArrays[smuXiAssoc]; }
  // returns pointer to the kink list
TClonesArray* StMuDst::kinks() { return instance()->strangeArrays[smuKink]; }
  // returns pointer to the mc kink list
TClonesArray* StMuDst::kinksMc() { return instance()->strangeArrays[smuKinkMc]; }
  // returns pointer to the kink association list
TClonesArray* StMuDst::kinkAssoc() { return instance()->strangeArrays[smuKinkAssoc]; }
  // returns pointer to the list of strangeCuts
TClonesArray* StMuDst::strangeCuts() { return instance()->strangeArrays[smuCut]; }
  // returns pointer to the i-th v0
StV0MuDst* StMuDst::v0s(Int_t i) { return (StV0MuDst*)instance()->strangeArrays[smuV0]->UncheckedAt(i); }
StV0Mc* StMuDst::v0sMc(Int_t i) { return (StV0Mc*)instance()->strangeArrays[smuV0Mc]->UncheckedAt(i); }
StStrangeAssoc* StMuDst::v0Assoc(Int_t i) { return (StStrangeAssoc*)instance()->strangeArrays[smuV0Assoc]->UncheckedAt(i); }
  // returns pointer to the i-th xi
StXiMuDst* StMuDst::xis(Int_t i) { return (StXiMuDst*)(void*)instance()->strangeArrays[smuXi]->UncheckedAt(i); }
StXiMc* StMuDst::xisMc(Int_t i) { return (StXiMc*)instance()->strangeArrays[smuXiMc]->UncheckedAt(i); }
StStrangeAssoc* StMuDst::xiAssoc(Int_t i) { return (StStrangeAssoc*)instance()->strangeArrays[smuXiAssoc]->UncheckedAt(i); }
  // returns pointer to the i-th kink
StKinkMuDst* StMuDst::kinks(Int_t i) { return (StKinkMuDst*)instance()->strangeArrays[smuKink]->UncheckedAt(i); }
StKinkMc* StMuDst::kinksMc(Int_t i) { return (StKinkMc*)instance()->strangeArrays[smuKinkMc]->UncheckedAt(i); }
StStrangeAssoc* StMuDst::kinkAssoc(Int_t i) { return (StStrangeAssoc*)instance()->strangeArrays[smuKinkAssoc]->UncheckedAt(i); }
  // returns pointer to the i-th stranneCut (of type TCut)
TCut* StMuDst::strangeCuts(Int_t i) { return (TCut*)instance()->strangeArrays[smuCut]->UncheckedAt(i); }
#endif
  // returns pointer to current StMuEmcCollection
StMuEmcCollection* StMuDst::muEmcCollection() { if (instance()->mMuEmcCollectionArray) return (StMuEmcCollection*) instance()->mMuEmcCollectionArray->UncheckedAt(0); else return instance()->mMuEmcCollection; }
   // returns pointer to current StMuFmsCollection
StMuFmsCollection* StMuDst::muFmsCollection() { return instance()->mMuFmsCollection; }
  // returns pointer to current StMuPmdCollection
StMuPmdCollection* StMuDst::pmdCollection() { if (instance()->mMuPmdCollectionArray)  return (StMuPmdCollection*) instance()->mMuPmdCollectionArray->UncheckedAt(0); else return instance()->mMuPmdCollection; }
  // returns pointer to current StEmcCollection
StEmcCollection* StMuDst::emcCollection() {  return instance()->mEmcCollection; }
  // returns pointer to current StFmsCollection
StFmsCollection* StMuDst::fmsCollection() {  return instance()->mFmsCollection; }

  // returns pointer to the i-th muTofHit
StMuTofHit* StMuDst::tofHit(Int_t i) { return (StMuTofHit*)instance()->tofArrays[muTofHit]->UncheckedAt(i); }
  // returns pointer to the i-th tofData
StTofData* StMuDst::tofData(Int_t i) { return (StTofData*)instance()->tofArrays[muTofData]->UncheckedAt(i); }
  // run 5 - dongx
  // returns pointer to the i-th tofRawData
StTofRawData* StMuDst::tofRawData(Int_t i) { return (StTofRawData*)instance()->tofArrays[muTofRawData]->UncheckedAt(i); }
  // returns pointer to the i-th muBTofHit
StMuBTofHit* StMuDst::btofHit(Int_t i) { return (StMuBTofHit*)instance()->btofArrays[muBTofHit]->UncheckedAt(i); }
  // returns pointer to the i-th btofRawHit - dongx
StBTofRawHit* StMuDst::btofRawHit(Int_t i) { return (StBTofRawHit*)instance()->btofArrays[muBTofRawHit]->UncheckedAt(i); }
  // returns pointer to the btofHeader - dongx
StBTofHeader* StMuDst::btofHeader() { return (StBTofHeader*)instance()->btofArrays[muBTofHeader]->UncheckedAt(0); }

  // returns pointer to the i-th muETofHit
StMuETofHit* StMuDst::etofHit(Int_t i) { return (StMuETofHit*)instance()->etofArrays[muETofHit]->UncheckedAt(i); }
  // returns pointer to the i-th etofRawHit - dongx
StMuETofDigi* StMuDst::etofDigi(Int_t i) { return (StMuETofDigi*)instance()->etofArrays[muETofDigi]->UncheckedAt(i); }
  // returns pointer to the etofHeader - dongx
StMuETofHeader* StMuDst::etofHeader() { return (StMuETofHeader*)instance()->etofArrays[muETofHeader]->UncheckedAt(0); }

StMuEpdHit* StMuDst::epdHit(int i) { return (StMuEpdHit*)instance()->epdArrays[muEpdHit]->UncheckedAt(i); }  // MALisa

StMuMtdHit* StMuDst::mtdHit(Int_t  i) { return (StMuMtdHit*)instance()->mtdArrays[muMTDHit]->UncheckedAt(i); }
  StMuMtdRawHit* StMuDst::mtdRawHit(Int_t  i) { return (StMuMtdRawHit*)instance()->mtdArrays[muMTDRawHit]->UncheckedAt(i); }
  StMuMtdHeader* StMuDst::mtdHeader() { return (StMuMtdHeader*)instance()->mtdArrays[muMTDHeader]->UncheckedAt(0); } 
    
    
  // returns pointer to eztHeader 
 EztEventHeader* StMuDst::eztHeader() { return (EztEventHeader*)instance()->eztArrays[muEztHead]->UncheckedAt(0); }

//  StMuBTofHit* StMuDst::btofHit(Int_t i) { return (StMuBTofHit*)instance()->btofArrays[muBTofHit]->UncheckedAt(i); }

    
  // returns pointer to eztTrig 
 EztTrigBlob* StMuDst::eztTrig() 
        { return (EztTrigBlob*)instance()->eztArrays[muEztTrig]->UncheckedAt(0); }

  // returns pointer to eztFpd 
 EztFpdBlob* StMuDst::eztFpd() 
        { return (EztFpdBlob*)instance()->eztArrays[muEztFpd]->UncheckedAt(0); }

  // returns pointer to ETOW 
 EztEmcRawData* StMuDst::eztETow() 
        { return (EztEmcRawData*)instance()->eztArrays[muEztETow]->UncheckedAt(0); }
  // returns pointer to eztESmd +pre/post
 EztEmcRawData* StMuDst::eztESmd() 
        { return (EztEmcRawData*)instance()->eztArrays[muEztESmd]->UncheckedAt(0); }

UInt_t StMuDst::numberOfPrimaryVertices()  { return instance()->arrays[muPrimaryVertex]->GetEntriesFast(); }
UInt_t StMuDst::numberOfPrimaryTracks()  { return instance()->mCurrPrimaryTracks ? instance()->mCurrPrimaryTracks->GetEntriesFast() : 0; }
UInt_t StMuDst::numberOfGlobalTracks()   { return instance()->arrays[muGlobal]->GetEntriesFast(); }
UInt_t StMuDst::numberOfOtherTracks()    { return instance()->arrays[muOther]->GetEntriesFast(); }
UInt_t StMuDst::numberOfL3Tracks()       { return instance()->arrays[muL3]->GetEntriesFast(); }
UInt_t StMuDst::numberOfRichSpectras()   { return instance()->arrays[muRich]->GetEntriesFast(); }
UInt_t StMuDst::numberOfDetectorStates() { return instance()->arrays[muState]->GetEntriesFast(); }
UInt_t StMuDst::numberOfL3AlgoAccepts()  { return instance()->arrays[muAccept]->GetEntriesFast(); }
UInt_t StMuDst::numberOfL3AlgoRejects()  { return instance()->arrays[muReject]->GetEntriesFast(); }
UInt_t StMuDst::numberOfCovGlobTracks()  { return instance()->arrays[muCovGlobTrack]->GetEntriesFast(); }
UInt_t StMuDst::numberOfCovPrimTracks()  { return instance()->arrays[muCovPrimTrack]->GetEntriesFast(); }
UInt_t StMuDst::numberOfKFTracks()       { return instance()->arrays[muKFTracks]->GetEntriesFast(); }
UInt_t StMuDst::numberOfKFVertices()     { return instance()->arrays[muKFVertices]->GetEntriesFast(); }
UInt_t StMuDst::numberOfMcVertices()     { return instance()->mcVertices()->GetEntriesFast(); }
UInt_t StMuDst::numberOfMcTracks()       { return instance()->mcTracks()->GetEntriesFast(); }
#ifndef __NO_STRANGE_MUDST__
UInt_t StMuDst::numberOfV0s()            { return instance()->strangeArrays[smuV0]->GetEntriesFast(); }
UInt_t StMuDst::numberOfV0sMc()          { return instance()->strangeArrays[smuV0Mc]->GetEntriesFast(); }
UInt_t StMuDst::numberOfV0Assoc()        { return instance()->strangeArrays[smuV0Assoc]->GetEntriesFast(); }
UInt_t StMuDst::numberOfXis()            { return instance()->strangeArrays[smuXi]->GetEntriesFast(); }
UInt_t StMuDst::numberOfXisMc()          { return instance()->strangeArrays[smuXiMc]->GetEntriesFast(); }
UInt_t StMuDst::numberOfXiAssoc()        { return instance()->strangeArrays[smuXiAssoc]->GetEntriesFast(); }  
UInt_t StMuDst::numberOfKinks()          { return instance()->strangeArrays[smuKink]->GetEntriesFast(); }
UInt_t StMuDst::numberOfKinksMc()        { return instance()->strangeArrays[smuKinkMc]->GetEntriesFast(); } 
UInt_t StMuDst::numberOfKinkAssoc()      { return instance()->strangeArrays[smuKinkAssoc]->GetEntriesFast(); }
UInt_t StMuDst::numberOfStrangeCuts()    { return instance()->strangeArrays[smuCut]->GetEntriesFast(); }
#endif
  // tofr
UInt_t StMuDst::numberOfTofHit()        { return instance()->tofArrays[muTofHit]->GetEntriesFast(); }
UInt_t StMuDst::numberOfTofData()       { return instance()->tofArrays[muTofData]->GetEntriesFast(); }
  // run 5 - dongx
UInt_t StMuDst::numberOfTofRawData()    { return instance()->tofArrays[muTofRawData]->GetEntriesFast(); }
  // dongx
UInt_t StMuDst::numberOfBTofHit()       { return instance()->btofArrays[muBTofHit]->GetEntriesFast(); }
UInt_t StMuDst::numberOfBTofRawHit()    { return instance()->btofArrays[muBTofRawHit]->GetEntriesFast(); }

UInt_t StMuDst::numberOfETofDigi()     { return instance()->etofArrays[muETofDigi]->GetEntriesFast(); }
UInt_t StMuDst::numberOfETofHit()      { return instance()->etofArrays[muETofHit]->GetEntriesFast(); }
UInt_t StMuDst::numberOfEpdHit()       { return instance()->epdArrays[muEpdHit]->GetEntriesFast(); }

UInt_t StMuDst::numberOfMTDHit()       { return instance()->mtdArrays[muMTDHit]->GetEntriesFast(); }
UInt_t StMuDst::numberOfBMTDRawHit()    { return instance()->mtdArrays[muMTDRawHit]->GetEntriesFast(); }
    
UInt_t StMuDst::GetNPrimaryVertex()    { return instance()->numberOfPrimaryVertices(); }  
UInt_t StMuDst::GetNPrimaryTrack()    { return instance()->numberOfPrimaryTracks(); }  
UInt_t StMuDst::GetNGlobalTrack()     { return instance()->numberOfGlobalTracks(); }   
UInt_t StMuDst::GetNOtherTrack()      { return instance()->numberOfOtherTracks(); }    
UInt_t StMuDst::GetNL3Track()         { return instance()->numberOfL3Tracks(); }       
UInt_t StMuDst::GetNRichSpectra()     { return instance()->numberOfRichSpectras(); }   
UInt_t StMuDst::GetNDetectorState()   { return instance()->numberOfDetectorStates(); } 
UInt_t StMuDst::GetNL3AlgoAccept()    { return instance()->numberOfL3AlgoAccepts(); }  
UInt_t StMuDst::GetNL3AlgoReject()    { return instance()->numberOfL3AlgoRejects(); }  
#ifndef __NO_STRANGE_MUDST__
UInt_t StMuDst::GetNV0()              { return instance()->numberOfV0s(); }            
UInt_t StMuDst::GetNV0Mc()            { return instance()->numberOfV0sMc(); }            
UInt_t StMuDst::GetNV0Assoc()         { return instance()->numberOfV0Assoc(); }            
UInt_t StMuDst::GetNXi()              { return instance()->numberOfXis(); }            
UInt_t StMuDst::GetNXiMc()            { return instance()->numberOfXisMc(); }            
UInt_t StMuDst::GetNXiAssoc()         { return instance()->numberOfXiAssoc(); }            
UInt_t StMuDst::GetNKink()            { return instance()->numberOfKinks(); }
UInt_t StMuDst::GetNKinkMc()          { return instance()->numberOfKinksMc(); }            
UInt_t StMuDst::GetNKinkAssoc()       { return instance()->numberOfKinkAssoc(); }            
UInt_t StMuDst::GetNStrangeCut()      { return instance()->numberOfStrangeCuts(); }    
#endif
UInt_t StMuDst::GetNTofHit()          { return instance()->numberOfTofHit(); }
UInt_t StMuDst::GetNTofData()         { return instance()->numberOfTofData(); }
  // run 5 - dongx
UInt_t StMuDst::GetNTofRawData()      { return instance()->numberOfTofRawData(); }
  // dongx
UInt_t StMuDst::GetNBTofHit()         { return instance()->numberOfBTofHit(); }
UInt_t StMuDst::GetNBTofRawHit()      { return instance()->numberOfBTofRawHit(); }

UInt_t StMuDst::GetNETofDigi()        { return instance()->numberOfETofDigi(); }
UInt_t StMuDst::GetNETofHit()         { return instance()->numberOfETofHit(); }

UInt_t StMuDst::GetNEpdHit()         { return instance()->numberOfEpdHit(); }

UInt_t StMuDst::GetNMTDHit()         { return instance()->numberOfMTDHit(); }
UInt_t StMuDst::GetNMTDRawHit()      { return instance()->numberOfBMTDRawHit(); }
/***************************************************************************
 *
 * $Log: StMuDst.cxx,v $
 * Revision 1.70  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes ETofDigi, ETofHit, ETofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 * Revision 1.69  2018/02/27 04:11:57  jdb
 * Added epdArrays
 *
 * Revision 1.68  2017/01/19 23:03:19  smirnovd
 * Copy previously missing values in StMuTrack to StTrack conversion
 *
 * Revision 1.67  2017/01/19 23:03:13  smirnovd
 * Promise to not modify original StMuTrack when converting to StTrack
 *
 * Revision 1.66  2016/10/01 21:23:25  jdb
 * Changed default vertex index to -2. Recently changed it to -1 which caused unintended segfault since -1 is used as a special case value in other parts of the code.
 *
 * Revision 1.65  2016/09/30 01:06:40  jdb
 * initialize mCurrVertexId to -1 (was 0) so that when it is set the first time it will always cause the needed function call to collectVertexTracks().
 *
 * Revision 1.64  2014/06/25 01:26:39  jdb
 * Updated StMuDst::setMtdArray() and reset the MTD header. Needed for Run12 UU data where only the muMtdCollection is available.
 *
 * Revision 1.63  2014/05/16 15:06:45  jdb
 * chaned StMuDst{.h,.cxx} to add setMtdArray function
 *
 * Revision 1.62  2014/04/15 04:48:47  jdb
 * Changed mtdArrays[muBTofHit] to mtdArrays[muMTDHit] in StMuDst.cxx in function fixMtdTrackIndices
 *
 * Revision 1.61  2014/03/03 23:11:46  jdb
 * Added Rongrongs changes to StMuDst.cxx and StMuMtdHit.{cxx,h} for mtd tracks
 *
 * Revision 1.60  2013/12/04 19:56:32  jdb
 * Added StMuMtdPidTraits.{cxx, h} added Mtd items to StMuMtdHit.h, StMuDst.{cxx,h}, StMuDstMaker.cxx, StMuTrack.{cxx,h}
 *
 * Revision 1.59  2012/11/26 23:14:32  fisyak
 * Replace GetEntries() by GetEntriesFast(), fix print outs
 *
 * Revision 1.58  2012/11/15 22:26:13  sangalin
 * Added the FGT. Fixed bugs in array offsets for the MTD.
 *
 * Revision 1.57  2012/09/28 22:38:05  tone421
 * Changed array stucture of MTD upon request of the TOF group. MTD arrays now on top level, rather than within __NARRAYS__
 *
 * Revision 1.56  2011/10/17 00:19:13  fisyak
 * Active handing of IdTruth
 *
 * Revision 1.55  2011/05/04 19:51:32  tone421
 * Added MTD infomation
 *
 * Revision 1.54  2011/04/08 01:25:50  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.53  2010/05/26 04:25:50  tone421
 * Added StTriggerData arrays in muevent and fixed an issue with PMD arrays being read....
 *
 * Revision 1.52  2010/03/10 15:03:06  tone421
 * Added more pointer protection in StMuDst::fixTrackIndicesG()
 *
 * Revision 1.51  2010/03/09 23:59:49  tone421
 * Added null point protection in StMuDst::fixTrackIndicesG(int mult)
 *
 * Revision 1.50  2010/03/08 19:06:51  tone421
 * Two things. Global tracks how are filled with an index to primary at birth. Added StMuDst::fixTrackIndicesG(), which is used for matching the primary track indices to global tracks. Previously, this was quite slow -  see this post:
 *
 * http://www.star.bnl.gov/HyperNews-star/protected/get/starsoft/8092/1/1/1.html
 *
 * for more details.
 *
 * Revision 1.49  2010/02/01 23:15:27  fine
 * replace non-static method
 *
 * Revision 1.48  2010/01/25 03:57:39  tone421
 * Added FMS and Roman pot arrays
 *
 * Revision 1.47  2009/12/01 03:42:54  tone421
 * Fixed small bug in StMuDst::fixTrackIndices and StMuDst::fixTofTrackIndices(), added StMuTrack::primaryTrack() and ensured StMuTrack::vertexIndex() returns some sensible for globals.
 *
 * Revision 1.46  2009/03/05 04:39:25  tone421
 * Added safety check for btofcoll->setHeader(new StBTofHeader(*(btofHeader()))) on line 456
 *
 * Revision 1.45  2009/02/20 16:37:44  tone421
 * *** empty log message ***
 *
 * Revision 1.43  2008/06/26 15:48:04  tone421
 *
 * Get info from StEvent so vpd z vertex infomation is available in StMuEvent
 *
 * Revision 1.42  2007/09/18 02:29:57  mvl
 * Added basic printing functionality. For convenience and to assist data consistency checks
 *
 * Revision 1.41  2007/05/16 18:50:49  mvl
 * Cleanup of output. Replaced cout with LOG_INFO etc.
 *
 * Revision 1.40  2007/04/20 06:25:21  mvl
 * Removed Q-vectors (will implement utility class).
 * Added Vpd info.
 *
 * Revision 1.39  2006/02/07 03:26:08  mvl
 * Changed createStEvent (use by MuDst2StEventmaker) to only copy primary tracks
 * that belong to the first primary vertex. This prevents segvio for events with
 * multiple primary vertices.
 *
 * Revision 1.38  2005/12/13 03:12:13  mvl
 * Changes to StMuDst2StEventMaker (code in StMuDst) and StMuDstFilterMaker
 * to no longer rely on track keys for matching global and primary tracks.
 * This was needed because track keys are not guaranteed to be unique anymore.
 *
 * Revision 1.37  2005/08/19 19:46:05  mvl
 * Further updates for multiple vertices. The main changes are:
 * 1) StMudst::primaryTracks() now returns a list (TObjArray*) of tracks
 *    belonging to the 'current' primary vertex. The index number of the
 *    'current' vertex can be set using StMuDst::setCurrentVertex().
 *    This also affects StMuDst::primaryTracks(int i) and
 *    StMuDst::numberOfprimaryTracks().
 * 2) refMult is now stored for all vertices, in StMuPrimaryVertex. The
 *    obvious way to access these numbers is from the StMuprimaryVertex structures,
 *    but for ebakcward compatibility a function is provided in StMuEvent as well
 *    (this is the only function taht works for existing MuDst)
 *
 * As an aside, I've also changes the internals of StMuDst::createStEvent and
 * StMuDst::fixTrackIndices() to be able to deal with a larger range of index numbers for tracks as generated by Ittf.
 *
 * BIG FAT WARNING: StMudst2StEventMaker and StMuDstFilterMaker
 * do not fully support the multiple vertex functionality yet.
 *
 * Revision 1.36  2005/05/20 20:30:35  mvl
 * More fixed to StMuDst::fixTrackIndices(). The bug afafcets only few tracks
 * for events with many tracks, so most people will not be much affected by it.
 * After heavy filtering with StMuDstFilterMaker (many fewer tracks than tack-ids)
 * the bug became apparent.
 *
 * Revision 1.35  2005/05/18 20:56:57  mvl
 * Fixed inconsistency in fixTrackIndices (pointed out by Alex Suaide):
 * now using the TClonesArray that are passed to the function, instead of the
 * ones in StMuDst.
 *
 * Revision 1.34  2005/04/12 21:56:29  mvl
 * Changes by Xin Dong for year-5 TOF data format: extra TClonesArray and routines to fill it from StEvent (StTofRawData).
 *
 * Revision 1.33  2004/10/28 00:11:33  mvl
 * Added stuff to support ezTree mode of MuDstMaker.
 * This is a special mode for fast-online processing of fast-detector data.
 *
 * Revision 1.32  2004/10/22 23:44:07  mvl
 * Fixed StMuDst::fixTrackIndices()
 *
 * Revision 1.31  2004/10/21 02:56:35  mvl
 * Added pointer to StEmcColleciton for Emc clustering etc.
 * Also made some technical changes for backward compatibility mode with
 * StMuIOMaker (pointers to TClonesArray for StMuEmcCollection)
 *
 * Revision 1.30  2004/10/19 01:45:26  mvl
 * Changes to split Emc and Pmd collections. Minor change to track copying logic
 *
 * Revision 1.29  2004/08/14 00:48:41  mvl
 * Bug fix in createStTrack & mods for vertex flag in fitTraits
 *
 * Revision 1.28  2004/08/07 02:44:06  mvl
 * Added support for fitted and possible points in different detectors, for ITTF
 *
 * Revision 1.27  2004/04/20 18:41:20  perev
 * Change arrays to pointer to StMuDstMaker::arrays StMuDst.h
 *
 * Revision 1.26  2004/04/14 17:15:56  subhasis
 * Xin's TOF reinclusion
 *
 * Revision 1.25  2004/04/09 22:04:55  subhasis
 * after tof createevent fix by Xin
 *
 * Revision 1.24  2004/04/09 03:36:14  jeromel
 * Removed TOF support entirely for now as we need a working version ... Will
 * revisit later.
 *
 * Revision 1.23  2004/04/08 23:58:07  jeromel
 * Small protection on tofcoll
 *
 * Revision 1.22  2004/04/06 00:25:35  jeromel
 * Missing TofCollection addition
 *
 * Revision 1.21  2004/04/02 03:24:53  jeromel
 * Changes implements PMD and TOF.  TOF is clearly incomplete.
 *
 * Revision 1.20  2003/10/31 19:12:56  laue
 * added filling of track id to createStTrack() function
 *
 * Revision 1.19  2003/10/28 18:57:56  perev
 * BadData protection added
 *
 * Revision 1.18  2003/10/23 04:08:29  perev
 * use SetBranchStatus fixed
 *
 * Revision 1.17  2003/10/20 22:55:39  laue
 * added filling of the topology map in the createStTrack function
 *
 * Revision 1.16  2003/10/14 14:35:53  laue
 * Alex Suaide EMC updates
 *
 * Revision 1.15  2003/08/04 14:38:10  laue
 * Alex Suaide's updated for the EMC. Now EEMC is included.
 *
 * Revision 1.14  2003/04/15 18:48:34  laue
 * Minor changes to be able to filter MuDst.root files and an example
 * how to do this. The StMuDstFilterMaker is just an example, it has to be
 * customized (spoilers, chrome weels, etc.) by the user.
 *
 * Revision 1.13  2003/03/19 18:58:04  laue
 * StMuChainMaker: updates for moved file catalog
 * StTriggerIdCollection added to the createStEvent function in StMuDst.cxx
 *
 * Revision 1.12  2003/02/18 20:38:30  laue
 * updates from Alex Suaide for filling StTrack from StMuTrack
 *
 * Revision 1.11  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.10  2002/09/21 00:26:09  laue
 * Bug fix in createStEvent() function. Now you can delete the StEvent
 *
 * Revision 1.9  2002/08/27 19:05:56  laue
 * Minor updates to make the muDst from simulation work
 *
 * Revision 1.8  2002/08/20 19:55:48  laue
 * Doxygen comments added
 *
 * Revision 1.7  2002/06/18 19:21:00  laue
 * cout statement taken out
 *
 * Revision 1.6  2002/05/20 18:57:18  laue
 * update for Christof
 *
 * Revision 1.5  2002/03/27 00:50:11  laue
 * bux fix from earlier check in
 *
 * Revision 1.4  2002/03/26 19:33:14  laue
 * minor updates
 *
 * Revision 1.3  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.2  2002/03/14 04:12:55  laue
 * bug fix: StMuL3EventSummary.cxx
 * update: StMuDst.h StMuDst.cxx
 *
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
