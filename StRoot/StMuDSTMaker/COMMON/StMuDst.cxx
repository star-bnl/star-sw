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
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerData2003.h"
#include "StEvent/StTriggerData2004.h"
#include "StEvent/StTriggerData2005.h"
#include "StEvent/StTriggerData2007.h"
#include "StEvent/StTriggerData2008.h"
#include "StEvent/StTriggerData2009.h"

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
TClonesArray** StMuDst::arrays       = 0;
#ifndef __NO_STRANGE_MUDST__
TClonesArray** StMuDst::strangeArrays= 0;
#endif
#include "StMuMcVertex.h"
#include "StMuMcTrack.h"
TClonesArray** StMuDst::mcArrays             = 0;
TClonesArray** StMuDst::emcArrays            = 0;
TClonesArray** StMuDst::fmsArrays            = 0;
TClonesArray** StMuDst::pmdArrays            = 0;
TClonesArray** StMuDst::tofArrays            = 0;
TClonesArray** StMuDst::btofArrays           = 0;   /// dongx
TClonesArray** StMuDst::etofArrays           = 0;   /// jdb
TClonesArray** StMuDst::epdArrays            = 0;   /// MALisa
TClonesArray** StMuDst::mtdArrays            = 0;
TClonesArray** StMuDst::fgtArrays            = 0;
TClonesArray *StMuDst::mMuEmcCollectionArray = 0;
StMuEmcCollection *StMuDst::mMuEmcCollection = 0;
StMuFmsCollection *StMuDst::mMuFmsCollection = 0;
TClonesArray *StMuDst::mMuPmdCollectionArray = 0;
StMuPmdCollection *StMuDst::mMuPmdCollection = 0;
StEmcCollection *StMuDst::mEmcCollection     = 0;
StFmsCollection *StMuDst::mFmsCollection     = 0;
TClonesArray** StMuDst::eztArrays            = 0;

Int_t StMuDst::mCurrVertexId                 = -2;
TObjArray* StMuDst::mCurrPrimaryTracks       = 0;

StMuDst::StMuDst() {
  DEBUGMESSAGE("");
  /* no-op */
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
  int nV0s = v0s()->GetEntriesFast(); for (int i=0;i<nV0s; i++) v0s(i)->SetEvent(ev); // set the pointer to the StStrangeEvMuDst which is not read from disk
  int nXis = xis()->GetEntriesFast(); for (int i=0;i<nXis; i++) xis(i)->SetEvent(ev); // set the pointer to the StStrangeEvMuDst which is not read from disk
  //  int nKinks = kinks()->GetEntriesFast(); for (int i=0;i<nKinks; i++) kinks(i)->SetEvent(ev);
#endif
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
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
  if (mCurrVertexId == vtx_id)  
     return;
  mCurrVertexId = vtx_id;
  collectVertexTracks();  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuDst::fixTrackIndices() {
  /// global and primary tracks share the same id, so we can fix the 
  /// index2Global up in case they got out of order (e.g. by removing 
  /// a track from the TClonesArrays
    fixTrackIndices( arrays[muPrimary], arrays[muGlobal] );  
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

  static int warningPrinted = 0;
  if (!warningPrinted) {
     LOG_WARN << "WARNING: You are using " << __PRETTY_FUNCTION__ 
              << " which does not work properly " 
                 " for productions with FTPC >= SL04d and <= SL05g" << endm;
     warningPrinted = 1;
  }
  int nGlobals = global->GetEntriesFast();
  int nPrimaries = primary->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<short,unsigned short> globalIndex;

  for (int i=0; i<nGlobals; i++) {
    StMuTrack *g = (StMuTrack*) global->UncheckedAt(i);
    if (g) {
      globalIndex[g->id()] = i+1;
      globalTracks(i)->setIndex2Global(i);
    }
  }
  // set the indices for the primary tracks
  DEBUGVALUE2(primary->GetEntriesFast());
  for (int i=0; i<nPrimaries; i++) {
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

void StMuDst::fixTrackIndicesG(int mult) {
	/// Match global track index to primary track
	//mult = 0 means there is just a single vertex in the event, mult>0 means there are multiple...

	if (mult==0){
		if(!(fabs(event()->primaryVertexPosition().x()) < 1.e-5 && fabs(event()->primaryVertexPosition().y()) < 1.e-5 && fabs(event()->primaryVertexPosition().z()) < 1.e-5)){   
			int startpos = 0;
			int tid, pid;
			if(!globalTracks()) return;
			for (int i=0;i<globalTracks()->GetEntriesFast();i++){
				tid = globalTracks(i)->id();
				globalTracks(i)->setIndex2Global(-2);
				if(!primaryTracks()) return;
				for(int j=startpos;j<primaryTracks()->GetEntriesFast();j++){
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
	const int Nvert = primaryVertices()->GetEntriesFast();
	if(!Nvert) return;
	int curVer =  currentVertexIndex();
	int startpos[Nvert];
	for(int i=0;i<Nvert;i++) startpos[i]=0;	
	int tid, pid;
	if(!globalTracks()) return;

	for (int i=0;i<globalTracks()->GetEntriesFast();i++){
		tid = globalTracks(i)->id();
		globalTracks(i)->setIndex2Global(-2);
		globalTracks(i)->setVertexIndex(-2);			
		//Scan through vertices
		for(int j=0;j<Nvert;j++){
			if(globalTracks(i)->index2Global() >= 0) break;
			setVertexIndex(j);
			if(!primaryTracks()) continue;
			for(int k=startpos[j];k<primaryTracks()->GetEntriesFast();k++){
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

  int nPrimarys = primary->GetEntriesFast();
  int nGlobals = global->GetEntriesFast();
  int nBTofHits = btofHit->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<short,unsigned short> tofIndex;
  map<short,unsigned short> globalIndex;
  map<short,unsigned short> primaryIndex;

  for (int i=0; i<nBTofHits; i++) {
    StMuBTofHit *t = (StMuBTofHit*) btofHit->UncheckedAt(i);
    if (t) {
      tofIndex[t->associatedTrackId()] = i+1;  // starting from 1
    }
  }

  for (int i=0; i<nGlobals; i++) {
    StMuTrack *g = (StMuTrack*) global->UncheckedAt(i);
    if (g) {
      globalIndex[g->id()] = i+1;

      if(tofIndex[g->id()])
        g->setIndex2BTofHit( tofIndex[g->id()]-1 );
      else
        g->setIndex2BTofHit(-1);
    }
  }
  for (int i=0; i<nPrimarys; i++) {
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
  for (int i=0; i<nBTofHits; i++) {
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

 int nPrimarys = primary->GetEntriesFast();
  int nGlobals = global->GetEntriesFast();
  int nMtdHits = mtdHit->GetEntriesFast();
  // map to keep track of index numbers, key is track->id(), value is index of track in MuDst
  map<short,unsigned short> mtdIndex;
  map<short,unsigned short> globalIndex;
  map<short,unsigned short> primaryIndex;

  for (int i=0; i<nMtdHits; i++) {
    StMuMtdHit *t = (StMuMtdHit*) mtdHit->UncheckedAt(i);
    if (t) {
      mtdIndex[t->associatedTrackKey()] = i+1;  // starting from 1
    }
  }

  for (int i=0; i<nGlobals; i++) {
    StMuTrack *g = (StMuTrack*) global->UncheckedAt(i);
    if (g) {
      globalIndex[g->id()] = i+1;

      if(mtdIndex[g->id()])
        g->setIndex2MtdHit( mtdIndex[g->id()]-1 );
      else
        g->setIndex2MtdHit(-1);
    }
  }
  for (int i=0; i<nPrimarys; i++) {
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
  for (int i=0; i<nMtdHits; i++) {
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

  int nGlobals = arrays[muGlobal]->GetEntriesFast();

  StSPtrVecTrackNode &trackNodes = ev->trackNodes();
  TArrayI global_indices(nGlobals); // Temporary array to keep track of index numbers on trackNodes

  // add global tracks to tracknodes
  for (int i=0; i<nGlobals; i++) {
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

  int nPrimaries = prim_tracks->GetEntriesFast();
  for (int i=0; i<nPrimaries; i++) if(primaryTracks(i)) {
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
  int nStates = arrays[muState]->GetEntriesFast();
  for (int i=0; i<nStates; i++) {
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
  int nTofData = tofArrays[muTofData]->GetEntriesFast();
  for(int i=0;i<nTofData;i++) {
    StTofData *aData;
    if(tofData(i)) {
      unsigned short id = tofData(i)->dataIndex();
      unsigned short adc = tofData(i)->adc();
      unsigned short tdc = tofData(i)->tdc();
      short tc = tofData(i)->tc();
      unsigned short sc = tofData(i)->sc();
      // run 5 - dongx
      aData = new StTofData(id, adc, tdc, tc, sc, 0, 0);
    } else {
      aData = new StTofData(0, 0, 0, 0, 0, 0, 0);
    }
    tofcoll->addData(aData);
  }
  // run 5 - dongx
  int nTofRawData = tofArrays[muTofRawData]->GetEntriesFast();
  for(int i=0;i<nTofRawData;i++) {
    StTofRawData *aRawData;
    if(tofRawData(i)) {
      unsigned short tray = tofRawData(i)->tray();
      unsigned short leteFlag = tofRawData(i)->leteFlag();
      unsigned short channel = tofRawData(i)->channel();
      unsigned int tdc = tofRawData(i)->tdc();
      unsigned int triggertime = tofRawData(i)->triggertime();
      unsigned short quality = tofRawData(i)->quality();
      aRawData = new StTofRawData(leteFlag,tray,channel,tdc,triggertime,quality);
    } else {
      aRawData = new StTofRawData(0, 0, 0, 0, 0, 0);
    }
    tofcoll->addRawData(aRawData);
  }

  // now create, fill the StBTofCollection - dongx
  StBTofCollection *btofcoll = new StBTofCollection();
  ev->setBTofCollection(btofcoll);
  int nBTofRawHits = btofArrays[muBTofRawHit]->GetEntriesFast();
  for(int i=0;i<nBTofRawHits;i++) {
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
StTrackGeometry* StMuDst::trackGeometry(int q, StPhysicalHelixD* h) {
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
    cout << endl << "primary vertex pos " << event->primaryVertexPosition() << endl;
  }
  else 
    cout << "No event structure (StMuEvent) found!" << endl;


  cout << numberOfPrimaryVertices() << " vertices reconstructed" << endl;
  cout << numberOfPrimaryTracks() << " primary tracks, ";
  if (mCurrVertexId != 0)
    cout << "( note vtx_id " << mCurrVertexId << " ) " ; 
  cout << numberOfGlobalTracks() << " global " << endl;

#ifndef __NO_STRANGE_MUDST__
  cout << numberOfV0s() << " V0s, " << numberOfXis() << " Xis " 
       << numberOfKinks() << " kinks" << endl;
#endif
  cout << endl;
  if (muEmcCollection())
    cout << "EMC data present" << endl;
  else
    cout << "No EMC data present" << endl;
   cout << endl;
   if (muFmsCollection())
     cout << "FMS data present" << endl;
   else
     cout << "No FMS data present" << endl;
   if (pmdCollection())
    cout << "PMD data present" << endl;
  else
    cout << "No PMD data present" << endl;

  if (numberOfTofHit())
    cout << "TOF data present" << endl;
  else
    cout << "No TOF data present" << endl;
  cout << endl;
}

void StMuDst::printVertices()  {
  if (numberOfPrimaryVertices() == 0) {
    cout << "No vertices stored (for older data, check StMuEvent)" << endl;
    return;
  }
  cout << endl;
  cout << "+++++++++ vertex list ( " << numberOfPrimaryVertices() << " entries )" << endl << endl;
  for (UInt_t i_vtx = 0; i_vtx < numberOfPrimaryVertices(); i_vtx++) {
    cout << "+++ Vertex " << i_vtx << endl;
    primaryVertex(i_vtx)->Print();
    cout << endl;
  }
}

void StMuDst::printPrimaryTracks() {
  if (numberOfPrimaryTracks() == 0) {
    cout << "No primary tracks found!" << endl;
    return;
  }
  cout << endl;
  cout << "+++++++++ PRIMARY track list ( " << numberOfPrimaryTracks() << " entries )" << endl << endl;
  for (UInt_t i_trk = 0; i_trk < numberOfPrimaryTracks(); i_trk++) {
    cout << "+++ Primary track " << i_trk << endl;
    primaryTracks(i_trk)->Print();
    cout << endl;
  }
}

void StMuDst::printGlobalTracks()  {
  if (numberOfGlobalTracks() == 0) {
    cout << "No global tracks found!" << endl;
    return;
  }
  cout << endl;
  cout << "+++++++++ GLOBAL track list ( " << numberOfGlobalTracks() << " entries )" << endl << endl;
  for (UInt_t i_trk = 0; i_trk < numberOfGlobalTracks(); i_trk++) {
    cout << "+++ Global track " << i_trk << endl;
    globalTracks(i_trk)->Print();
    cout << endl;
  }
}

ClassImp(StMuDst)

/***************************************************************************
 *
 * $Log: StMuDst.cxx,v $
 * Revision 1.70  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
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
