/***************************************************************************
 *
 * $Id: StMuDstFilterMaker.cxx,v 1.1 2003/04/15 18:48:35 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#include "StMuDstFilterMaker.h"

#include "StMuDSTMaker/COMMON/StMuTypes.hh"


#include "StEvent/StEventTypes.h"

#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"

#include <vector>
#include <algorithm>

#define __COMPRESSION__ 9
#define __BUFFER__ 65536
#define __SPLIT__ 99
#define __AUTOSAVE__ 1000000



StMuDstFilterMaker::StMuDstFilterMaker(const char* name) : StMaker(name), mMuDstMaker(0) {
  DEBUGMESSAGE2("");
}

/**
 * Gets pointer to the StMuDstMaker
 * Create output file and the TClonesArrays needed
 */
int StMuDstFilterMaker::Init() {
  if (!mMuDstMaker) throw StMuExceptionNullPointer("pointer to the StMuDstMaker not set",PF);
  mFile = new TFile(mFileName.c_str(),"RECREATE","StMuDst");
  if (!mFile) throw StMuExceptionNullPointer("no file openend",PF);
  mFile->SetCompressionLevel(__COMPRESSION__);
  
  // Create the TClonesArrays
  createArrays();

  // Create a ROOT Tree and one superbranch
  DEBUGMESSAGE2("now create trees and branches");
  
  TBranch* branch;
  mTTree = new TTree("MuDst", "StMuDst",__SPLIT__);
  if (!mTTree) throw StMuExceptionNullPointer("can not create tree",PF);
  mTTree->SetAutoSave(__AUTOSAVE__);  // autosave when 1 Mbyte written
  //  muDst stuff
  DEBUGMESSAGE2("arrays");
  for ( int i=0; i<__NARRAYS__; i++) {
    DEBUGVALUE2(i);
    branch = mTTree->Branch(StMuArrays::arrayNames[i],&mArrays[i], __BUFFER__, __SPLIT__);
  }
  // strange stuff
  DEBUGMESSAGE2("strange arrays");
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    DEBUGVALUE2(i);
    branch = mTTree->Branch(StMuArrays::strangeArrayNames[i],&mStrangeArrays[i],  __BUFFER__, __SPLIT__);
  }
  // emc stuff
  DEBUGMESSAGE2("emc arrays");
  for ( int i=0; i<__NEMCARRAYS__; i++) {
    DEBUGVALUE2(i);
    branch = mTTree->Branch(StMuArrays::emcArrayNames[i],&mEmcArrays[i],  __BUFFER__, __SPLIT__);
  }
  return 0;
}

/**
 * Writes the tree to disk and closes the output file
 */
void StMuDstFilterMaker::close(){
  if (mTTree) mTTree->AutoSave(); 
  mTTree = 0;
  if (mFile) mFile->Close();
  mFile = 0;
}


StMuDstFilterMaker::~StMuDstFilterMaker() { 
  /* no=op */
}
    
 
void StMuDstFilterMaker::Clear() {
  /* no-op */
}

int StMuDstFilterMaker::Make(){  ///< create a StEvent from the muDst and put it into the .data tree 
    DEBUGMESSAGE1("");
    clear();
    if ( !mMuDstMaker ) return 0;
    
    StMuDst* muDst = mMuDstMaker->muDst();
    if ( !muDst ) return 0;

    /* In this example we want to write only primary tracks with pT>2,
     * the corresponding globals tracks and the event-wise information
     * and the event wise information. We will discarde the event completely if
     * the vertex is not |z|<50cm and if it has no tracks above 2GeV/c
     */
    // this is the function that decides whether the whole event shall be discarded or not
    if ( filter(muDst)==false ) return 0;

    /*
     * Now apply filters to the individual TClonesArrays.
     */

    //the event wise information first
    if ( filter( muDst->event() ) ) { 
	 addType( mArrays[muEvent], *(muDst->event()) );
    }

    //The tracks are the most difficult part, because the different typ of tracks are related via their ids.
    //For all primary tracks that are accepted, I also want to right the global track
    vector<int> ids; 
    int numberOfTracks = 0;
    numberOfTracks = muDst->primaryTracks()->GetEntries();
    for ( int i=0; i<numberOfTracks; i++) {
	StMuTrack* track = muDst->primaryTracks(i);
	if ( filter( track )==true ) {
	    ids.push_back( track->id() );
	    addType( mArrays[muPrimary], *track );
	}
    }
    sort(ids.begin(),ids.end()); // sort so that we can use the fast binary search
    numberOfTracks = muDst->globalTracks()->GetEntries();
    for ( int i=0; i<numberOfTracks; i++) {
	StMuTrack* track = muDst->globalTracks(i);
	if ( binary_search(ids.begin(),ids.end(),track->id()) ) { // if the id is in the list
	    addType( mArrays[muGlobal], *track );
	}
    }
    // primary tracks are pointing to the global tracks, we have to set the indecies right
    StMuDst::fixTrackIndices( mArrays[muPrimary],mArrays[muGlobal] );

    // the emc collection
    StMuEmcCollection* emc = muDst->emcCollection();
    if ( filter(emc) ) {
	addType( mEmcArrays[0], *emc );
    }
    
    
    // write the event only if it has at least one primary track
    if ( mArrays[muPrimary]->GetEntries()>0) mTTree->Fill();
    return 0;
}

template <class T>
int StMuDstFilterMaker::addType(TClonesArray* tcaTo , T t) {
  int counter =-1;
  if (tcaTo) {
    counter = tcaTo->GetEntries();
    new((*tcaTo)[counter]) T( t );
  }
  return counter;
}


int StMuDstFilterMaker::Finish() { 
    close();
    return 0;
}

/**
 * Clears all TClonesArrays
 */
void StMuDstFilterMaker::clear(){
  DEBUGMESSAGE2("");
  int dummy;
  /// from muDst
  for ( int i=0; i<__NARRAYS__; i++) mMuDstMaker->clear(mArrays[i],dummy);
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) mMuDstMaker->clear(mStrangeArrays[i],dummy);
  for ( int i=0; i<__NEMCARRAYS__; i++) mMuDstMaker->clear(mEmcArrays[i],dummy);
}

/**
 * Creates all the TClonesArrays.
 * There are three set of arrays: the "regular", the "strangeness" and the "emc" arrays
 */
void StMuDstFilterMaker::createArrays() {
  /// regular stuff
    int dummy;
    for ( int i=0; i<__NARRAYS__; i++) {
	DEBUGVALUE2(arrays[i]);
	mArrays[i]= mMuDstMaker->clonesArray(arrays[i],StMuArrays::arrayTypes[i],StMuArrays::arraySizes[i],dummy);
	DEBUGVALUE2(arrays[i]);
    }
    /// from strangeness group
    for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
	mStrangeArrays[i]= mMuDstMaker->clonesArray(strangeArrays[i],StMuArrays::strangeArrayTypes[i],StMuArrays::strangeArraySizes[i],dummy);
    }
    /// from emc group
    for ( int i=0; i<__NEMCARRAYS__; i++) {
	mEmcArrays[i]= mMuDstMaker->clonesArray(emcArrays[i],StMuArrays::emcArrayTypes[i],StMuArrays::emcArraySizes[i],dummy);
  }
}

/*
 * Here I define my cuts by specializing the
 * template<class T> bool filter(T*)
 * function
 */
/// and I want to keep all tracks with p > 1 and 0<eta<1
bool StMuDstFilterMaker::filter(StMuTrack* track) {
    return ( track->p().mag()>1. ) && ( fabs(track->eta()-0.5)<0.5 );
}


ClassImp(StMuDstFilterMaker)

/***************************************************************************
 *
 * $Log: StMuDstFilterMaker.cxx,v $
 * Revision 1.1  2003/04/15 18:48:35  laue
 * Minor changes to be able to filter MuDst.root files and an example
 * how to do this. The StMuDstFilterMaker is just an example, it has to be
 * customized (spoilers, chrome weels, etc.) by the user.
 *
 *
 **************************************************************************/
