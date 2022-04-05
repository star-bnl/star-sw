/***************************************************************************
 *
 * $Id: StMuDstFilterMaker.cxx,v 1.18 2016/05/04 19:24:07 smirnovd Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#include "StMuDstFilterMaker.h"

#include "StMuDSTMaker/COMMON/StMuTypes.hh"


#include "StEvent/StEventTypes.h"

#include "THack.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TChain.h"

#include <vector>
#include <algorithm>

#define __COMPRESSION__ 9
#define __BUFFER__ 65536
#define __SPLIT__ 99
#define __AUTOSAVE__ 1000000



StMuDstFilterMaker::StMuDstFilterMaker(const char* name) : StMaker(name), mMuDstMaker(0), mFile(0), mTTree(0), mFilterGlobals(1), mDoBemc(1), mDoEemc(1) {
  DEBUGMESSAGE2("");
  // Create the TClonesArrays
  createArrays();
}

/**
 * Gets pointer to the StMuDstMaker
 * Create output file and the TClonesArrays needed
 */
void StMuDstFilterMaker::open(const Char_t *fname) {
  mFile = new TFile(fname,"RECREATE","StMuDst");
  if (mFile->IsZombie() ) throw StMuExceptionNullPointer("no file openend",__PRETTYF__);
  mFile->SetCompressionLevel(__COMPRESSION__);

  // Create a ROOT Tree and one superbranch
  DEBUGMESSAGE2("now create trees and branches");
  
  mTTree = new TTree("MuDst", "StMuDst",__SPLIT__);
  if (!mTTree) throw StMuExceptionNullPointer("can not create tree",__PRETTYF__);
#if ROOT_VERSION_CODE < ROOT_VERSION(5,26,0)
  Long64_t MAXLONG=100000000000LL; // 100 GB
  LOG_INFO << "Tree size MAX will be " << (float) MAXLONG/1000/1000/1000 << " GB " << endm;

  mTTree->SetMaxTreeSize(MAXLONG); 
#endif
  //  muDst stuff
  DEBUGMESSAGE2("arrays");
  for ( int i=0; i<__NARRAYS__; i++) {
    DEBUGVALUE2(i);
    mTTree->Branch(StMuArrays::arrayNames[i],&mArrays[i], __BUFFER__, __SPLIT__);
  }
#ifndef __NO_STRANGE_MUDST__
  // strange stuff
  DEBUGMESSAGE2("strange arrays");
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    DEBUGVALUE2(i);
    mTTree->Branch(StMuArrays::strangeArrayNames[i],&mStrangeArrays[i],  __BUFFER__, __SPLIT__);
  }
#endif
  // emc stuff
  DEBUGMESSAGE2("emc arrays");
  for ( int i=0; i<__NEMCARRAYS__; i++) {
    DEBUGVALUE2(i);
    mTTree->Branch(StMuArrays::emcArrayNames[i],&mEmcArrays[i],  __BUFFER__, __SPLIT__);
  }
}

/**
 * Writes the tree to disk and closes the output file
 */
void StMuDstFilterMaker::close(){
  if (mFile) {
    mFile->Write();
    mFile->Close();
  }
  mFile = 0;
  mTTree = 0;
}


StMuDstFilterMaker::~StMuDstFilterMaker() { 
  /* no=op */
}
    
 

int StMuDstFilterMaker::Make(){  ///< create a StEvent from the muDst and put it into the .data tree 
    DEBUGMESSAGE1("");
    if ( !mMuDstMaker ) return 0;
    
    if (mFile==0 || mCurFileName != mMuDstMaker->chain()->GetFile()->GetName()) {
       string outName;
       if (mOutDirName.size())
          outName=mOutDirName+'/';
       if (mOutFileName.size()) {
          outName+=mOutFileName;
       }
       else {
         close();
	 const Char_t *inName = mMuDstMaker->chain()->GetFile()->GetName();
         const Char_t *baseName = strrchr(inName,'/');
         if (!baseName)
            baseName=inName;
         else 
            baseName++; 
         outName+=baseName;
       }
       if (mFile==0) {
          cout << "Opening output file " << outName << endl;
          open(outName.c_str());
       }
       mCurFileName = mMuDstMaker->chain()->GetFile()->GetName();
    }
    StMuDst* muDst = mMuDstMaker->muDst();
    if ( !muDst ) return 0;

    /* In this example we want to write only primary tracks with pT>2,
     * the corresponding globals tracks and the event-wise information
     * and the event wise information. We will discarde the event completely if
     * the vertex is not |z|<50cm and if it has no tracks above 2GeV/c
     */
    // this is the function that decides whether the whole event shall be discarded or not
    /*
    cout << "event at " << muDst->event();
    if (muDst->event() != 0)
      cout << ", vtx z " << muDst->event()->primaryVertexPosition().z() << endl;
    else
      cout << endl;
    */
    clear();
    if ( filter(muDst)==false ) return 0;

    /*
     * Now apply filters to the individual TClonesArrays.
     */

    //the event wise information first
    if ( filter( muDst->event() ) == 0 ) return 0;
       
    DEBUGMESSAGE("Event accepted");
    addType( mArrays[muEvent], *(muDst->event()) );

    // Add first primary vertex by default
    if (muDst->primaryVertex()) {
      addType( mArrays[muPrimaryVertex], *(muDst->primaryVertex()) );
    }

    // The tracks are the most difficult part, because the primary tracks point
    // to their global counterparts.
    // For now, we are just keeping the global counterparts of all primary 
    // tracks, so thta's  relatively easy to keep track off.

    // Note that for some productions we cannot trust the uniqueness of 
    // the track keys (StMuTrack::id()) anymore.

    TArrayI globals_stored(muDst->globalTracks()->GetEntries());	  
    int numberOfTracks = 0;
    numberOfTracks = muDst->primaryTracks()->GetEntries();
    for ( int i=0; i<numberOfTracks; i++) {
      StMuTrack* track = muDst->primaryTracks(i); // Note: this only works for a single primary vertex
      if ( filter( track )==true ) {
	Int_t global_idx=track->index2Global();
	if (global_idx >= 0 && globals_stored[global_idx]==0) {
	  
	  Int_t nw_global_idx = addType( mArrays[muGlobal], *muDst->globalTracks(global_idx) );
	  track->setIndex2Global(nw_global_idx);
	  globals_stored[global_idx] = 1;
	} 
	addType( mArrays[muPrimary], *track );
      }
    }

    numberOfTracks = muDst->globalTracks()->GetEntries();
    for ( int i=0; i<numberOfTracks; i++) {
      StMuTrack* track = muDst->globalTracks(i);
      if (mFilterGlobals && globals_stored[i] == 0) {
	if (filter(track)) {
	  addType( mArrays[muGlobal], *track);
	} 
      }
    }

    // the emc collection
    StMuEmcCollection* emc = muDst->muEmcCollection();
    if ( filter(emc) ) {
      // This only works if the input MuDst is in the new format 
      // (data spread over multiple branches, not stored in StMuEmcCollection)
      StMuEmcTowerData *typeOfTowerData=0;
      StMuEmcHit *typeOfEmcHit=0;
      if (mDoBemc || mDoEemc) {
        addType( muDst->emcArray(muEmcTow), mEmcArrays[muEmcTow], typeOfTowerData ); 
        if (mEmcArrays[muEmcTow]->GetEntries()) {
          if (!mDoBemc) 
            ((StMuEmcTowerData*) mEmcArrays[muEmcTow]->UncheckedAt(0))->clearBemc();
          if (!mDoEemc) 
            ((StMuEmcTowerData*) mEmcArrays[muEmcTow]->UncheckedAt(0))->clearEemc();
        }
      }
      if (mDoBemc) {
        addType( muDst->emcArray(muEmcPrs), mEmcArrays[muEmcPrs], typeOfEmcHit ); 
        addType( muDst->emcArray(muEmcSmde), mEmcArrays[muEmcSmde], typeOfEmcHit ); 
        addType( muDst->emcArray(muEmcSmdp), mEmcArrays[muEmcSmdp], typeOfEmcHit ); 
      }
      if (mDoEemc) {
        addType( muDst->emcArray(muEEmcPrs), mEmcArrays[muEEmcPrs], typeOfEmcHit ); 
        addType( muDst->emcArray(muEEmcSmdu), mEmcArrays[muEEmcSmdu], typeOfEmcHit ); 
        addType( muDst->emcArray(muEEmcSmdv), mEmcArrays[muEEmcSmdv], typeOfEmcHit ); 
      }
    }
    
    // write the event only if it has at least one primary track
    if ( mArrays[muPrimary]->GetEntries()>0) { 
      mTTree->Fill(); THack::IsTreeWritable(mTTree);
    }
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

template <class T>
int StMuDstFilterMaker::addType(TClonesArray* tcaFrom, TClonesArray* tcaTo ,T *t) {
  if (tcaFrom && tcaTo) {
    int n = tcaFrom->GetEntries();
    int counter = tcaTo->GetEntries();
    for (int i=0; i<n;i++) {
      new((*tcaTo)[counter++]) T( *(T*)(void*)tcaFrom->UncheckedAt(i) );
    }
  }
  return 0;
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

  clearArrays();
}

/**
 * Creates all the TClonesArrays.
 * There are three set of arrays: the "regular", the "strangeness" and the "emc" arrays
 */
void StMuDstFilterMaker::createArrays() {
  /// regular stuff
    int dummy;
    for ( int i=0; i<__NARRAYS__; i++) {
	mArrays[i] = 0;
	DEBUGVALUE2(mArrays[i]);
	mArrays[i]= mMuDstMaker->clonesArray(mArrays[i],StMuArrays::arrayTypes[i],StMuArrays::arraySizes[i],dummy);
	DEBUGVALUE2(mArrays[i]);
    }
#ifndef __NO_STRANGE_MUDST__
    /// from strangeness group
    for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
	mStrangeArrays[i] = 0;
	mStrangeArrays[i]= mMuDstMaker->clonesArray(mStrangeArrays[i],StMuArrays::strangeArrayTypes[i],StMuArrays::strangeArraySizes[i],dummy);
    }
#endif
    /// from emc group
    for ( int i=0; i<__NEMCARRAYS__; i++) {
	mEmcArrays[i] = 0;
	mEmcArrays[i]= mMuDstMaker->clonesArray(mEmcArrays[i],StMuArrays::emcArrayTypes[i],StMuArrays::emcArraySizes[i],dummy);
  }
}

void StMuDstFilterMaker::clearArrays()
{
    /// regular stuff
    for ( int i=0; i<__NARRAYS__; i++) {
        mArrays[i]->Clear();
    }
#ifndef __NO_STRANGE_MUDST__
    /// from strangeness group
    for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
         mStrangeArrays[i]->Clear();
    }
#endif
    /// from emc group
    for ( int i=0; i<__NEMCARRAYS__; i++) {
        mEmcArrays[i]->Clear();
    }
}


/*
 * Here I define my cuts by specializing the
 * template<class T> bool filter(T*)
 * function
 */
/// and I want to keep all tracks with p > 1 and 0<eta<1
bool StMuDstFilterMaker::filter(StMuTrack* track) {
    return ( track->p().mag()>1. ) && ( fabs(track->eta())<1.0 );
}


ClassImp(StMuDstFilterMaker)

/***************************************************************************
 *
 * $Log: StMuDstFilterMaker.cxx,v $
 * Revision 1.18  2016/05/04 19:24:07  smirnovd
 * Addressed compiler warning by removing set but never used variables
 *
 * Revision 1.17  2011/08/18 18:41:36  fisyak
 * set max. tree size = 100 GB
 *
 * Revision 1.16  2011/04/19 22:50:08  fisyak
 * Use default size of TTree (100 GB) for ROOT >= 5.26.0
 *
 * Revision 1.15  2011/04/08 01:25:50  fisyak
 * Add branches for MC track and vertex information, add IdTruth to  tracks and vertices, reserve a possiblity to remove Strange MuDst
 *
 * Revision 1.14  2009/05/22 23:48:18  fine
 * Test I/O errors after filling the TTree
 *
 * Revision 1.13  2009/05/22 22:25:31  fine
 * Add the Zombue test for TFile ctors
 *
 * Revision 1.12  2009/03/10 23:43:53  jeromel
 * Set tree size to max size
 *
 * Revision 1.11  2005/12/19 01:55:55  mvl
 * Introduced check before copying primary vertex for backwards compatibility
 *
 * Revision 1.10  2005/12/13 03:12:13  mvl
 * Changes to StMuDst2StEventMaker (code in StMuDst) and StMuDstFilterMaker
 * to no longer rely on track keys for matching global and primary tracks.
 * This was needed because track keys are not guaranteed to be unique anymore.
 *
 * Revision 1.9  2005/05/23 19:46:20  mvl
 * Two incremental changes by Alex Suiade: A message is printed when a new output file is opened
 * and if StMuEvent does not pass the cut, the whole event is discarded
 * (previously, tracks could be kept, which does not make sense)
 *
 * Revision 1.8  2005/05/18 22:47:29  mvl
 * Fixed StMuDstFilterMaker to work again with changes in MuDstMaker
 * (the change in v1.6 was faulty. Thanks Alex for finding this)
 * Added some new features suggested by Alex Suiade:
 * - Emc data now supported (for SL04k and later MuDst).
 *   Flags added to switch Eemc and Bemc copying seperately (setDoBemc and setDoEemc)
 * - Global tracks are checked seperately. They were only copied
 *   if the corresponding primary fullfills the filter() criteria.
 *   Now they are also copied if only the global track fullfills the criteria
 *   Can be switched with setFilterGlobals()
 *
 * Revision 1.7  2004/10/21 02:57:25  mvl
 * Changed call to getter for StMuEmcCollection
 *
 * Revision 1.6  2004/10/19 01:47:57  mvl
 * Changed calls to clear TClonesArray in StMuDstMaker (not tested ;-()
 *
 * Revision 1.5  2004/02/17 04:56:36  jeromel
 * Extended help, added crs support, restored __GNUC__ for PRETTY_FUNCTION(checked once
 * more and yes, it is ONLY defined in GCC and so is __FUCTION__),  use of a consistent
 * internal __PRETTYF__, return NULL if no case selected (+message) and protected against
 * NULL mChain.
 *
 * Revision 1.4  2003/11/24 23:36:36  laue
 * commented the StMuEmcCollection out
 *
 * Revision 1.3  2003/09/07 03:49:03  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.2  2003/08/04 14:38:10  laue
 * Alex Suaide's updated for the EMC. Now EEMC is included.
 *
 * Revision 1.1  2003/04/15 18:48:35  laue
 * Minor changes to be able to filter MuDst.root files and an example
 * how to do this. The StMuDstFilterMaker is just an example, it has to be
 * customized (spoilers, chrome weels, etc.) by the user.
 *
 *
 **************************************************************************/
