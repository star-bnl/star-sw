/***************************************************************************
 *
 * $Id: StMuIOMaker.cxx,v 1.2 2003/09/09 18:16:53 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include <fstream>
#include <strstream>

#include "St_base/StFileI.h"

#include "StarClassLibrary/StTimer.hh"


#include "StMuEmcUtil.h"
#include "StMuDebug.h"
#include "StMuIOMaker.h"
#include "StMuDst.h"

#include "TFile.h"
#include "TTree.h"
#include "TClass.h"
#include "TChain.h"
#include "TStreamerInfo.h"
#include "TClonesArray.h"


ClassImp(StMuIOMaker)



//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   The default constructor. Creates the TClonesArrays, the StMuDst instance and an instance of the 
   EMC helper class StMuEmcUtil.
   The mCurrentIndex variable is initialized to -1, indicating that no valid event was read.
 */
StMuIOMaker::StMuIOMaker(const char* name) : StIOInterFace(name,"r"), 
mNumberOfEvents(0),
mCurrentIndex(-1), mEventCounter(0), mChain(0) {
	mStMuDst = new StMuDst();
	mEmcUtil = new StMuEmcUtil();
	createArrays();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuIOMaker::~StMuIOMaker() {
  DEBUGMESSAGE1("");
  clear();
  delete mStMuDst;
  for ( int i=0; i<__NARRAYS__; i++) { delete arrays[i]; arrays[i]=0;} 
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) { delete strangeArrays[i];strangeArrays[i]=0;}
  for ( int i=0; i<__NEMCARRAYS__; i++) { delete emcArrays[i]; emcArrays[i]=0;}
  DEBUGMESSAGE3("after arrays");
  closeRead();
  DEBUGMESSAGE3("after close");
  if (mChain) { 
	  delete mChain;
	  mChain=0;
  }
  DEBUGMESSAGE3("out");
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::createArrays() {
  /// regular stuff
  for ( int i=0; i<__NARRAYS__; i++) {
    DEBUGVALUE2(arrays[i]);
    mArrays[i]= clonesArray(arrays[i],StMuArrays::arrayTypes[i],StMuArrays::arraySizes[i],StMuArrays::arrayCounters[i]);
    DEBUGVALUE2(arrays[i]);
  } 
  /// from strangeness group
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    mStrangeArrays[i]= clonesArray(strangeArrays[i],StMuArrays::strangeArrayTypes[i],StMuArrays::strangeArraySizes[i],StMuArrays::strangeArrayCounters[i]);
  }
  /// from emcness group
  for ( int i=0; i<__NEMCARRAYS__; i++) {
    mEmcArrays[i]= clonesArray(emcArrays[i],StMuArrays::emcArrayTypes[i],StMuArrays::emcArraySizes[i],StMuArrays::emcArrayCounters[i]);
  }
  mStMuDst->set(mArrays,mStrangeArrays,mEmcArrays);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::clear(){
  DEBUGMESSAGE2("");
  /// from muDst
  for ( int i=0; i<__NARRAYS__; i++) {
    clear(mArrays[i],StMuArrays::arrayCounters[i]);
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    clear(mStrangeArrays[i],StMuArrays::strangeArrayCounters[i]);
  }
  for ( int i=0; i<__NEMCARRAYS__; i++) {
    del(mEmcArrays[i],StMuArrays::emcArrayCounters[i]);
  }
  DEBUGMESSAGE2("out");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::clear(TClonesArray* t, int& counter){
  DEBUGMESSAGE3("");
  if (t) { 
    t->Clear(""); 
    counter=0;
  }
 DEBUGMESSAGE3("out");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::del(TClonesArray* t, int& counter){
  DEBUGMESSAGE3("");
  if (t) { 
    if (t->UncheckedAt(0)) {
      ((StMuEmcCollection*)t->UncheckedAt(0))->DeleteThis();
    }
    counter=0;
  }
  DEBUGMESSAGE3("out");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
TClonesArray* StMuIOMaker::clonesArray(TClonesArray*& p, const char* type, int size, int& counter) {
  DEBUGMESSAGE2("");
  if (!p) {
    DEBUGVALUE2(type);
    p = new TClonesArray(type, size);
    counter=0;
  }
  if (!p) throw StMuExceptionNullPointer("could not create TClonesArray",PF);
  return p;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   The Init() routine is used to establish contact to other makers. 
   Here, the names are hard-wired because they have to be identical to the 
   names the bfc is assigning to the makers. Do not alter these names 
   unless you know what you are doing.
*/
int StMuIOMaker::Init(){
  DEBUGMESSAGE2("");
  TObjectSet *muDstSet =  AddObj(mStMuDst,".const");   ///< added for Valeri to be able to pick it up in other makers 
  if (muDstSet ) muDstSet->SetName("muDst");          ///< added for Valeri to be able to pick it up in other makers 
  return 0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::Clear(){
  DEBUGMESSAGE2("");
  clear();
  DEBUGMESSAGE3("out");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   Just closes the input file. 
 */
int StMuIOMaker::Finish() {
  DEBUGMESSAGE2("");
  closeRead();
  DEBUGMESSAGE3("out");
  return kStOK;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::setBranchAddresses(TChain* chain) {
  // muDst stuff
  for ( int i=0; i<__NARRAYS__; i++) {
    chain->SetBranchAddress(StMuArrays::arrayNames[i],&mArrays[i]);
  } 
  
  // strange stuff
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    chain->SetBranchAddress(StMuArrays::strangeArrayNames[i],&mStrangeArrays[i]);
  } 
  
  // emc stuff
  for ( int i=0; i<__NEMCARRAYS__; i++) {
      chain->SetBranchAddress(StMuArrays::emcArrayNames[i],&mEmcArrays[i]);
  } 
  TTree *tree;
  tree = mChain->GetTree();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	Closes open input files (deletes the input chain). Then creates a new 
	TChain, builds up the index from runId and eventID. Sets the local
	TClonesArray pointers to the branch addresses of the TChain.
 */
void StMuIOMaker::openRead() {
  DEBUGVALUE2(fFile);
  if ( mChain ) {
	  closeRead();
  }
  mCurrentIndex = -1;	
  mChain = new TChain("MuDst");
  mChain->Add(fFile);
  mNumberOfEvents = mChain->GetEntries();
  StTimer timer;
  timer.reset();
  timer.start();
  mChain->BuildIndex("MuEvent.mEventInfo.mRunId","MuEvent.mEventInfo.mId");
  timer.stop();
  cout << " Index of " << mNumberOfEvents << " events buit in " << timer.elapsedTime()/1000. << " seconds " << endl;  
  setBranchAddresses(mChain);
  mStMuDst->unset();  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	This is the function that actually reads a new event. All other 
	implementations of Make will eventually call this function.
	@param  the index of the event to read
	@return kStEOF if the requested index is greater that the number of events in the chain
	@return kStErr if the requested index does not exists
	@return kStOk if the requested event was read successfully 
	If for any reason no event is read, the pointers to the TClonesArrays 
	are set to 0. It will cause a crash if you will try to access 
	information from the pointer to the StMuDst.  This is done for ,
	so that nobody reads the same (last) event over and over again without 
	noticing. Please check the return values of Make(), only kStOk means a 
	an event was read succesfull and data can be extracted.
 */
int StMuIOMaker::Make(int index){
  DEBUGMESSAGE2("");
  DEBUGVALUE3(index);
  mCurrentIndex = index;
  mStMuDst->unset();
  if ( mCurrentIndex >= mNumberOfEvents) return kStEOF;
  if ( mCurrentIndex < 0 ) return kStErr;
  if (!mChain) return kStEOF;
  int bytes = mChain->GetEntry(mCurrentIndex);
  DEBUGVALUE3(bytes);
  mStMuDst->set(mArrays,mStrangeArrays,mEmcArrays);
  mEventCounter++;
  return kStOk;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	Causes an event identified by runId and and eventID to be read.
	@return kStEOF if the requested index is greater that the number of events in the chain
	@return kStErr if the requested index does not exists
	@return kStOk if the requested event was read successfully 
	@param runId, eventId
 */
int StMuIOMaker::Make(int major, int minor){
  DEBUGMESSAGE2("");
  DEBUGVALUE3(major);
  DEBUGVALUE3(minor);
  int index = mChain->GetEntryNumberWithIndex(major,minor);
  return Make(index);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	Causes an event identified by a StUKey to be read.
	@return kStEOF if the requested index is greater that the number of events in the chain
	@return kStErr if the requested index does not exists
	@return kStOk if the requested event was read successfully 
	@param StUKey identifier (runId and eventId will be extracted from the StUKey)
 */
int StMuIOMaker::Make(const StUKey& key){
  DEBUGMESSAGE2("");
  int index = mChain->GetEntryNumberWithIndex(key.GetRunId(),key.GetEventId());
  return Make(index);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	Causes the next event in the chain to be read
	@return kStEOF if the requested index is greater that the number of events in the chain
	@return kStErr if the requested index does not exists
	@return kStOk if the requested event was read successfully 
 */
int StMuIOMaker::Make(){
  return Make(mCurrentIndex+1);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::closeRead(){
/**
	Deletes the current chain.
 */
	DEBUGMESSAGE2("");
  if (mChain) mChain->Delete();
  mChain = 0;
 }
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	Sets the new file to be read by deleting the old chain firt and then 
	opening a new chain.
	@param full path and filename of the next file to be read
 */
void  StMuIOMaker::SetFileName(const char *fileName){
	closeRead();
	fFile = fileName;
	openRead();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	Sets the new file to be read by deleting the old chain firt and then 
	opening a new chain.
	@param full path and filename of the next file to be read
 */
void  StMuIOMaker::SetFile(const char *fileName)    {
	closeRead();
	fFile = fileName;
	openRead();
}


/***************************************************************************
 *
 * $Log: StMuIOMaker.cxx,v $
 * Revision 1.2  2003/09/09 18:16:53  laue
 * StMuIOMaker: embedded documentation added
 * StMuTimer: name of define changed (was same as StTimer)
 *
 **************************************************************************/



















