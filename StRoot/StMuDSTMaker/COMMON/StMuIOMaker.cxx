/***************************************************************************
 *
 * $Id: StMuIOMaker.cxx,v 1.1 2003/09/09 00:05:21 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include <fstream>
#include <strstream>

#include "St_base/StFileI.h"

#include "StEvent/StEvent.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StRichSpectra.h"
#include "StEvent/StDetectorState.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StRunInfo.h"
#include "StEvent/StEventInfo.h"

#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StTimer.hh"

#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StV0Mc.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StXiMc.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMc.hh"
#include "StStrangeMuDstMaker/StStrangeCuts.hh"


#include "StMuException.hh"
#include "StMuEvent.h"
#include "StMuTrack.h"
#include "StMuDebug.h"
#include "StMuCut.h"
#include "StMuFilter.h"
#include "StMuL3Filter.h"
#include "StMuChainMaker.h"
#include "StMuEmcCollection.h"
#include "StMuEmcUtil.h"
#include "StMuTimer.h"

#include "StMuIOMaker.h"
#include "StMuDst.h"

#include "TFile.h"
#include "TTree.h"
#include "TClass.h"
#include "TChain.h"
#include "TStreamerInfo.h"
#include "TClonesArray.h"


ClassImp(StMuIOMaker)

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif



//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   The default constructor as it is right now was written in order to run the StMuIOMaker during reconstruction in the bfc.
   Since the PID table that is needed for muDst production is not passed as an argument to the bfc, this default constructor
   sets a specific PID table. This table has to be updated when changing to a new production version.
   Also, the standard track and l3 track filters are set.
 */
StMuIOMaker::StMuIOMaker(const char* name) : StIOInterFace(name,"r"), mChain(0) {
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
   The Init() routine is used to establish contact to other makers. As it is STAR habit (but really really bad coding) we identify the 
   other makers by names (instead of passing pointers). Here, the names are hard-wired because they have to be identical to the names 
   the bfc is assining to the makers. Do not alter these names unless you know what you are doing.
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
int StMuIOMaker::Finish() {
  DEBUGMESSAGE2("");
  closeRead();
  DEBUGMESSAGE3("out");
  return 0;
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
void StMuIOMaker::openRead() {
  DEBUGVALUE2(fFile);
  if ( mChain ) {
	  closeRead();
  }
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
  mStMuDst->set(mArrays,mStrangeArrays,mEmcArrays);  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::Make(int index){
  DEBUGMESSAGE2("");
  DEBUGVALUE3(index);
  mCurrentIndex = index;
  mStMuDst->unset();
  if ( mCurrentIndex >= mNumberOfEvents) return kStEOF;
  if ( mCurrentIndex < 0 ) return kStErr;
  int bytes = mChain->GetEntry(mCurrentIndex);
  DEBUGVALUE3(bytes);
  mStMuDst->set(mArrays,mStrangeArrays,mEmcArrays);
  mEventCounter++;
  return kStOk;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
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
int StMuIOMaker::Make(const StUKey& key){
  DEBUGMESSAGE2("");
  int index = mChain->GetEntryNumberWithIndex(key.GetRunId(),key.GetEventId());
  return Make(index);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::Make(){
  return Make(mCurrentIndex+1);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::closeRead(){
  DEBUGMESSAGE2("");
  if (mChain) mChain->Delete();
  mChain = 0;
 }
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void  StMuIOMaker::SetFileName(const char *fileName){
	fFile = fileName;
	openRead();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void  StMuIOMaker::SetFile(const char *fileName)    {
	fFile = fileName;
	openRead();
}


/***************************************************************************
 *
 * $Log: StMuIOMaker.cxx,v $
 * Revision 1.1  2003/09/09 00:05:21  jeromel
 * First version
 *
 **************************************************************************/



















