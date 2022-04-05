/***************************************************************************
 *
 * $Id: StMuIOMaker.cxx,v 1.21 2011/08/18 18:41:36 fisyak Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 * Made it integrated to StIOMaker for applying Grid Collector 
 *                                            Wei-Ming Zhang KSU 3/8/2004 
 *
 **************************************************************************/
#include "StarClassLibrary/StTimer.hh"
//#include "StMuEmcUtil.h"
#include "StMessMgr.h"
#include "StMuDebug.h"
#include "StMuIOMaker.h"
#include "StMuDst.h"
#include "StMuEvent.h"

#include "TFile.h"
#include "THack.h"
#include "TChain.h"
#include "TClonesArray.h"

#include "THack.h"
ClassImp(StMuIOMaker)

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuIOMaker::StMuIOMaker(const char* name, const char *ioFile) : 
   StMuDstMaker(name) 
{
  mOutFile	=0;    


  mNumberOfEvents=0; 	//! holds the # of events in the current chain (file)
  mCurrentIndex	=0;   	//! holds the index of the last event read
  mEventCounter	=0;   	//! a counter holding the total # of events read

  mSplit	=99;          
  mCompression	=9;    
  mBufferSize	=65536*4;  
  mMuSave	=0;        
  mBadInFile	=0;       
  mIoMode       =0;
  mCloseWrite	=1;       




}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StMuIOMaker::~StMuIOMaker() {
  DEBUGMESSAGE("");
  if(mMuSave && !mBadInFile && !mCloseWrite) closeMuWrite();
  DEBUGMESSAGE3("out");
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
   The Init() routine is used to establish contact to other makers. As it is STAR 
   habit (but really really bad coding) we identify the 
   other makers by names (instead of passing pointers). Here, the names are 
   hard-wired because they have to be identical to the names 
   the bfc is assining to the makers. Do not alter these names unless you know 
   what you are doing.
*/
int StMuIOMaker::Init(){
  DEBUGMESSAGE("");
  TDataSet *muDstSet =  AddObj(mStMuDst,".const");   ///< added for Valeri to be able to pick it up in other makers 
  if (muDstSet ) muDstSet ->SetName("MuDst");          ///< added for Valeri to be able to pick it up in other makers 

// borrow StIOMaker::TString TreeName to pass a save flag for MuDst  
  if(fTreeName == "MuSave") mMuSave = true;

  DEBUGVALUE(mMuSave);
  DEBUGMESSAGE2("out");
  return 0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::Make(){
// same as "Read mode" of StTreeMaker
    int iret=0,ntry=13;
    while(1999) {
      iret = MakeRead();
      if (iret!=kStErr && ntry--) break;
      Warning("Make","%d *** ReadError ***\n",ntry);
    }
    return iret;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
/**
	This is the function that actually reads a new event. All other 
	implementations of Make will eventually call this function.
	@param  the index of the event to read
	@return kStEOF if the requested index is greater that the number of events in the chain
	@return kStErr if the requested index does not exists
        @return kStWarn if there is a problem with reading the tree (e.g. file corrupt)
	@return kStOk if the requested event was read successfully 
	If for any reason no event is read, the pointers to the TClonesArrays 
	are set to 0. It will cause a crash if you will try to access 
	information from the pointer to the StMuDst.  This is done for ,
	so that nobody reads the same (last) event over and over again without 
	noticing. Please check the return values of Make(), only kStOk means a 
	an event was read succesfull and data can be extracted.
 */
int StMuIOMaker::Make(int index){
  DEBUGMESSAGE("");
// as default clear(0)
  //clear();
  DEBUGVALUE3(index);
  mCurrentIndex = index;
  DEBUGVALUE3(mNumberOfEvents);
  if ( mCurrentIndex >= mNumberOfEvents) return kStEOF;
  if ( mCurrentIndex < 0 ) return kStErr;
  if (!mChain) return kStEOF;
  int bytes = mChain->GetEntry(mCurrentIndex);
  if (bytes <= 0) {
     LOG_WARN << "#### " << fFile.Data() << " could not read event " << index 
              << ", TTree::GetEntry returned " << bytes << endm;
     return kStWarn;
  }

  DEBUGVALUE(mMuSave);
  if(mMuSave) { mOutTree->Fill(); THack::IsTreeWritable(mOutTree); }
// One event would be skipped in StChain if making increment here!!!!
//  mCurrentIndex++;
  DEBUGVALUE3(bytes);
  mStMuDst->set(this);
  mEventCounter++;
  fillHddr();
  mStMuDst->collectVertexTracks();
  return kStOk;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::Make(int major, int minor){
  DEBUGMESSAGE("");
  DEBUGVALUE3(major);
  DEBUGVALUE3(minor);
  int index = mChain->GetEntryNumberWithIndex(major,minor);
  DEBUGMESSAGE("out");
  return Make(index);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::Make(const StUKey& key){
  DEBUGMESSAGE("");
  int index = mChain->GetEntryNumberWithIndex(key.GetRunId(),key.GetEventId());
  DEBUGVALUE3(index);
  DEBUGMESSAGE("out");
  return Make(index);
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::Finish() {
  DEBUGMESSAGE("");
  LOG_INFO << "###### StMuIOMaker read " << mEventCounter << " events"  << endm;
  if(mMuSave && !mBadInFile && !mCloseWrite) { 
    if (mOutTree) mOutTree->AutoSave(); 
    closeMuWrite();
  }
  DEBUGMESSAGE3("out");
  return kStOK;
}

// implementaion of virtual methods of base StIOInterFace
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
Int_t StMuIOMaker:: MakeRead(const StUKey &key) {
    DEBUGMESSAGE("");
    if(!mBadInFile) {
      if(key.IsNull()) { // sequential reading in normal case
         DEBUGMESSAGE("Seq");
         return Make(mCurrentIndex+1);
      }
      else  {            // GC
         DEBUGMESSAGE("GC");
         return Make(key);
      }
    }
    else {               // Bad input file
      DEBUGMESSAGE("BadFile");
      return kStEOF;
    }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// Open() in BASE StIOInterace returns 1999 if not implemented in DERIVED here
Int_t StMuIOMaker::Open(const char*) { 
  DEBUGMESSAGE("");
  mBadInFile = false;
  mCloseWrite = true;
  int iret = openRead();
  if(mMuSave && !mBadInFile) { 
    openMuWrite();
    mCloseWrite = false;
  }
  return iret; 
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
// Close() in BASE StIOInterace will quit if not implemented in DERIVED here
void StMuIOMaker::Close(Option_t *) { 
  DEBUGMESSAGE("");
  if(mMuSave && !mBadInFile && !mCloseWrite) closeMuWrite(); 
  closeRead(); 
  DEBUGMESSAGE3("out");
}

// private methods
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StMuIOMaker::openRead() {
  DEBUGMESSAGE("");
  DEBUGVALUE(fFile.Data());
  mChain = new TChain("MuDst");  

  mChain->Add(fFile.Data());
  DEBUGVALUE3(mChain);

  mNumberOfEvents = (int)mChain->GetEntries();

  if(mNumberOfEvents == 1234567890) {
     LOG_WARN << "#### " << fFile.Data() << " Corrupted!" << endm;
     mBadInFile = true;
     closeRead();
     return kStOK;
  }

  StTimer timer;
  timer.reset();
  timer.start();

  setBranchAddresses(mChain);
  mChain->BuildIndex("MuEvent.mEventInfo.mRunId","MuEvent.mEventInfo.mId");
  timer.stop();
  LOG_INFO << " Index of " << mNumberOfEvents << " events buit in " 
           << timer.elapsedTime() << " seconds " << endm;


  mCurrentIndex = -1;
  DEBUGMESSAGE3("out");
  return kStOK;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::openMuWrite() {
  DEBUGMESSAGE("");
  mOutFileName = fFile.Data(); 
  while (mOutFileName.find("/")!=string::npos) {
      int pos = mOutFileName.find("/");
      mOutFileName.erase(0,pos+1);
  }
  int pos = mOutFileName.find("MuDst");
  mOutFileName = mOutFileName.insert(pos,"Sel.");
  DEBUGVALUE(mOutFileName.c_str());

  mOutFile = new TFile(mOutFileName.c_str(),"RECREATE","StMuDst");
  if (mOutFile->IsZombie()) {
      LOG_FATAL << "StMuIOMaker::openMuWrite" << " Can not create TFile object for "
                 << mOutFileName.c_str()  << " file" << endm;
  }
     
  mOutFile->SetCompressionLevel(mCompression);

  TTree *tree = mChain->GetTree();
  mOutTree = new TTree("MuDst", "StMuDst", mSplit);
#if ROOT_VERSION_CODE < ROOT_VERSION(5,26,0)
  Long64_t MAXLONG=100000000000LL; // 100 GB
  LOG_INFO << "Tree size MAX will be " << (float) MAXLONG/1000/1000/1000 << " GB " << endm;
  mOutTree->SetMaxTreeSize(MAXLONG); // limited to 1.9 GB  - set to maximum
#endif
  mOutTree = tree->CloneTree(0);

  DEBUGMESSAGE3("out");
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StMuIOMaker::closeMuWrite(){
  DEBUGMESSAGE("");
  LOG_INFO << __PRETTY_FUNCTION__ << endm;
  if (mOutTree && mOutFile) {
      LOG_INFO << " ##### " << __PRETTY_FUNCTION__ << " "
               << " ##### " << endm;
      LOG_INFO << " ##### File=" << mOutFile->GetName() << " " 
               << " ##### " << endm;
      LOG_INFO << " ##### NumberOfEvents= " << mOutTree->GetEntries() << " "
               << " ##### " << endm;
  }
  StTimer timer;
  timer.reset();
  timer.start();

  if(mOutTree) mOutTree->Write();
  mOutTree = 0;
  if(mOutFile) mOutFile->Close();
  mOutFile = 0;

  timer.stop();
  LOG_INFO << " Writing took " << timer.elapsedTime() << " seconds " << endm;
  mCloseWrite = true;
  DEBUGMESSAGE3("out");
}
/***************************************************************************
 *
 * $Log: StMuIOMaker.cxx,v $
 * Revision 1.21  2011/08/18 18:41:36  fisyak
 * set max. tree size = 100 GB
 *
 * Revision 1.20  2011/04/19 22:50:08  fisyak
 * Use default size of TTree (100 GB) for ROOT >= 5.26.0
 *
 * Revision 1.19  2009/05/22 23:48:18  fine
 * Test I/O errors after filling the TTree
 *
 * Revision 1.18  2009/05/22 22:25:31  fine
 * Add the Zombue test for TFile ctors
 *
 * Revision 1.17  2009/04/28 22:15:10  perev
 * CleanUp
 *
 * Revision 1.16  2009/03/10 23:43:53  jeromel
 * Set tree size to max size
 *
 * Revision 1.15  2007/08/31 01:55:14  mvl
 * Added protection against corrupted files by checking for return code -1 from TTree:GetEntry(). StMuDstMaker will silently skip these events; StMuIOMaker returns kStWarn.
 *
 * Revision 1.14  2007/05/16 18:50:48  mvl
 * Cleanup of output. Replaced cout with LOG_INFO etc.
 *
 * Revision 1.13  2005/08/19 19:46:05  mvl
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
 * Revision 1.12  2004/10/19 01:46:46  mvl
 * Removed call to StMuDstMaker::clear() in Make() (Obsolete)
 *
 * Revision 1.11  2004/07/02 01:51:09  perev
 * Wei-Ming Zhang developments
 *
 * Revision 1.10  2004/04/20 18:49:16  perev
 * Big reorganization, now StMuIOMkaer inherits from StMuDstMaker
 *
 * Revision 1.5  2004/04/02 03:24:54  jeromel
 * Changes implements PMD and TOF.  TOF is clearly incomplete.
 *
 * Revision 1.4  2004/02/17 04:56:36  jeromel
 * Extended help, added crs support, restored __GNUC__ for PRETTY_FUNCTION(checked once
 * more and yes, it is ONLY defined in GCC and so is __FUCTION__),  use of a consistent
 * internal __PRETTYF__, return NULL if no case selected (+message) and protected against
 * NULL mChain.
 *
 * Revision 1.3  2003/09/11 05:49:20  perev
 * ansi corrs
 *
 * Revision 1.2  2003/09/09 18:16:53  laue
 * StMuIOMaker: embedded documentation added
 * StMuTimer: name of define changed (was same as StTimer)
 *
 **************************************************************************/



