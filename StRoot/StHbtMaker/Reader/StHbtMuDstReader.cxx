/***************************************************************************
 *
 * $Id: StHbtMuDstReader.cxx,v 1.10 2005/07/18 16:33:20 kopytin Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 **************************************************************************/
#include "StChain.h"

#include "StEvent/StEventTypes.h"

#include "StEventUtilities/StuRefMult.hh"
#include "StEventUtilities/StuProbabilityPidAlgorithm.h"

#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StTimer.hh"

#include "StIOMaker/StIOMaker.h"

#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"
#include "StStrangeMuDstMaker/StStrangeEvMuDst.hh"
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StV0Mc.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StXiMc.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMc.hh"

#include "StMuDSTMaker/COMMON/StMuException.hh"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuCut.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"

#include "StHbtMuDstReader.h"
#include "Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"

#include "TFile.h"
#include "TTree.h"
#include "TClass.h"
#include "TChain.h"
#include "TStreamerInfo.h"
#include "TClonesArray.h"

ClassImp(StHbtMuDstReader)

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtMuDstReader::StHbtMuDstReader(int mode, int nameMode, const char* dirName, const char* fileName, const char* filter, int maxFiles) : 
  mStEvent(0), mStStrangeMuDstMaker(0), mIOMaker(0),
  mIoMode((ioMode)mode), mIoNameMode((ioNameMode)nameMode),
  mDirName(dirName), mFileName(fileName), mFilter(filter), mMaxFiles(maxFiles),
  mTrackType(primary), mReadTracks(1), 
  mReadV0s(1), mReadXis(1), mReadKinks(1), mFinish(0),
  mSplit(99), mCompress(9), mBufferSize(65536*4), mHbtEvent(0)
{
  mReaderStatus = 0;
  mEventCounter=0;
  mStMuDst = new StMuDst();
  /// from muDst
  for ( int i=0; i<__NARRAYS__; i++) {
    arrays[i] = 0;
    mArrays[i]= clonesArray(arrays[i],StMuArrays::arrayTypes[i],StMuArrays::arraySizes[i],StMuArrays::arrayCounters[i]);
  }
  /// from strangeness group
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    strangeArrays[i] = 0;
    mStrangeArrays[i]= clonesArray(strangeArrays[i],StMuArrays::strangeArrayTypes[i],StMuArrays::strangeArraySizes[i],StMuArrays::strangeArrayCounters[i]);
  }

  mStMuDst->set(arrays,strangeArrays);

  if (mIoMode==ioRead) openRead();
  if (mIoMode==ioWrite) mProbabilityPidAlgorithm = new StuProbabilityPidAlgorithm();

}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtMuDstReader::~StHbtMuDstReader(){
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::clear(){
  DEBUGMESSAGE1("");

  //  if (mHbtEvent) { delete mHbtEvent; mHbtEvent=0;}
  /// from muDst
  for ( int i=0; i<__NARRAYS__; i++) {
    clear(mArrays[i],StMuArrays::arrayCounters[i]);
  }
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    clear(mStrangeArrays[i],StMuArrays::strangeArrayCounters[i]);
  }


}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::clear(TClonesArray* t, int& counter){
  if (t) t->Clear(""); counter=0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
TClonesArray* StHbtMuDstReader::clonesArray(TClonesArray* p, const char* type, int size, int& counter) {
  DEBUGMESSAGE1("");
  if (!p) {
    DEBUGVALUE2(type);
    p = new TClonesArray(type, size);
    counter=0;
  }
  if (!p) throw StMuExceptionNullPointer("could not create TClonesArray",__PRETTYF__);
  return p;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
int StHbtMuDstReader::Init(){
  DEBUGMESSAGE1("");
//   mIOMaker = (StIOMaker*)GetMaker("IOMaker");
//   mStStrangeMuDstMaker = (StStrangeMuDstMaker*)GetMaker("StrangeMaker");
  return 0;
}

//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::Clear(){
  DEBUGMESSAGE1("");
  clear();
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StHbtEvent* StHbtMuDstReader::ReturnHbtEvent(){
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();
  clear();
  try {
    if (mIoMode == ioRead)  read();
  }
  catch(StMuExceptionEOF e) {
    e.print();
    mReaderStatus = 2;
    return 0;
  }
  catch(StMuException e) {
    e.print();
    mReaderStatus = 1;
    return 0;
  }
  DEBUGVALUE1(timer.elapsedTime());
  if (mStMuDst) {
    mStMuDst->fixTrackIndices();
    mHbtEvent = new StHbtEvent(mStMuDst, mTrackType);
  }
  if (mEventCut){
    if (!(mEventCut->Pass(mHbtEvent))){
	delete mHbtEvent;
	mHbtEvent = 0;
	return 0;
    }
  }
  return mHbtEvent;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fill(){
  DEBUGMESSAGE1("");
  //  mStEvent = (StEvent*) GetInputDS("StEvent");
  if (!mStEvent) {
    DEBUGMESSAGE1("no StEvent");
    return;
  }
  
  /// once per event the pid algorithm has to be set up
  /// we make it static for the StMuTrack, because all tracks use the same instance
  if (mProbabilityPidAlgorithm) delete mProbabilityPidAlgorithm;
  mProbabilityPidAlgorithm = new StuProbabilityPidAlgorithm(*mStEvent);
  StMuTrack::setProbabilityPidAlgorithm(mProbabilityPidAlgorithm);
  StMuTrack::setProbabilityPidCentrality(uncorrectedNumberOfNegativePrimaries(*mStEvent));


  try {
    fillTrees(mStEvent);
  }
  catch(StMuException e) {
    e.print();
    throw e;
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::write(){
  DEBUGMESSAGE1("");


  try {
    fill();
  }
  catch (StMuException e) {
    return;
  }

  // filename
  string ioMakerFileName;
  if (mIOMaker) {  // if the ioMaker is specified, we take the output filename from the ioMaker
    ioMakerFileName = string(mIOMaker->GetFile()); 
  }
  else { // if not then construct the filename
    ioMakerFileName = mDirName+mFileName;
  }
  DEBUGVALUE1(ioMakerFileName.c_str());


  string theFileName = buildFileName(mDirName,basename(ioMakerFileName),".MuDst.root");
  if (theFileName != mCurrentFileName) {
    closeWrite();
    openWrite(theFileName);
    mCurrentFileName = theFileName;
  }

  DEBUGMESSAGE2("now fill tree");
  mTTree->Fill();
  DEBUGMESSAGE2("tree filled");

  return;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::Finish() {
  if (mFinish) {
    for ( int i=0; i<10; i++) {
      cout << "why are you calling the Finish() again  ???????" << endl;
      cout << "are you the stupid chain destructor ???????????" << endl;
    }
  }
  else {
    if (mIoMode== ioWrite ) closeWrite();
    if (mIoMode== ioRead ) closeRead();
    mFinish = true;
  }
  return;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::openRead() {
  DEBUGVALUE1(mDirName.c_str());
  DEBUGVALUE1(mFileName.c_str());
  DEBUGVALUE1(mFilter.c_str());
 
  makeChain(mDirName.c_str(), mFilter.c_str(),mMaxFiles);

  // muDst stuff
  for ( int i=0; i<__NARRAYS__; i++) {
    mChain->SetBranchAddress(StMuArrays::arrayNames[i],&mArrays[i]);
  } 

  // strange stuff
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    mChain->SetBranchAddress(StMuArrays::strangeArrayNames[i],&mStrangeArrays[i]);
  } 
  
  (void*)mChain->GetBranch("");  /// this dummy call returns 0, but magically after calling it, I get a the tree in the next call
  mTTree = mChain->GetTree();

  mStMuDst->set(mArrays,mStrangeArrays);  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::read(){
  DEBUGMESSAGE1("");
    if ( !(mEventCounter<mChain->GetEntries()) ) throw StMuExceptionEOF("end of input",__PRETTYF__);
  mChain->GetEntry(mEventCounter);
  DEBUGVALUE2(mChain->GetCurrentFile()->GetName());
  mEventCounter++;
  
  return;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::closeRead(){
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::openWrite(string fileName) {
  DEBUGVALUE1(fileName.c_str());
  // creat a Picoevent and and output file
  DEBUGMESSAGE2("now create file");
  mCurrentFile = new TFile(fileName.c_str(),"RECREATE","StMuDst");
  
  if (!mCurrentFile) throw StMuExceptionNullPointer("no file openend",__PRETTYF__);

  mCurrentFile->SetCompressionLevel(mCompress);
  
  // Create a ROOT Tree and one superbranch
  DEBUGMESSAGE2("now create trees and branches");

  TBranch* branch;
  int bufsize = mBufferSize;
  if (mSplit) bufsize /= 4;

  //  muDst stuff
  mTTree = new TTree("MuDst", "StMuDst",mSplit);
  if (!mTTree) throw StMuExceptionNullPointer("can not create tree",__PRETTYF__);
  mTTree->SetAutoSave(1000000);  // autosave when 1 Mbyte written
    DEBUGMESSAGE("arrays");
  for ( int i=0; i<__NARRAYS__; i++) {
    DEBUGVALUE2(i);
    branch = mTTree->Branch(StMuArrays::arrayNames[i],&mArrays[i], bufsize, mSplit);
  }

  // strange stuff
    DEBUGMESSAGE("strange arrays");
  for ( int i=0; i<__NSTRANGEARRAYS__; i++) {
    DEBUGVALUE2(i);
    branch = mTTree->Branch(StMuArrays::strangeArrayNames[i],&mStrangeArrays[i], bufsize, mSplit);
  }
 
  mCurrentFileName = fileName;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::closeWrite(){
  if (mTTree) mTTree->AutoSave(); 
  if (mCurrentFile) mCurrentFile->Close();
  mTTree = 0;
  mCurrentFile = 0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillTrees(StEvent* ev, StMuCut* cut){
  DEBUGMESSAGE1("");
  
  try {
    fillEvent(ev);
    fillL3AlgorithmInfo(ev);
    fillDetectorStates(ev);
  }
  catch(StMuException e) {
    e.print();
  }
  
  try {
    fillTracks(ev,mTrackFilter);
  }
  catch(StMuException e) {
    e.print();
  }

  try {
    fillL3Tracks(ev, mL3TrackFilter);
  }
  catch(StMuException e) {
    e.print();
  }

  try {
    fillStrange(mStStrangeMuDstMaker);
  }
  catch(StMuException e) {
    e.print();
  }
}



//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillEvent(StEvent* ev, StMuCut* cut) {
  DEBUGMESSAGE1("");
  StMuEvent typeOfEvent;
  if (!ev) throw StMuExceptionNullPointer("no StEvent",__PRETTYF__);
  StTimer timer;
  timer.start();
  if (!cut || cut->pass(ev)) {
    DEBUGMESSAGE3("");
    addType(mArrays[muEvent],ev,typeOfEvent);
  }
  timer.stop();
  DEBUGVALUE2(timer.elapsedTime());
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillL3AlgorithmInfo(StEvent* ev) {
  DEBUGMESSAGE1("");
  if ( !ev->l3Trigger() ) return;
  if ( !ev->l3Trigger()->l3EventSummary()) return;

  StTimer timer;
  timer.start();
  StL3EventSummary* l3 = ev->l3Trigger()->l3EventSummary();
  int n = l3->numberOfAlgorithms();
  for (int i=0; i<n; i++) {
    if (l3->algorithms()[i]->accept()) 
      addType( mArrays[muAccept], *l3->algorithms()[i] );
    else
      addType( mArrays[muReject], *l3->algorithms()[i] );
  }
  timer.stop();
  DEBUGVALUE2(timer.elapsedTime());
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillTracks(StEvent* ev, StMuCut* cut) {
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();

  StSPtrVecTrackNode& nodes= ev->trackNodes();
  DEBUGVALUE2(nodes.size());
  for (StSPtrVecTrackNodeConstIterator iter=nodes.begin(); iter!=nodes.end(); iter++) {
    addTrackNode(ev, *iter, cut, mArrays[muGlobal], mArrays[muPrimary], mArrays[muOther], false);
  }
  timer.stop();
  DEBUGVALUE2(timer.elapsedTime());
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillL3Tracks(StEvent* ev, StMuCut* cut) {
  DEBUGMESSAGE1("");
  if (!ev->l3Trigger()) return;

  StTimer timer;
  timer.start();
  StSPtrVecTrackNode& nodes= ev->l3Trigger()->trackNodes();
  DEBUGVALUE2(nodes.size());
  for (StSPtrVecTrackNodeConstIterator iter=nodes.begin(); iter!=nodes.end(); iter++) {
    addTrackNode(ev, *iter, cut, mArrays[muL3], 0, 0, true );
  }
  timer.stop();
  DEBUGVALUE2(timer.elapsedTime());
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillDetectorStates(StEvent* ev) {
  DEBUGMESSAGE1("");
  StTimer timer;
  timer.start();
  for (int i=0; i<StMuArrays::arraySizes[muState]; i++) {
    StDetectorState* state = ev->detectorState((StDetectorId) i);
    if (state)
      addType( mArrays[muState], ev->detectorState((StDetectorId)i) );
  }
  timer.stop();
  DEBUGVALUE2(timer.elapsedTime());
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::addTrackNode(const StEvent* ev, const StTrackNode* node, StMuCut* cut, 
				  TClonesArray* gTCA, TClonesArray* pTCA, TClonesArray* oTCA, bool l3) {
  DEBUGMESSAGE3("");
  const StTrack* tr=0;

  /// do global track
  int index2Global =-1;
  if (gTCA) {
    tr= node->track(global);
    if (tr ) index2Global = addTrack(gTCA, ev, tr, cut, -1, l3);
  }
  /// do primary track track
  int index;
  if (pTCA) {
    tr = node->track(primary);
    if (tr) index = addTrack(pTCA, ev, tr, cut, index2Global, l3);
  }
  /// all other tracks
  if (oTCA) {
    size_t nEntries = node->entries();
    for (size_t j=0; j<nEntries; j++) { /// loop over all tracks in tracknode
      tr = node->track(j);
      if (tr && (tr->type()!=global) && (tr->type()!=primary) ) { /// exclude global and primary tracks
	index = addTrack(oTCA, ev, tr, cut, index2Global, l3);
      }
    }
  }
}
//---------------------------------------------------------------------
//---------------------------------------------------------------------
//---------------------------------------------------------------------
int StHbtMuDstReader::addTrack(TClonesArray* tca, const StEvent* event, const StTrack* track, StMuCut* cut, int index2Global, bool l3) {
  DEBUGMESSAGE3("");
  StRichSpectra typeOfStRichSpectra;
  int index = -1;
  int index2RichSpectra=-1;
  /// if (!tca || !track) return index; /// I made sure that the array anf the track is there
  int counter = tca->GetEntries();
  try{
    if (cut && !cut->pass(track)) throw StMuExceptionBadValue("failed track cut",__PRETTYF__);
    // add StRichSpectra if StRichPidTraits are found 
    // we have to do this more elegant
    StRichSpectra* rich = richSpectra(track);
    if (rich) {
      index2RichSpectra  =  addType( mArrays[muRich], *rich );
    }
    new((*tca)[counter]) StMuTrack(event, track, 0, index2Global, index2RichSpectra, l3);
    index = counter;
  }
  catch (StMuException e) {
    IFDEBUG3(e.print());
  }
  return index;  /// return index to self if newly created, else return -1;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
StRichSpectra* StHbtMuDstReader::richSpectra(const StTrack* track) {
  DEBUGMESSAGE3("");
  const StPtrVecTrackPidTraits& traits = track->pidTraits(kRichId);
  for (StPtrVecTrackPidTraitsConstIterator traitIter=traits.begin();traitIter!=traits.end();++traitIter) {
    StRichPidTraits* pid = dynamic_cast<StRichPidTraits*>(*traitIter);
    if (pid) return pid->getRichSpectra();
  }
  return 0;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::fillStrange(StStrangeMuDstMaker* maker) {
  DEBUGMESSAGE2("");
  /// now fill the strangeness stuff
  if (!maker) throw StMuExceptionNullPointer("no StrangeMuDstMaker",__PRETTYF__);
 
  StStrangeEvMuDst ev;
  StV0MuDst v0;     
  StStrangeAssoc assoc;
  StXiMuDst xi;     
  StKinkMuDst kink; 
  StV0Mc v0Mc;      
  StXiMc xiMc;      
  StKinkMc kinkMc;  

  addType(maker->GetEvClonesArray(),  mStrangeArrays[0],ev);
  addType(maker->GetEvMcArray(),      mStrangeArrays[1],ev);

  addType(maker->GetV0ClonesArray(),  mStrangeArrays[2],v0);
  addType(maker->GetV0McArray(),      mStrangeArrays[3],v0Mc);
  addType(maker->GetV0AssocArray(),   mStrangeArrays[4],assoc);

  addType(maker->GetXiClonesArray(),  mStrangeArrays[5],xi);
  addType(maker->GetXiMcArray(),      mStrangeArrays[6],xiMc);
  addType(maker->GetXiAssocArray(),   mStrangeArrays[7],assoc);

  addType(maker->GetKinkClonesArray(),mStrangeArrays[8],kink);
  addType(maker->GetKinkMcArray(),    mStrangeArrays[9],kinkMc);
  addType(maker->GetKinkAssocArray(), mStrangeArrays[10],assoc);
  
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
template <class T>
void StHbtMuDstReader::addType(TClonesArray* tcaFrom, TClonesArray* tcaTo , T t) {
  if (tcaFrom && tcaTo) {
    int n = tcaFrom->GetEntries();
    int counter = tcaTo->GetEntries();
    for (int i=0; i<n;i++) {
      new((*tcaTo)[counter++]) T( (T&)*tcaFrom->UncheckedAt(i) );
    }
  }
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
template <class T>
int StHbtMuDstReader::addType(TClonesArray* tcaTo , T t) {
  int counter =-1;
  if (tcaTo) {
    counter = tcaTo->GetEntries();
    new((*tcaTo)[counter]) T( t );
  }
  return counter;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
template <class T, class U>
int StHbtMuDstReader::addType(TClonesArray* tcaTo , U u, T t) {
  int counter =-1;
  if (tcaTo) {
    counter = tcaTo->GetEntries();
    DEBUGMESSAGE("");
    new((*tcaTo)[counter]) T(u);
  }
  return counter;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
string StHbtMuDstReader::buildFileName(string dir, string fileName, string extention){
  DEBUGMESSAGE1("");
  fileName = dir + "/" + fileName + extention;
  return fileName;
}
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
string StHbtMuDstReader::basename(string s){
  string name(s);
  while ( name.find("/") != string::npos ) {
    string::size_type pos =  name.find("/");
    name.erase(0, pos+1 );
  }
  string::size_type pos =  name.find(".event.root");
  if (pos != string::npos) name.erase(pos,pos+11);
  return name;
}  
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
//-----------------------------------------------------------------------
void StHbtMuDstReader::makeChain(const char* dir, const char* filter, int maxFiles) {
  DEBUGMESSAGE1("");
  mChain = new TChain("MuDst");
  //  TChain* mChain2 = new TChain("StrangeMuDst");
  // read directory
  // now find the files that end in the specified extention
  int fileCount(0);
  if(strncmp(dir+strlen(dir)-4,".lis",4)==0){
    ifstream tF(dir);
    char tFileName[500];
    int tNFile =0;
    tF >> tFileName;
    while ((!tF.eof()) && (tNFile<=maxFiles)){
      cout << "Add file " << tFileName << endl;
      mChain->Add(tFileName);
      tNFile++;
      tF >> tFileName;
    }
    fileCount = tNFile;
  }
  else{
    const char* fileName(0);
    void *pDir = gSystem->OpenDirectory(dir);
    while((fileName = gSystem->GetDirEntry(pDir))){
      if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;
      if(strcmp(fileName,".event.root")==0 || strcmp(fileName,"..")==0) continue;
      //    if( !strcmp(fileName,".MuDst.root") ) continue;
      if(strstr(fileName,filter) && strstr(fileName,".MuDst.root") ){ // found a match
	char* fullFile = gSystem->ConcatFileName(dir,fileName);
	// add it to the chain
	cout << fileCount << " " << fullFile << endl;
	mChain->Add(fullFile);
	//  mChain2->Add(mChain2);
	delete fullFile;
	if(++fileCount >= maxFiles) break;
      }   
    }
  }
  DEBUGVALUE2(fileCount);
}

void StHbtMuDstReader::setProbabilityPidFile(const char* file) {
  if (mProbabilityPidAlgorithm)
    mProbabilityPidAlgorithm->readParametersFromFile(file);
}
/***************************************************************************
 *
 * $Log: StHbtMuDstReader.cxx,v $
 * Revision 1.10  2005/07/18 16:33:20  kopytin
 * Brought addTrack in corresponce with MvL's new StMuTrack constructor list of arguments
 *
 * Revision 1.9  2005/07/01 00:23:15  chajecki
 * fix of potential memory leak in ReturnHbtEvent method
 * added 'delete mHbtEvent' if this  method returns 0
 * (can happend if an even didn't pass an event cut)
 *
 * Revision 1.8  2004/10/12 13:52:46  kisiel
 * Properly hadle mReaderStatus to make frontLoadedEventCut work
 *
 * Revision 1.7  2004/02/24 19:45:34  magestro
 * reverted to 1.5 code
 *
 * Revision 1.5  2004/02/17 16:46:38  jeromel
 * PF to PRETTYF
 *
 * Revision 1.4  2002/12/13 11:27:58  kisiel
 * List files support for StHbtMuDstReader
 *
 * Revision 1.3  2002/05/24 16:08:03  rcwells
 * Added event cut to StHbtMuDstReader
 *
 * Revision 1.2  2002/03/22 14:17:49  laue
 * minor changes for RH 7.2
 *
 * Revision 1.1  2002/03/20 19:32:24  laue
 * new reader for common MuDsts
 *
 *
 **************************************************************************/


















