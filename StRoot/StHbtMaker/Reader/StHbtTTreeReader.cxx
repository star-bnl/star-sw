/***************************************************************************
 *
 * $Id: StHbtTTreeReader.cxx,v 1.8 2003/09/02 17:58:34 perev Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#include "StHbtMaker/Reader/StHbtTTreeReader.h"

#include "StChain.h"
#include "TChain.h"
#include "TFile.h"
#include "TTree.h"

#include "StIOMaker/StIOMaker.h"


#include "StEvent.h"
#include "StEventTypes.h"
#include <math.h>
#include <string>
#include <typeinfo>

#include "SystemOfUnits.h"   // has "tesla" in it
#include "StHbtMaker/Infrastructure/StExceptions.hh"
#include "StHbtMaker/Infrastructure/StHbtTrackCollection.hh"
#include "StHbtMaker/Infrastructure/StHbtV0Collection.hh"
#include "StHbtMaker/Infrastructure/StHbtXiCollection.hh"
#include "StHbtMaker/Reader/StHbtGstarTxtReader.h"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#include "StHbtMaker/Base/StHbtV0Cut.h"
#include "StHbtMaker/Base/StHbtXiCut.h"
#include "StHbtMaker/Base/StHbtKinkCut.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"  
#include "StStrangeMuDstMaker/StV0MuDst.hh"
#include "StStrangeMuDstMaker/StXiMuDst.hh"
#include "StStrangeMuDstMaker/StKinkMuDst.hh"

#include "StHbtMaker/Infrastructure/StHbtTTreeEvent.h"
#include "StarClassLibrary/StMemoryInfo.hh"

#ifdef __ROOT__
ClassImp(StHbtTTreeReader)
#endif

#if !(ST_NO_NAMESPACES)
  using namespace units;
#endif


//__________________
StHbtTTreeReader::StHbtTTreeReader(StHbtIOMode mode, StIOMaker* io, 
				   const char* dirName, const char* fileName, const char* extention, 
				   const char* filter, int maxFiles)
  : mIOMaker(io), mTrackType(primary), mIOMode(mode), mMaxFiles(maxFiles), mDebug(0), mCurrentFile(0), 
  mTTree(0), split(99), comp(2), bufsize(256000/4)  {
  if (mDebug) cout << "StHbtTTreeReader::StHbtTTreeReader(...)"<< endl;

  mDir = string(dirName);
  mFile = string(fileName);
  mExtention = string(extention);
  mFilter = string(filter);
  mReaderStatus = 0;  // "good"
  mHbtTTreeEvent = new StHbtTTreeEvent();
  if (mDebug) cout << "StHbtTTreeReader::StHbtTTreeReader(...) - leaving"<< endl;
}
//__________________
StHbtTTreeReader::~StHbtTTreeReader(){
  if (mIOMode==hbtWrite && mCurrentFile && mTTree) { mTTree->AutoSave(); mTTree=0;} 
  if (mCurrentFile) { mCurrentFile->Close(); delete mCurrentFile; mCurrentFile = 0;}
  if (mEventCut) delete mEventCut;
  if (mTrackCut) delete mTrackCut;
  if (mV0Cut) delete mV0Cut;
  if (mXiCut) delete mXiCut;
  if (mKinkCut) delete mKinkCut;


}
//__________________
StHbtString StHbtTTreeReader::Report(){
  StHbtString temp = "\n This is the StHbtTTreeReader\n";
  char ccc[100];
  sprintf(ccc," Track type is %d\n",mTrackType);
  temp += ccc;
  temp += "---> EventCuts in Reader: ";
  if (mEventCut) {
    temp += mEventCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> TrackCuts in Reader: ";
  if (mTrackCut) {
    temp += mTrackCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> V0Cuts in Reader: ";
  if (mV0Cut) {
    temp += mV0Cut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> XiCuts in Reader: ";
  if (mXiCut) {
    temp += mXiCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n---> KinkCuts in Reader: ";
  if (mKinkCut) {
    temp += mKinkCut->Report();
  }
  else {
    temp += "NONE";
  }
  temp += "\n";
  return temp;
}
//__________________
StHbtEvent* StHbtTTreeReader::ReturnHbtEvent(){
  if (mDebug) cout << "StHbtTTreeReader::ReturnHbtEvent()" << endl;

  StHbtEvent* hbtEvent = 0;

  try {
    if (mIOMode == hbtRead) hbtEvent = read();
  }
  catch(StExceptionEOF e) {
    e.print();
    mReaderStatus = 2;
    return 0;
  }
  catch(StException e) {
    e.print();
    mReaderStatus = 1;
    return 0;
  }
    
  if (hbtEvent && mDebug>1) cout << *hbtEvent << endl;
  if (!hbtEvent) cout << "StHbtTTreeReader::ReturnHbtEvent() - no hbtEvent" << endl;
  if (mEventCut && hbtEvent ) {
    if ( mEventCut->Pass(hbtEvent)==0 ) {
      delete hbtEvent;
      hbtEvent=0;
    }
  }
  
  return hbtEvent;
}

//__________________
int StHbtTTreeReader::WriteHbtEvent(StHbtEvent* event){
  if (mDebug) cout << "StHbtTTreeReader::WriteHbtEvent()" << endl;

  StHbtEvent* hbtEvent = 0;

  try {
    if (mIOMode == hbtWrite) hbtEvent = write(event);
  }
  catch(StExceptionEOF e) {
    e.print();
    return 1;
  }
  catch(StException e) {
    e.print();
    return 0;
  }
    
  if (hbtEvent && mDebug>1) cout << *hbtEvent << endl;
  if (!hbtEvent) cout << "StHbtTTreeReader::WriteHbtEvent() - no hbtEvent" << endl;
  
  return 0;
}

//__________________
StHbtEvent* StHbtTTreeReader::write(StHbtEvent* event){
  /////////////////
  // get StEvent //
  /////////////////
  if (!event){
    if (mDebug) cout << "StHbtTTreeReader::write() - No StHbtEvent!!! " << endl;
    return 0;
  }


  string ioMakerFileName;
  if (mIOMaker) {  // if the ioMaker is specified, we take the output filename from the ioMaker
    ioMakerFileName = string(mIOMaker->GetFile()); 
  }
  else { // if not then construct the filename
    ioMakerFileName = mDir+mFile;
  }
  
  if (!mCurrentFile) initWrite(ioMakerFileName,mExtention);  // if no file open, open the file

  if (mCurrentFileName != ioMakerFileName) {  // if wrong file open, close file and open new file
    uninitWrite();
    initWrite(ioMakerFileName,mExtention);
  }

  if (!mCurrentFile) {
    if (mDebug) cout << "StHbtTTreeReader::write() - can not open file!!! " << endl;
    return 0;
  }

  if (mEventCut->Pass(event)) {
    try{
      mHbtTTreeEvent->clear();
      mHbtTTreeEvent->fill(event, mTrackCut, mV0Cut, mXiCut, mKinkCut);
      if (mDebug) cout << "StHbtTTreeReader::write() - now fill the tree " << endl;
      mTTree->Fill();
    }
    catch(StException e) {
      mHbtTTreeEvent =0;
      e.print();
    }
  }
  if (mDebug) cout << "StHbtTTreeReader::write() - leaving " << endl;
  return event;
}

int StHbtTTreeReader::initWrite(string ioMakerFileName, string extention){
  if (split) bufsize /= 4;
  
  string fileName = ioMakerFileName;
  while ( fileName.find("/") != string::npos ) {
    string::size_type pos =  fileName.find("/");
    fileName.erase(0, pos+1 );
  }
  fileName = mDir +fileName + extention;
  if (mDebug) cout << "StHbtTTreeReader::initWrite(...) - new fileName: " << fileName.c_str() << endl; 
  // creat a Picoevent and and output file
  if (mDebug) cout << "StHbtTTreeReader::initWrite(...) - now create file " << endl; 
  mCurrentFile = new TFile(fileName.c_str(),"RECREATE","hbtTTreeMuDst");
  if (!mCurrentFile) {
    cout << "StHbtTTreeReader::initWrite(...) - Warning: no file opened = " << endl; 
    assert(0);
  }
  //mCurrentFile->SetFormat(1);
  mCurrentFile->SetCompressionLevel(comp);
  
  // Create a ROOT Tree and one superbranch
  if (mDebug) cout << "StHbtTTreeReader::initWrite(...) - now create tree " << endl; 
  mTTree = new TTree("Tree", "hbtTTree",split);
  if (!mTTree) {
    cout << "StHbtTTreeReader::initWrite(...) - Warning: No Tree" << endl;
    assert(0);
  }
  
  mTTree->SetAutoSave(1000000);  // autosave when 1 Mbyte written
  //mTTree->SetAutoSave(1000);  // autosave when 1 Mbyte written
  if (mDebug) cout << "StHbtTTreeReader::initWrite(...) - now create branch " << endl; 
  mTTree->Branch("mHbtTTreeEvent", "StHbtTTreeEvent", &mHbtTTreeEvent, bufsize, split);
  mCurrentFileName = ioMakerFileName;
  return 0;
}

int StHbtTTreeReader::uninitWrite(){
  mTTree->AutoSave();
  mCurrentFile->Close();
  if (mCurrentFile) delete mCurrentFile;
  if (mTTree) delete mTTree;
  mTTree = 0;
  mCurrentFile = 0;
  return 0;
}

StHbtEvent* StHbtTTreeReader::read(){
  if (!mTChain) {
    try {
      cout << initRead(mDir,mFile,mFilter,mExtention,mMaxFiles) << " files to analyse " << endl;
    }
    catch(StException e) {
      e.print();
      return 0;
    }
  }
 
  unsigned int nEvents = (unsigned int)mTChain->GetEntries();
  if (!nEvents) throw StException("StHbtTTreeReader::read() - no events to read ");
  mHbtTTreeEvent->clear();
  int iBytes= mTChain->GetEntry(mEventIndex++);
  if (mDebug) cout << "StHbtTTreeReader::read() - bytes read :" << iBytes << endl;
  if (nEvents<mEventIndex) throw StExceptionEOF("StHbtTTreeReader::read()");
  if (!iBytes) throw StException("StHbtTTreeReader::read() - no event ");
  StHbtEvent* hbtEvent = new StHbtEvent(mHbtTTreeEvent); // ok, here we got event read from the file
  // now we should use the copy constructor for StHbtEvent in order to appy the cuts
  return hbtEvent; 
}

int StHbtTTreeReader::initRead(string dir, string file, string filter, string extention, int mMaxFiles){
  mEventIndex =0;
  mTChain = new TChain("Tree","hbtTree");
  int nFiles =0;
  if (file!="") { // if a filename was given
    if( strstr(file.c_str(),".lis") ) { // if a file list is specified
      try {
	nFiles = fillChain(mTChain, (dir+file).c_str(), mMaxFiles);
      }
      catch(StException e) {
	throw e;
      }
    }
    else { // a single file was specified
      mTChain->Add((dir+file).c_str());
      nFiles++;
    }
  }
  else {
    try {
      nFiles = fillChain(mTChain,dir.c_str(), filter.c_str(), extention.c_str(), mMaxFiles);
    }
    catch(StException e) {
      throw e;
    }
  }  mTChain->SetBranchAddress("mHbtTTreeEvent",&mHbtTTreeEvent);  // set the address where to read the event object
  return nFiles;
}

int StHbtTTreeReader::uninitRead(){
  if (mHbtTTreeEvent) delete mHbtTTreeEvent;
  if (mTChain) delete mTChain;
  mHbtTTreeEvent = 0;
  mTChain = 0;
  return 0;
}

int StHbtTTreeReader::fillChain(TChain* chain, const char* fileList, const int maxFiles) {
  ifstream* inputStream = new ifstream;
  inputStream->open(fileList);
  if (!(inputStream)) throw StException("StHbtTTreeReader::fillChain(string dir) - can not open directory");
  char* temp;
  int count=0;
  if (mDebug>1) cout << " StHbtTTreeReader::fillChain(...)- inputStream->good() : " << inputStream->good() << endl;
  for (;inputStream->good();) {
    temp = new char[200];
    inputStream->getline(temp,200);
    if (mDebug) cout << temp << endl;
    chain->Add(temp);
    delete temp;
    ++count;
    if (count>maxFiles) break;
  }   
  delete inputStream;
  if (mDebug) cout << "StHbtTTreeReader::(string dir)(string dir) - Added " << count << " files to the chain" << endl;
  return count;
}

int StHbtTTreeReader::fillChain(TChain* chain, const char* dir, const char* filter, const char* extention, const int maxFiles) {
  // read directory
  void *pDir = gSystem->OpenDirectory(dir);
  if(!pDir) throw StException("StHbtTTreeReader::fillChain(string dir) - can not open directory");
  // now find the files that end in the specified searchString
  const char* fileName(0);
  int count(0);
  while((fileName = gSystem->GetDirEntry(pDir))){
    if(strcmp(fileName,".")==0 || strcmp(fileName,"..")==0) continue;
    if(strstr(fileName,filter) && strstr(fileName,extention)) { // found a match
      char* fullFile = gSystem->ConcatFileName(dir,fileName);
      // add it to the chain
      cout << "StHbtTTreeReader::fillChain(string dir) - Adding " << fullFile << " to the chain" << endl;
      chain->Add(fullFile);
      delete fullFile;
      ++count;
      if (count>maxFiles) break;
    }   
  }
  cout << "StHbtTTreeReader::(string dir)(string dir) - Added " << count << " files to the chain" << endl;
  return count;
}


/***************************************************************************
 *
 * $Log: StHbtTTreeReader.cxx,v $
 * Revision 1.8  2003/09/02 17:58:34  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.7  2003/05/16 21:30:18  magestro
 * Removed obsolete include file
 *
 * Revision 1.6  2003/05/07 20:05:25  magestro
 * Removed StFlowTagMaker.h include
 *
 * Revision 1.5  2001/12/05 14:42:17  laue
 * updated for trigger(action)word and l3TriggerAlgorithm
 *
 * Revision 1.1  2001/06/21 19:18:42  laue
 * Modified Files: (to match the changed base classes)
 * 	StHbtAsciiReader.cxx StHbtAsciiReader.h
 * 	StHbtAssociationReader.cxx StHbtAssociationReader.h
 *  	StHbtBinaryReader.cxx StHbtBinaryReader.h
 *  	StHbtGstarTxtReader.cxx StHbtGstarTxtReader.h
 *  	StHbtStrangeMuDstEventReader.cxx
 *  	StHbtStrangeMuDstEventReader.h StStandardHbtEventReader.cxx
 * Added Files: new reader
 *  	StHbtTTreeReader.cxx StHbtTTreeReader.h
 *
 *
 **************************************************************************/



















