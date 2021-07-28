#include "StHbtMaker/Reader/StHbtTagReader.h"
#ifdef HPUX
#include <Stiostream.h>
#include <string.h>
#else
#include "Stiostream.h"
#include <string>
#endif

#include "TFile.h"
#include "TTree.h"

ClassImp(StHbtTagReader)

StHbtTagReader::StHbtTagReader(const char* tagFileName) :  mTagFileName(""), mDstFileName(""), mTFile(0), mTTree(0) {
  cout << " StHbtTagReader::StHbtTagReader(const char* tagFileName)" << endl;
  mTagFileName = string(tagFileName);
  init();
}

StHbtTagReader::StHbtTagReader(const StIOMaker* ioMaker) : mTagFileName(""), mDstFileName(""), mTFile(0), mTTree(0) {
  cout << " StHbtTagReader::StHbtTagReader(const StIOMaker* ioMaker)" << endl;
  mIOMaker = ioMaker;
}

void StHbtTagReader::init() {
  cout << " StHbtTagReader::init()" << endl;
  if (mTFile) {
      if ( mTFile->IsOpen() ) {
	  cout << " StHbtTagReader::init() - closing file  " << endl;
	  mTFile->Close(); 
      }
      delete  mTFile;
  }
  mTFile = new TFile(mTagFileName.c_str(),"r");
  if ( mTFile->IsOpen() ) {
      cout << " StHbtTagReader::init() - new tags file opend : -" << mTagFileName.c_str() << "-" << endl;
      mDstFileName = mIOMaker->GetFile();
      mTTree = (TTree*)mTFile->Get("Tag");
      mNEvents = (int)mTTree->GetEntries();
  }
  else {
      cout << " StHbtTagReader::init() - coundn't open new tags file : -" << mTagFileName.c_str() << "-" <<  endl;
  }
}

StHbtTagReader::~StHbtTagReader(){
  if (mTFile) mTFile->Close();
}

int StHbtTagReader::EventMatch(int runNumber, int eventNumber) {
  //cout << " StHbtTagReader::EventMatch(int runNumber, int eventNumber)" << endl;
  if (mIOMaker) {
      cout << " mIOMaker " << mIOMaker << " " << mDstFileName.c_str() << " " << mIOMaker->GetFile() << endl;
      if (strcmp(mDstFileName.c_str(),mIOMaker->GetFile())  ) {// file names doesn't match
	  cout <<  " new file " << endl;
	  mTagFileName = mIOMaker->GetFile();
	  if (mTagFileName.find(".dst.") != string::npos) 
	    mTagFileName.replace(mTagFileName.find(".dst."),5,".tags.");
	  else if (mTagFileName.find(".event.") != string::npos) 
	    mTagFileName.replace(mTagFileName.find(".event."),7,".tags.");
	  init();
      }
  }
  return (GetEventIndex(runNumber,eventNumber)!=-1) ? 1 : 0;
}

int StHbtTagReader::GetEventIndex(int runNumber, int eventNumber){
  cout << " StHbtTagReader::GetEventIndex(int runNumber, int eventNumber)" << endl;
  mRunNumber = -1;
  mEventNumber = -1;
  mEventIndex  = -1;
  for ( int i=0; i<mNEvents; i++) { 
    mIret = mTTree->GetEntry(i);
    if ( mTTree->GetBranch("EvtHddr.mEventNumber")->GetLeaf("mEventNumber")->GetValue() == eventNumber &&
	 mTTree->GetBranch("EvtHddr.mRunNumber")->GetLeaf("mRunNumber")->GetValue() == runNumber            ) {
      mEventIndex = i;
      mRunNumber = (int)mTTree->GetBranch("EvtHddr.mRunNumber")->GetLeaf("mRunNumber")->GetValue();
      mEventNumber = (int)mTTree->GetBranch("EvtHddr.mEventNumber")->GetLeaf("mEventNumber")->GetValue();
      break;
    }
  }
  return mEventIndex;
}


