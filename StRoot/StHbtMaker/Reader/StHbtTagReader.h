#ifndef StHbtTagReader_h
#define StHbtTagReader_h

#include "StMaker.h"
#include "StChain.h"
#include "StIOMaker/StIOMaker.h"

#include "TTree.h"
#include "TBranch.h"
#include "TLeaf.h"

#ifdef __CC5__
  #include <Stiostream.h>
#endif


#include <string>
#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif 

class StHbtTagReader {
 protected:
  const StIOMaker* mIOMaker;
  string mTagFileName;
  string mDstFileName;
  TFile* mTFile;
  TTree* mTTree;

  int mIret;
  int mNEvents;
  int mEventIndex;
  int mRunNumber;
  int mEventNumber;
  // pointers to other makers

  int GetEventIndex(int runNumber, int eventNumber);
  void init();
 public:
    StHbtTagReader(const char* tagFileName);
    StHbtTagReader(const StIOMaker* ioMaker);

    virtual ~StHbtTagReader();


    // is there a tag matching the current event and run-number ?
    int EventMatch(int runNumber, int eventNumber);
    double tag(char* name, unsigned int i=0) {
      if ( mTTree->GetLeaf(name) ) 
	if ( (unsigned int)mTTree->GetLeaf(name)->GetNdata() > i )
	  return (double)mTTree->GetLeaf(name)->GetValue(i);
      return -9999;
    }
    int intTag(char* name, unsigned int i=0) {
      if ( mTTree->GetLeaf(name) && strcmp(mTTree->GetLeaf(name)->GetTypeName(),"Int_t") )
	if ( (unsigned int)mTTree->GetLeaf(name)->GetNdata() > i )
	  return (int)mTTree->GetLeaf(name)->GetValue(i);
      return -9999;
    }
    float floatTag(char* name, unsigned int i=0) {
      if ( mTTree->GetLeaf(name) && strcmp(mTTree->GetLeaf(name)->GetTypeName(),"Float_t") ) 
	if ( (unsigned int)mTTree->GetLeaf(name)->GetNdata() > i )
	  return (float)mTTree->GetLeaf(name)->GetValue(i);
      return -9999.;
    }

    
#ifdef __ROOT__  
      ClassDef(StHbtTagReader,0)
#endif
};


#endif
