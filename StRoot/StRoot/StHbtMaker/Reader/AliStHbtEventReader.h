#ifndef AliStHbtEventReader_hh
#define AliStHbtEventReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StHbtMaker/Infrastructure/StHbtEnumeration.hh"

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StEvent/StEnumerations.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"



#include <string>
#include <stdlib.h>
//#include <iterator.h>
//#include <algo.h>

class StHbtEvent;
//class Event;
class StIOMaker;
class TFile; 
class TTree;
class TChain;
class AliStHbtEvent;
class AliStHbtTrack;

class AliStHbtEventReader : public StHbtEventReader {

private:
  StIOMaker* mIOMaker;

  StHbtIOMode mIOMode;
  string mCurrentFileName;
  string mDir;
  string mFile;
  string mFilter;

  int mMaxFiles;

  int mDebug;
  TChain*            mTChain; 
  TFile*             mCurrentFile;
  TTree*             mTTree;

  string             mInputDir; 
  AliStHbtEvent*     mEvent; 
  StHbtEvent*        mHbtEvent; 

  unsigned int       mEventIndex;

  StHbtEvent* read();
  int initRead(string dir, string file, string filter, int mMaxFiles);
  int uninitRead();

  int fillChain(TChain* chain, const char* dir, const char* filter, const int maxFiles);
  int fillChain(TChain* chain, const char* list, const int maxFiles);

 protected:
  
  public:
  AliStHbtEventReader(StHbtIOMode mode, StIOMaker* io, 
		   const char* dirName, const char* fileName, 
		   const char* filter=".", int maxFiles=999);
 
 ~AliStHbtEventReader();
  
  StHbtEvent* ReturnHbtEvent();

  StHbtString Report();
  
  void SetDebug(int);


  ClassDef(AliStHbtEventReader, 1)
};

inline void AliStHbtEventReader::SetDebug(int debug) {mDebug=debug;}
  
#endif

