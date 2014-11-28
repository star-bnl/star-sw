/***************************************************************************
 *
 * $Id: StHbtTTreeReader.h,v 1.3 2001/12/05 14:42:17 laue Exp $
 *
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/

#ifndef StHbtTTreeReader_hh
#define StHbtTTreeReader_hh

#include "StHbtMaker/Base/StHbtEventReader.hh"
#include "StHbtMaker/Infrastructure/StHbtEnumeration.hh"

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StEvent/StEnumerations.h"
#include "StStrangeMuDstMaker/StStrangeMuDstMaker.h"


#include <string>

class StHbtEvent;
class StHbtTTreeEvent;
class StIOMaker;
class TFile; 
class TTree;
class TChain;

class StHbtTTreeReader : public StHbtEventReader {

private:
  StIOMaker* mIOMaker;

  StTrackType mTrackType;

  StHbtIOMode mIOMode;
  string mCurrentFileName;
  string mDir;
  string mFile;
  string mExtention;
  string mFilter;

  int mMaxFiles;

  int mDebug;
  TChain*            mTChain; //!
  TFile*             mCurrentFile; //!
  TTree*             mTTree; //!
  int split;       // by default split Event into sub branches
  int comp;        // by default file is compressed
  int bufsize;     // buffersize
  string             mInputDir; //!
  StHbtTTreeEvent*   mHbtTTreeEvent; //!
  StHbtEvent*        mHbtEvent; //!
  unsigned int       mEventIndex;

  StHbtEvent* read();
  int initRead(string dir, string file, string filter, string extention, int mMaxFiles);
  int uninitRead();
  StHbtEvent* write(StHbtEvent*);
  int initWrite(string, string);
  int uninitWrite();
  int fillChain(TChain* chain, const char* dir, const char* filter, const char* extention, const int maxFiles);
  int fillChain(TChain* chain, const char* list, const int maxFiles);

 protected:
  
  public:
  StHbtTTreeReader(StHbtIOMode mode, StIOMaker* io, 
		   const char* dirName, const char* fileName, const char* extention=".hbtTTreeMuDst", 
		   const char* filter=".", int maxFiles=99999);
 
 ~StHbtTTreeReader();
  
  StHbtEvent* ReturnHbtEvent();
  int WriteHbtEvent(StHbtEvent*);
  StHbtString Report();
  
  StTrackType TrackType(); 
  void SetTrackType(StTrackType);
  void SetDebug(int);


  ClassDef(StHbtTTreeReader, 1)
};

inline StTrackType StHbtTTreeReader::TrackType() { return mTrackType;}
inline void StHbtTTreeReader::SetTrackType(StTrackType t) { mTrackType=t;}
inline void StHbtTTreeReader::SetDebug(int debug) {mDebug=debug;}
  
#endif

/***************************************************************************
 *
 * $Log: StHbtTTreeReader.h,v $
 * Revision 1.3  2001/12/05 14:42:17  laue
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
 **************************************************************************/
