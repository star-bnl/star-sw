/***************************************************************************
 *
 * $Id: StarMuDstMaker.h,v 1.1 2002/03/05 15:41:09 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StarMuDstMaker_hh
#define StarMuDstMaker_hh

#include <string>

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"



class StarMuEvent;
class StarMuDst;
class StarMuCut;

class StEvent;
class StTrackNode;
class StTrack;
class StRichSpectra;
class StDetectorState;
class StL3AlgorithmInfo;

class StuProbabilityPidAlgorithm;

class StIOMaker;

/// strangeness group stuff
class StStrangeEvMuDst;
class StStrangeMuDstMaker;
class StV0MuDst;
class StV0Mc;
class StXiMuDst;
class StXiMc;
class StKinkMuDst;
class StKinkMc;
class StStrangeAssoc;

///
class StarMuCut;

class TFile;
class TTree;
class TChain;
class TClonesArray;

enum ioMode {ioRead, ioWrite};
enum ioNameMode {ioFix, ioAuto};

enum strangeTypes {smuEv=0, smuEvMc, smuV0, smuV0mc, smuV0Assoc, smuXi, smuXiMc, smuXiAssoc, smuKink, smuKinkMc, smuKinkAssoc};
enum muDstTypes {muEvent=0, muPrimary, muGlobal, muOther, muL3, muRich, muState, muAccept, muReject}; 

#define __MAX_Events__ 1
#define __MAX_Tracks__ 10000

#define __MAX_StRichSpectra__ 100
#define __MAX_StDetectorState__ 100
#define __MAX_StL3AlgorithmInfo__ 100

#define __MAX_StStrangeEvMuDst__ 1
#define __MAX_StV0MuDst__ 10000
#define __MAX_StV0Mc__ 10000
#define __MAX_StXiMuDst__ 10000
#define __MAX_StXiMc__ 10000
#define __MAX_StKinkMuDst__ 100
#define __MAX_StKinkMc__ 100
#define __MAX_StStrangeAssoc__ 100

#define __NARRAYS__ 9
#define __NSTRANGEARRAYS__ 11

class StarMuDstMaker : public StMaker{
 public:
  StarMuDstMaker(ioMode mode, ioNameMode nameMode, const char* dirName="./", const char* fileName="test.event.root", const char* filter="." );
  ~StarMuDstMaker();
  
  int Init();
  void Clear();
  int Make();
  int Finish();

  void setTrackFilter(StarMuCut* c);
  void setL3TrackFilter(StarMuCut* c);
  void setProbabilityPidFile(const char* file);

  StarMuDst* muDst();

private:
  StarMuDst* mStarMuDst;

  StEvent* mStEvent;
  StStrangeMuDstMaker* mStStrangeMuDstMaker;
  StIOMaker* mIOMaker;

  ioMode mIoMode;
  ioNameMode mIoNameMode;
  string mDirName;
  string mFileName;
  string mFilter;

  unsigned int mTrackType;
  bool mReadTracks;
  bool mReadV0s;
  bool mReadXis;
  bool mReadKinks;
  bool mFinish;

  StarMuCut* mTrackFilter;
  StarMuCut* mL3TrackFilter;

  TFile* mCurrentFile;
  string mCurrentFileName;

  TChain* mChain;
  TChain* mStrangeChain;

  TTree* mTTree;
  TTree* mStrangeTTree;

  int mEventCounter;
  int mSplit;
  int mCompress;
  int mBufferSize;

  StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;


  //! protected:
  
  string buildFileName(string dir, string fileName, string extention);
  void openWrite(string fileName);
  void write();
  void closeWrite();

  void makeChain(const char* dir, const char* filter, int maxFiles=10);
  void openRead();
  void read();
  void closeRead();

  void clear(TClonesArray* t, int& counter);
  void clear();
  TClonesArray* clonesArray(TClonesArray* p, const char* type, int size, int& counter);

  void fill();
  void fillTrees(StEvent* ev, StarMuCut* cut=0);
  void fillEvent(StEvent* ev, StarMuCut* cut=0);
  void fillStrange(StStrangeMuDstMaker*);
  void fillL3Tracks(StEvent* ev, StarMuCut* cut=0);
  void fillTracks(StEvent* ev, StarMuCut* cut=0);
  void fillDetectorStates(StEvent* ev);
  void fillL3AlgorithmInfo(StEvent* ev);
  template <class T> void addType(TClonesArray* tcaFrom, TClonesArray* tcaTo , T t);
  template <class T> int addType(TClonesArray* tcaTo , T t);
  template <class T, class U> int addType(TClonesArray* tcaTo , U u, T t);
  void addTrackNode(const StEvent* ev, const StTrackNode* node, StarMuCut* cut, TClonesArray* gTCA=0, TClonesArray* pTCA=0, TClonesArray* oTCA=0, bool l3=false);
  int addTrack(TClonesArray* tca, const StEvent* event, const StTrack* track, StarMuCut* cut, int index2Global, bool l3=false);
/*   int addRichSpectra(const StRichSpectra* rich); */
/*   int addDetectorState(const StDetectorState* states); */
/*   int addL3AlgorithmInfo(TClonesArray* tca, StL3AlgorithmInfo* alg); */

  StRichSpectra* richSpectra(const StTrack* track);

  void setStEvent(StEvent*);
  StEvent* stEvent();
  void setStStrangeMuDstMaker(StStrangeMuDstMaker*);
  StStrangeMuDstMaker* stStrangeMuDstMaker();

  unsigned int trackType(); 
  bool readTracks();
  bool readV0s();
  bool readXis();
  bool readKinks();
  void setTrackType(unsigned int);
  void setReadTracks(bool);
  void setReadV0s(bool);
  void setReadXis(bool);
  void setReadKinks(bool);

  string basename(string);

  friend class StarMuDst;

  //! protected:
  
  static char* arrayNames[__NARRAYS__];
  static char* arrayTypes[__NARRAYS__];
  static int arraySizes[__NARRAYS__];
  static int arrayCounters[__NARRAYS__];
  static TClonesArray* arrays[__NARRAYS__];
  TClonesArray* mArrays[__NARRAYS__];
  
  static char* strangeArrayNames[__NSTRANGEARRAYS__];
  static char* strangeArrayTypes[__NSTRANGEARRAYS__];
  static int strangeArrayCounters[__NSTRANGEARRAYS__];
  static int strangeArraySizes[__NSTRANGEARRAYS__];
  static TClonesArray* strangeArrays[__NSTRANGEARRAYS__];
  TClonesArray* mStrangeArrays[__NSTRANGEARRAYS__];

  ClassDef(StarMuDstMaker, 1)
};

inline StarMuDst* StarMuDstMaker::muDst() { return mStarMuDst;}
inline void StarMuDstMaker::setTrackFilter(StarMuCut* c) { mTrackFilter=c;}
inline void StarMuDstMaker::setL3TrackFilter(StarMuCut* c) { mL3TrackFilter=c;}
inline void StarMuDstMaker::setStStrangeMuDstMaker(StStrangeMuDstMaker* s) {mStStrangeMuDstMaker=s;}
inline StStrangeMuDstMaker* StarMuDstMaker::stStrangeMuDstMaker() {return mStStrangeMuDstMaker;}
inline void StarMuDstMaker::setTrackType(unsigned int t) {mTrackType=t;}
inline unsigned int StarMuDstMaker::trackType() {return mTrackType;}

inline bool StarMuDstMaker::readTracks() { return mReadTracks;}
inline bool StarMuDstMaker::readV0s() { return mReadV0s;}
inline bool StarMuDstMaker::readXis() { return mReadXis;}
inline bool StarMuDstMaker::readKinks() { return mReadKinks;}
inline void StarMuDstMaker::setReadTracks(bool b) { mReadTracks=b;}
inline void StarMuDstMaker::setReadV0s(bool b) { mReadV0s=b;}
inline void StarMuDstMaker::setReadXis(bool b) { mReadXis=b;}
inline void StarMuDstMaker::setReadKinks(bool b) { mReadKinks=b;}

#endif

/***************************************************************************
 *
 * $Log: StarMuDstMaker.h,v $
 * Revision 1.1  2002/03/05 15:41:09  jeromel
 * First version of Frank's Commone MicroDST.
 *
 *
 **************************************************************************/
