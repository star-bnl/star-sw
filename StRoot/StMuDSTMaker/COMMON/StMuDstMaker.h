/***************************************************************************
 *
 * $Id: StMuDstMaker.h,v 1.1 2002/03/08 17:04:17 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuDstMaker_hh
#define StMuDstMaker_hh

#include <string>

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StMuArrays.h"




class StMuEvent;
class StMuDst;
class StMuCut;

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
class StMuCut;

class TFile;
class TTree;
class TChain;
class TClonesArray;

enum ioMode {ioRead, ioWrite};
enum ioNameMode {ioFix, ioAuto};

class StMuDstMaker : public StMaker{
 public:
  StMuDstMaker(ioMode mode, ioNameMode nameMode, const char* dirName="./", const char* fileName="test.event.root", const char* filter=".", int maxfiles=10 );
  ~StMuDstMaker();
  
  int Init();
  void Clear();
  int Make();
  int Finish();

  void setTrackFilter(StMuCut* c);
  void setL3TrackFilter(StMuCut* c);
  void setProbabilityPidFile(const char* file);

  StMuDst* muDst();
  TChain* chain();
  TChain* strangeChain();
  TTree* tree();
  TTree* strangeTree();

private:
  StMuDst* mStMuDst;

  StEvent* mStEvent;
  StStrangeMuDstMaker* mStStrangeMuDstMaker;
  StIOMaker* mIOMaker;

  ioMode mIoMode;
  ioNameMode mIoNameMode;
  string mDirName;
  string mFileName;
  string mFilter;
  int mMaxFiles;

  unsigned int mTrackType;
  bool mReadTracks;
  bool mReadV0s;
  bool mReadXis;
  bool mReadKinks;
  bool mFinish;

  StMuCut* mTrackFilter;
  StMuCut* mL3TrackFilter;

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
  void fillTrees(StEvent* ev, StMuCut* cut=0);
  void fillEvent(StEvent* ev, StMuCut* cut=0);
  void fillStrange(StStrangeMuDstMaker*);
  void fillL3Tracks(StEvent* ev, StMuCut* cut=0);
  void fillTracks(StEvent* ev, StMuCut* cut=0);
  void fillDetectorStates(StEvent* ev);
  void fillL3AlgorithmInfo(StEvent* ev);
  template <class T> void addType(TClonesArray* tcaFrom, TClonesArray* tcaTo , T t);
  template <class T> int addType(TClonesArray* tcaTo , T t);
  template <class T, class U> int addType(TClonesArray* tcaTo , U u, T t);
  void addTrackNode(const StEvent* ev, const StTrackNode* node, StMuCut* cut, TClonesArray* gTCA=0, TClonesArray* pTCA=0, TClonesArray* oTCA=0, bool l3=false);
  int addTrack(TClonesArray* tca, const StEvent* event, const StTrack* track, StMuCut* cut, int index2Global, bool l3=false);
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

  friend class StMuDst;

  //! protected:
  
//! /*   static char* arrayNames[__NARRAYS__]; */
//! /*   static char* arrayTypes[__NARRAYS__]; */
//! /*   static int arraySizes[__NARRAYS__]; */
//! /*   static int arrayCounters[__NARRAYS__]; */
  TClonesArray* arrays[__NARRAYS__];
  TClonesArray* mArrays[__NARRAYS__];
  
//! /*   static char* strangeArrayNames[__NSTRANGEARRAYS__]; */
//! /*   static char* strangeArrayTypes[__NSTRANGEARRAYS__]; */
//! /*   static int strangeArrayCounters[__NSTRANGEARRAYS__]; */
//! /*   static int strangeArraySizes[__NSTRANGEARRAYS__]; */
  TClonesArray* strangeArrays[__NSTRANGEARRAYS__];
  TClonesArray* mStrangeArrays[__NSTRANGEARRAYS__];

  ClassDef(StMuDstMaker, 1)
};

inline StMuDst* StMuDstMaker::muDst() { return mStMuDst;}
inline TChain* StMuDstMaker::chain() { return mChain; }
inline TChain* StMuDstMaker::strangeChain() { return mStrangeChain;}
inline TTree* StMuDstMaker::tree() { return mTTree; }
inline TTree* StMuDstMaker::strangeTree() { return mStrangeTTree;}
inline void StMuDstMaker::setTrackFilter(StMuCut* c) { mTrackFilter=c;}
inline void StMuDstMaker::setL3TrackFilter(StMuCut* c) { mL3TrackFilter=c;}
inline void StMuDstMaker::setStStrangeMuDstMaker(StStrangeMuDstMaker* s) {mStStrangeMuDstMaker=s;}
inline StStrangeMuDstMaker* StMuDstMaker::stStrangeMuDstMaker() {return mStStrangeMuDstMaker;}
inline void StMuDstMaker::setTrackType(unsigned int t) {mTrackType=t;}
inline unsigned int StMuDstMaker::trackType() {return mTrackType;}

inline bool StMuDstMaker::readTracks() { return mReadTracks;}
inline bool StMuDstMaker::readV0s() { return mReadV0s;}
inline bool StMuDstMaker::readXis() { return mReadXis;}
inline bool StMuDstMaker::readKinks() { return mReadKinks;}
inline void StMuDstMaker::setReadTracks(bool b) { mReadTracks=b;}
inline void StMuDstMaker::setReadV0s(bool b) { mReadV0s=b;}
inline void StMuDstMaker::setReadXis(bool b) { mReadXis=b;}
inline void StMuDstMaker::setReadKinks(bool b) { mReadKinks=b;}

#endif

/***************************************************************************
 *
 * $Log: StMuDstMaker.h,v $
 * Revision 1.1  2002/03/08 17:04:17  laue
 * initial revision
 *
 *
 **************************************************************************/
