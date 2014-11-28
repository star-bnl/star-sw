/***************************************************************************
 *
 * $Id: StHbtMuDstReader.h,v 1.2 2003/09/07 03:49:02 perev Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StHbtMuDstReader_hh
#define StHbtMuDstReader_hh

#include <string>

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMuDSTMaker/COMMON/StMuArrays.h"

#include "Base/StHbtEventReader.hh"



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
#ifndef __NO_STRANGE_MUDST__
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
#endif
class TFile;
class TTree;
class TChain;
class TClonesArray;

//!class ioMode;//!enum ioMode {ioRead, ioWrite};
//!class ioNameMode;//!enum ioNameMode {ioFix, ioAuto};

class StHbtMuDstReader : public StHbtEventReader {
 public:
  StHbtMuDstReader(int mode, int nameMode, const char* dirName="./", const char* fileName="test.event.root", const char* filter=".", int maxfiles=10 );
  ~StHbtMuDstReader();
  
  StHbtEvent* ReturnHbtEvent();
  int Init();
  int Init(const char* ReadWrite, StHbtString& Message)
          { return StHbtEventReader::Init(ReadWrite,Message);}//WarnOff
  void Clear();
  void Finish();

  bool readTracks();
  bool readV0s();
  bool readXis();
  bool readKinks();
  unsigned int trackType(); 
  StMuDst* muDst();
  TChain* chain();
  TTree* tree();
  StEvent* stEvent();
#ifndef __NO_STRANGE_MUDST__
  StStrangeMuDstMaker* stStrangeMuDstMaker();
#endif
  void setTrackFilter(StMuCut* c);
  void setL3TrackFilter(StMuCut* c);
  void setProbabilityPidFile(const char* file);
  void setStEvent(StEvent*);
#ifndef __NO_STRANGE_MUDST__
  void setStStrangeMuDstMaker(StStrangeMuDstMaker*);
#endif
  void setTrackType(unsigned int);
  void setReadTracks(bool);
#ifndef __NO_STRANGE_MUDST__
  void setReadV0s(bool);
  void setReadXis(bool);
  void setReadKinks(bool);
#endif
  enum ioMode {ioRead, ioWrite};
  enum ioNameMode {ioFix, ioAuto};
private:
 
  StMuDst* mStMuDst;

  StEvent* mStEvent;
#ifndef __NO_STRANGE_MUDST__
  StStrangeMuDstMaker* mStStrangeMuDstMaker;
#endif
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
  TTree* mTTree;

  int mEventCounter;
  int mSplit;
  int mCompress;
  int mBufferSize;

  StHbtEvent* mHbtEvent;
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
#ifndef __NO_STRANGE_MUDST__
  void fillStrange(StStrangeMuDstMaker*);
#endif
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


  string basename(string);
 
  friend class StMuDst;

  TClonesArray* arrays[__NARRAYS__];
  TClonesArray* mArrays[__NARRAYS__];

#ifndef __NO_STRANGE_MUDST__
  TClonesArray* strangeArrays[__NSTRANGEARRAYS__];
  TClonesArray* mStrangeArrays[__NSTRANGEARRAYS__];
#endif
  ClassDef(StHbtMuDstReader, 1)
};

inline StMuDst* StHbtMuDstReader::muDst() { return mStMuDst;}
inline TChain* StHbtMuDstReader::chain() { return mChain; }
inline TTree* StHbtMuDstReader::tree() { return mTTree; }
inline void StHbtMuDstReader::setTrackFilter(StMuCut* c) { mTrackFilter=c;}
inline void StHbtMuDstReader::setL3TrackFilter(StMuCut* c) { mL3TrackFilter=c;}
#ifndef __NO_STRANGE_MUDST__
inline void StHbtMuDstReader::setStStrangeMuDstMaker(StStrangeMuDstMaker* s) {mStStrangeMuDstMaker=s;}
inline StStrangeMuDstMaker* StHbtMuDstReader::stStrangeMuDstMaker() {return mStStrangeMuDstMaker;}
#endif
inline void StHbtMuDstReader::setTrackType(unsigned int t) {mTrackType=t;}
inline unsigned int StHbtMuDstReader::trackType() {return mTrackType;}

inline bool StHbtMuDstReader::readTracks() { return mReadTracks;}
inline bool StHbtMuDstReader::readV0s() { return mReadV0s;}
inline bool StHbtMuDstReader::readXis() { return mReadXis;}
inline bool StHbtMuDstReader::readKinks() { return mReadKinks;}
inline void StHbtMuDstReader::setReadTracks(bool b) { mReadTracks=b;}
#ifndef __NO_STRANGE_MUDST__
inline void StHbtMuDstReader::setReadV0s(bool b) { mReadV0s=b;}
inline void StHbtMuDstReader::setReadXis(bool b) { mReadXis=b;}
inline void StHbtMuDstReader::setReadKinks(bool b) { mReadKinks=b;}
#endif
#endif

/***************************************************************************
 *
 * $Log: StHbtMuDstReader.h,v $
 * Revision 1.2  2003/09/07 03:49:02  perev
 * gcc 3.2 + WarnOff
 *
 * Revision 1.1  2002/03/20 19:32:24  laue
 * new reader for common MuDsts
 *
 *
 **************************************************************************/
