/***************************************************************************
 *
 * $Id: StHbtMuDstMakerReader.h,v 1.1 2002/08/27 18:21:26 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StHbtMuDstMakerReader_hh
#define StHbtMuDstMakerReader_hh

#include <string>

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

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

/// flowstuff
class StFlowMaker;

class StMuCut;
class TFile;
class TTree;
class TChain;
class TClonesArray;

//!class ioMode;//!enum ioMode {ioRead, ioWrite};
//!class ioNameMode;//!enum ioNameMode {ioFix, ioAuto};

class StHbtMuDstMakerReader : public StHbtEventReader {
 public:
  StHbtMuDstMakerReader(StMuDstMaker* maker);
  ~StHbtMuDstMakerReader();
  
  StHbtEvent* ReturnHbtEvent();
  int Init();
  void Clear();
  void clear();
  void Finish();

  bool readTracks();
  bool readV0s();
  bool readXis();
  bool readKinks();
  unsigned int trackType(); 
  StMuDst* muDst();

  StIOMaker ioMaker();
  StStrangeMuDstMaker* stStrangeMuDstMaker();
  StFlowMaker* flowMaker();

  void setProbabilityPidFile(const char* file);
  void setFlowMaker(StFlowMaker*);
  void setTrackType(unsigned int);
  void setReadTracks(bool);
  void setReadV0s(bool);
  void setReadXis(bool);
  void setReadKinks(bool);

  enum ioMode {ioRead, ioWrite};
  enum ioNameMode {ioFix, ioAuto};
private:
 
  StMuDst* mMuDst;

  StStrangeMuDstMaker* mStStrangeMuDstMaker;
  StFlowMaker* mFlowMaker;
  StMuDstMaker* mMuDstMaker;

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

  TFile* mCurrentFile;
  string mCurrentFileName;

  int mEventCounter;

  StHbtEvent* mHbtEvent;
  StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;

  ClassDef(StHbtMuDstMakerReader, 1)
};

inline StMuDst* StHbtMuDstMakerReader::muDst() { return mMuDst;}
inline void StHbtMuDstMakerReader::setFlowMaker(StFlowMaker* f) {mFlowMaker=f;}
inline StFlowMaker* StHbtMuDstMakerReader::flowMaker() {return mFlowMaker;}
inline void StHbtMuDstMakerReader::setTrackType(unsigned int t) {mTrackType=t;}
inline unsigned int StHbtMuDstMakerReader::trackType() {return mTrackType;}

inline bool StHbtMuDstMakerReader::readTracks() { return mReadTracks;}
inline bool StHbtMuDstMakerReader::readV0s() { return mReadV0s;}
inline bool StHbtMuDstMakerReader::readXis() { return mReadXis;}
inline bool StHbtMuDstMakerReader::readKinks() { return mReadKinks;}
inline void StHbtMuDstMakerReader::setReadTracks(bool b) { mReadTracks=b;}
inline void StHbtMuDstMakerReader::setReadV0s(bool b) { mReadV0s=b;}
inline void StHbtMuDstMakerReader::setReadXis(bool b) { mReadXis=b;}
inline void StHbtMuDstMakerReader::setReadKinks(bool b) { mReadKinks=b;}

#endif

/***************************************************************************
 *
 * $Log: StHbtMuDstMakerReader.h,v $
 * Revision 1.1  2002/08/27 18:21:26  laue
 * New reader. Wrapper around the StMuDstMaker
 *
 *
 **************************************************************************/
