/***************************************************************************
 *
 * $Id: StHbtMuDstMakerReaderG.h,v 1.1 2002/11/19 23:42:16 renault Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StHbtMuDstMakerReaderG_hh
#define StHbtMuDstMakerReaderG_hh

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

class StHbtMuDstMakerReaderG : public StHbtEventReader {
 public:
  StHbtMuDstMakerReaderG(StMuDstMaker* maker);
  ~StHbtMuDstMakerReaderG();
  
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
  void setReadV0Daughters(bool b);//Gael 24 Sept 02

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
  bool mReadV0Daughters;

  bool mFinish;

  TFile* mCurrentFile;
  string mCurrentFileName;

  int mEventCounter;

  StHbtEvent* mHbtEvent;
  StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;

  ClassDef(StHbtMuDstMakerReaderG, 1)
};

inline StMuDst* StHbtMuDstMakerReaderG::muDst() { return mMuDst;}
inline void StHbtMuDstMakerReaderG::setFlowMaker(StFlowMaker* f) {mFlowMaker=f;}
inline StFlowMaker* StHbtMuDstMakerReaderG::flowMaker() {return mFlowMaker;}
inline void StHbtMuDstMakerReaderG::setTrackType(unsigned int t) {mTrackType=t;}
inline unsigned int StHbtMuDstMakerReaderG::trackType() {return mTrackType;}

inline bool StHbtMuDstMakerReaderG::readTracks() { return mReadTracks;}
inline bool StHbtMuDstMakerReaderG::readV0s() { return mReadV0s;}
inline bool StHbtMuDstMakerReaderG::readXis() { return mReadXis;}
inline bool StHbtMuDstMakerReaderG::readKinks() { return mReadKinks;}
inline void StHbtMuDstMakerReaderG::setReadTracks(bool b) { mReadTracks=b;}
inline void StHbtMuDstMakerReaderG::setReadV0s(bool b) { mReadV0s=b;}
inline void StHbtMuDstMakerReaderG::setReadXis(bool b) { mReadXis=b;}
inline void StHbtMuDstMakerReaderG::setReadKinks(bool b) { mReadKinks=b;}
inline void StHbtMuDstMakerReaderG::setReadV0Daughters(bool b) { mReadV0Daughters=b;}//Gael 24 Sept 02
#endif

/***************************************************************************
 *
 * $Log: StHbtMuDstMakerReaderG.h,v $
 * Revision 1.1  2002/11/19 23:42:16  renault
 * Adding new function to enable reading V0 daughters
 *
 *
 **************************************************************************/
