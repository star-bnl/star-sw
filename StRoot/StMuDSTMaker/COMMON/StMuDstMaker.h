/***************************************************************************
 *
 * $Id: StMuDstMaker.h,v 1.12 2002/05/20 17:23:31 laue Exp $
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
class StTreeMaker;

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
class StStrangeCuts;

///
class StMuCut;

class TFile;
class TTree;
class TChain;
class TClonesArray;


class StMuDstMaker : public StMaker {
 public:
  StMuDstMaker(const char* name="MuDst");
  StMuDstMaker(int mode, int nameMode, const char* dirName="./", const char* fileName="", const char* filter=".", int maxfiles=10 );
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
  TTree* tree();

  void setSplit(int=99);
  void setBufferSize(int=65536*4);
  void setCompression(int comp=9);

  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMuDstMaker.h,v 1.12 2002/05/20 17:23:31 laue Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }


private:
  enum ioMode {ioRead, ioWrite};
  enum ioNameMode {ioFix=0, ioIOMaker, ioTreeMaker};


  StMuDst* mStMuDst;

  StEvent* mStEvent;
  StStrangeMuDstMaker* mStStrangeMuDstMaker;
  StIOMaker* mIOMaker;
  StTreeMaker* mTreeMaker;

  int mIoMode;
  int mIoNameMode;
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
  int mCompression;
  int mBufferSize;

  StuProbabilityPidAlgorithm* mProbabilityPidAlgorithm;


  //! protected:
  
  void openWrite(string fileName);
  void write();
  void closeWrite();
 
  void streamerOff();

  void openRead();
  void read();
  void setBranchAddresses();
  void closeRead();

  void setBranchAddresses(TChain*);

  void clear(TClonesArray* t, int& counter);
  void clear();

  void createArrays();
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
  string dirname(string);
  string buildFileName(string dir, string fileName, string extention);
 
  friend class StMuDst;

  TClonesArray* arrays[__NARRAYS__]; //->
  TClonesArray* mArrays[__NARRAYS__];//->

  TClonesArray* strangeArrays[__NSTRANGEARRAYS__];//->
  TClonesArray* mStrangeArrays[__NSTRANGEARRAYS__];//->

  ClassDef(StMuDstMaker, 1)
};

inline StMuDst* StMuDstMaker::muDst() { return mStMuDst;}
inline TChain* StMuDstMaker::chain() { return mChain; }
inline TTree* StMuDstMaker::tree() { return mTTree; }
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

inline void StMuDstMaker::setSplit(int split) { mSplit = split;}
inline void StMuDstMaker::setCompression(int comp) { mCompression = comp;}
inline void StMuDstMaker::setBufferSize(int buf) { mBufferSize = buf; }

#endif

/***************************************************************************
 *
 * $Log: StMuDstMaker.h,v $
 * Revision 1.12  2002/05/20 17:23:31  laue
 * StStrangeCuts added
 *
 * Revision 1.11  2002/05/04 23:56:30  laue
 * some documentation added
 *
 * Revision 1.10  2002/04/26 20:57:31  jeromel
 * Added GetCVS()
 *
 * Revision 1.9  2002/04/11 14:19:30  laue
 * - update for RH 7.2
 * - decrease default arrays sizes
 * - add data base readerfor number of events in a file
 *
 * Revision 1.8  2002/04/01 22:42:30  laue
 * improved chain filter options
 *
 * Revision 1.7  2002/03/28 05:10:34  laue
 * update for running in the production
 *
 * Revision 1.6  2002/03/27 03:47:27  laue
 * better filter options
 *
 * Revision 1.5  2002/03/27 00:50:11  laue
 * bux fix from earlier check in
 *
 * Revision 1.4  2002/03/26 19:33:15  laue
 * minor updates
 *
 * Revision 1.3  2002/03/20 16:04:11  laue
 * minor changes, mostly added access functions
 *
 * Revision 1.2  2002/03/08 20:04:31  laue
 * change from two trees to 1 tree per file
 *
 *
 **************************************************************************/
