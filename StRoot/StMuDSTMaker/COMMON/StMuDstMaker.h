/***************************************************************************
 *
 * $Id: StMuDstMaker.h,v 1.18 2003/01/29 03:04:57 laue Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 ***************************************************************************/
#ifndef StMuDstMaker_hh
#define StMuDstMaker_hh

#include <string>

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

#include "StMaker.h"
#include "StChain.h"
#include "St_DataSetIter.h"

#include "StMuArrays.h"

#include "StMuFilter.h"
#include "StMuL3Filter.h"


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



/// emc stuff
#include "StMuEmcCollection.h"
class StMuEmcUtil;

class TFile;
class TTree;
class TChain;
class TClonesArray;

/**
   @class StMuDstMaker
   Class to create and read STAR's common micro dst (StMuDst)
   
   This class is a true maker in the STAR sense. It inherits from "StMaker" and implements the functions "int Init()", "void Clear()",
   int Make()", and "int Finish()" in order to run as part of an "StChain". Please refer to the STAR Computing Web pages in case you do not 
   know what "StMaker" and "StChain" mean.
   
*/
class StMuDstMaker : public StMaker {
 public:
    /// Default constructor
    StMuDstMaker(const char* name="MuDst");
    /// Constructor
    StMuDstMaker(int mode, int nameMode, const char* dirName="./", const char* fileName="", const char* filter=".", int maxfiles=10, 
		 const char* name="MuDst" );
    ~StMuDstMaker();
    
  int Init();
  void Clear();
  int Make();
  int Finish();

  /// Set the track filter used for all tracks (except the L3 tracks) when creating muDsts from StEvent and writing to disk.
  void setTrackFilter(StMuCut* c);
  StMuFilter* trackFilter();
  /// Set the track filter used for L3 tracks when creating muDsts from StEvent and writing to disk.
  void setL3TrackFilter(StMuCut* c);
  StMuL3Filter* l3TrackFilter();
  /// Set the file from where the PID probability tables should be read. 
  /** Set the file from where the PID probability tables should be read. These tables might change from production version to production version.
      It is the reposibility of who ever creates muDsts to make sure the right tables are used. So far, Aihong was providing these files. Thanks,Aihong. 
  */
  void setProbabilityPidFile(const char* file);
  /// Returns pointer to the StMuDst object, the transient class that holds all the TClonesArrays and has access functions to the tracks, v0s, etc. 
  /// Returns null pointer if no StMuDst available.
  StMuDst* muDst();
  /// In read mode, returns pointer to the chain of .MuDst.root files that where selected. 
  TChain* chain();
  /// Returns pointer to the current TTree, the top level io structure that holds the event, track, v0, etc. information in branches of that tree.
  TTree* tree();

  /// Sets the split level for the file and all branches. Please refer to the ROOT manual (http://root.cern.ch) for more information.
  void setSplit(int=99);
  /// Sets the buffer size for all branches. 
  void setBufferSize(int=65536*4);
  /// Sets the compression level for the file and all branches. 0 means no compression, 9 is the higher compression level. 
  void setCompression(int comp=9);

  StMuEmcUtil* muEmcUtil() { return mEmcUtil; } ///< return pointer to StMuEmcUtil;

  virtual const char *GetCVS() const {  ///< Returns version tag.

    static const char cvs[]="Tag $Name:  $ $Id: StMuDstMaker.h,v 1.18 2003/01/29 03:04:57 laue Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }


private:
  enum ioMode {ioRead, ioWrite};
  /** Specifies the way the output file name is contructed when creating muDsts. 
      ioFix = use filename specified in when calling the constructor, right in the same output file for all input files. 
      ioIOMaker = create one output file per input file, derive output filename from current input file of the StIOMaker.
      ioTreeMaker = create one output file per input file, derive output filename from current input file of the StTreeMaker.
  */
  enum ioNameMode {ioFix=0, ioIOMaker, ioTreeMaker};


  StMuDst* mStMuDst;
  StEvent* mStEvent;
  StStrangeMuDstMaker* mStStrangeMuDstMaker;
  StIOMaker* mIOMaker;
  StTreeMaker* mTreeMaker;
  StMuEmcUtil* mEmcUtil;

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

  
  template<class T> 
  void saveDelete(T* t) { if (t!=0) delete t; t=0;}    

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
  void del(TClonesArray* t, int& counter);
  void clear();

  void createArrays();
  TClonesArray* clonesArray(TClonesArray*& p, const char* type, int size, int& counter);

  void fill();
  void fillTrees(StEvent* ev, StMuCut* cut=0);
  void fillEvent(StEvent* ev, StMuCut* cut=0);
  void fillEmc(StEvent* ev);
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
/*   int addType(TClonesArray* tcaTo , StMuEmcCollection t); */

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

  TClonesArray* arrays[__NARRAYS__];//->
  TClonesArray* mArrays[__NARRAYS__];//->

  TClonesArray* strangeArrays[__NSTRANGEARRAYS__];//->
  TClonesArray* mStrangeArrays[__NSTRANGEARRAYS__];//->

  TClonesArray* emcArrays[__NEMCARRAYS__];//->
  TClonesArray* mEmcArrays[__NEMCARRAYS__];//->


  ClassDef(StMuDstMaker, 1)
}; 

inline StMuDst* StMuDstMaker::muDst() { return mStMuDst;}
inline TChain* StMuDstMaker::chain() { return mChain; }
inline TTree* StMuDstMaker::tree() { return mTTree; }
inline void StMuDstMaker::setTrackFilter(StMuCut* c) { mTrackFilter=c;}
inline void StMuDstMaker::setL3TrackFilter(StMuCut* c) { mL3TrackFilter=c;}
inline StMuFilter* StMuDstMaker::trackFilter() { return (StMuFilter*)mTrackFilter;}
inline StMuL3Filter* StMuDstMaker::l3TrackFilter() { return (StMuL3Filter*)mL3TrackFilter;}
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
 * Revision 1.18  2003/01/29 03:04:57  laue
 * !!DIRTY FIX FOR StMuEmcCollection
 * !! Was memor leaking. Leak fixed, but slow and dirty.
 * !! Propose to change the structure as soon as possible.
 *
 * Revision 1.17  2003/01/23 21:59:50  laue
 * Modification to compile on Solaris.
 *
 * Revision 1.16  2003/01/09 18:59:45  laue
 * initial check in of new EMC classes and the changes required
 *
 * Revision 1.15  2002/11/08 14:18:59  laue
 * saveDelete(<T>) added, sets pointer=null after delete
 *
 * Revision 1.14  2002/09/11 21:02:41  laue
 * added cut on track encoded method for ITTF
 *
 * Revision 1.13  2002/08/20 19:55:49  laue
 * Doxygen comments added
 *
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
