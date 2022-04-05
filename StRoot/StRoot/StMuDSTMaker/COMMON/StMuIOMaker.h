/***************************************************************************
 *
 * $Id: StMuIOMaker.h,v 1.11 2014/08/06 11:43:31 jeromel Exp $
 * Author: Frank Laue, BNL, laue@bnl.gov
 *
 * Made it integrated to StIOMaker for applying Grid Collector 
 *                                            Wei-Ming Zhang KSU 3/8/2004
 ***************************************************************************/
#ifndef StMuIOMaker_hh
#define StMuIOMaker_hh

#include <string>

#ifndef ST_NO_NAMESPACES
using namespace std;
#endif

#include "StMuDstMaker.h"

class StMuDst;

/// pmd stuff
//#include "StMuPmdCollection.h"
class StMuEmcUtil;
class StMuPmdUtil;
class StMuTofUtil;

class TFile;
class TChain;
class TClonesArray;

/**
   @class StMuIOMaker
   Class to create and read STAR's common micro dst (StMuDst)
   
   This class is a true maker in the STAR sense. It inherits from "StMaker" and 
   implements the functions "int Init()", "void Clear()",
   int Make()", and "int Finish()" in order to run as part of an "StChain". 
   Please refer to the STAR Computing Web pages in case you do not 
   know what "StMaker" and "StChain" mean.
   
*/
class StMuIOMaker : public StMuDstMaker {
 public:
    /// Default constructor
    StMuIOMaker(const char* name="",const char *ioFile="");
    ~StMuIOMaker();
    
  int Init();
  int Make();
  int Make(int index);
  int Make(int major, int minor);
  int Make(const StUKey &RunEvent);
  int Skip(int nskip) { return Make(mCurrentIndex+nskip); }
  int Finish();

// virtual methods of inherited StIOInterFace
  virtual Int_t  Open(const Char_t *ioFile=0);
  virtual void  Close(Option_t *opt=0);
  virtual Int_t MakeRead(const StUKey &RunEvent);
  virtual Int_t MakeRead(){StUKey uk; return MakeRead(uk);};


  int currentIndex() { return mCurrentIndex; }
  int eventCounter() { return mEventCounter; }
  int numberOfEvents() { return mNumberOfEvents; }


  virtual const char *GetCVS() const {  ///< Returns version tag.
    static const char cvs[]="Tag $Name:  $ $Id: StMuIOMaker.h,v 1.11 2014/08/06 11:43:31 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

protected:

  TTree* mOutTree;     //!
  
  string mOutFileName; //!
  TFile* mOutFile;     //!

  int mNumberOfEvents; //! holds the # of events in the current chain (file)
  int mCurrentIndex;   //! holds the index of the last event read
  bool mMuSave;        //!
  bool mBadInFile;     //!
  bool mCloseWrite;    //!

  int  openRead();
  void openMuWrite();
  void closeMuWrite();

//@

  ClassDef(StMuIOMaker, 0)
}; 

#endif

/***************************************************************************
 *
 * $Log: StMuIOMaker.h,v $
 * Revision 1.11  2014/08/06 11:43:31  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 1.10  2004/07/02 01:51:09  perev
 * Wei-Ming Zhang developments
 *
 * Revision 1.9  2004/04/20 18:49:10  perev
 * Big reorganization, now StMuIOMkaer inherits from StMuDstMaker
 *
 * Revision 1.7  2004/04/09 22:02:50  subhasis
 * after tof createevent fix by Xin
 *
 * Revision 1.6  2004/04/09 03:36:15  jeromel
 * Removed TOF support entirely for now as we need a working version ... Will
 * revisit later.
 *
 * Revision 1.5  2004/04/02 03:24:54  jeromel
 * Changes implements PMD and TOF.  TOF is clearly incomplete.
 *
 * Revision 1.4  2003/09/12 21:31:50  jeromel
 * No changes (ident)
 *
 * Revision 1.3  2003/09/11 05:49:20  perev
 * ansi corrs
 *
 * Revision 1.2  2003/09/09 18:16:53  laue
 * StMuIOMaker: embedded documentation added
 * StMuTimer: name of define changed (was same as StTimer)
 *
 **************************************************************************/
