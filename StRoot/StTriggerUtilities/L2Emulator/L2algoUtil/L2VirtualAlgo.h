#ifndef L2VirtualAlgo_h
#define L2VirtualAlgo_h


/***************************************************************
 * $Id: L2VirtualAlgo.h,v 1.11 2008/01/30 15:09:35 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 ***************************************************************
 * Descripion:
 * all actual L2 algos should inherit from it the 4 methods
 ***************************************************************
 */

//#include "/asm-i386/msr.h" /* for rdtscl */
// Great suggestion from Pibero, to use ASM macro directly
#define rdtscl_macro(low) \
     __asm__ __volatile__("rdtsc" : "=a" (low) : : "edx")

#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "trgStructures.h"
#else
  #include "StDaqLib/TRG/trgStructures.h"
#endif

class L2EmcDb;
class L2Histo;
class L2VirtualAlgo {
  enum {mxTxt=1000};
 protected:  
  char mName[mxTxt];    
  char mOutDir[mxTxt];
  L2EmcDb* mDb;
  FILE  *mLogFile, *mHistFile;
  int mResultOffset;
  int oflTrigId;
  bool mAccept;
  unsigned long mEveTimeStart, mEveTimeStop,mEveTimeDiff;
  void finishCommonHistos();
  L2Histo *mhT, *mhN;
  int mEventsInRun;// run_nEventIn,
 public:
  void setOflTrigID(int x) {oflTrigId=x;}
  int getOflTrigID() {return oflTrigId;}
  bool isAccepted(){ return mAccept; }
  bool accepted()  { return mAccept; }  // obsolete
  const char *getName(){ return mName; }
  L2VirtualAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff);
  virtual ~L2VirtualAlgo()=0; // memory leak NOT taken care off
  virtual int   initRun(int runNo, int *rc_ints, float *rc_floats)=0;
  // 2006 version
  virtual bool  doEvent(int  L0trg, int inpEveId, TrgDataType* trgData,  
                        int  bemcIn, unsigned short *bemcData,
                        int  eemcIn, unsigned short *eemcData)=0;

  virtual void  finishRun()=0;// at the end of each run
  static int  readParams(const char *fileN, int mxPar, int *iPar, float *fPar);
  const char* name() const { return mName; }
};


#endif
