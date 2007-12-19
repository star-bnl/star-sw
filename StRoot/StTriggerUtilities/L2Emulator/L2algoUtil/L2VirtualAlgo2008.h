#ifndef L2VirtualAlgo2008_h
#define L2VirtualAlgo2008_h


/*********************************************************************
 * $Id: L2VirtualAlgo2008.h,v 1.1 2007/12/19 02:30:17 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * all actual L2 algos should inherit from it the 4 methods
 *********************************************************************
 */

//#include "/asm-i386/msr.h" /* for rdtscl */
// Great suggestion from Pibero, to use ASM macro directly
#define rdtscl_macro(low) \
     __asm__ __volatile__("rdtsc" : "=a" (low) : : "edx")
 
#define rdtsc_macro(low,high) \
     __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))

#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "trgStructures.h"
#else
  #include "StDaqLib/TRG/trgStructures.h"
#endif

#include <string>
#include "L2event2008.h"
   
class L2EmcDb;
class L2Histo;
class L2VirtualAlgo2008 { 

 protected:  
  std::string mOutDir1, mName1;
  L2EmcDb *mDb;
  FILE    *mLogFile, *mHistFile;
  L2Histo *mhN; /*  Neve/case */
  L2Histo *mhTc,*mhTd,*mhTcd;/* time/eve : compute, decision, compute+decision */
  L2Histo *mhRc, *mhRd, *mhRa ; /*  rate/sec: compute, decision, accepted */
  int   oflTrigId; // important only for off-line analysis /w Makers
  int   mAccept;
  int   mEventsInRun;
  int   mSecondsInRun;
  int   mEventID;
  int   mRunNumber;
  enum {par_cpuTicksPerSecond=1600000000};
  unsigned long  mComputeTimeStart,  mComputeTimeStop,  mComputeTimeDiff;
  unsigned long  mDecisionTimeStart, mDecisionTimeStop, mDecisionTimeDiff;
  unsigned long long mRunStartTicks;

  L2Histo **hA; // my private HBOOK@L2
  int mxHA; // set by user
  void setMaxHist(int k) {  assert(k>0);
    mxHA=k;  hA=new L2Histo *[mxHA];
  }
  int finishCommonHistos(); 

  void  computeStart(int flag, int inpL2EveId);
  void  computeStop();

 public:
  L2VirtualAlgo2008(const char* name, L2EmcDb* db, char* outDir);
  virtual ~L2VirtualAlgo2008(); // memory leak NOT taken care off
  void setOflTrigID(int x) {oflTrigId=x;} // only for Maker-analysis
  int  getOflTrigID() {return oflTrigId;} // only for Maker-analysis

  bool   isAccepted(){ return mAccept; }
  static int  readParams(const char *fileN, int mxPar, int *iPar, float *fPar);
  const char *getName() { return mName1.c_str();}

  int   initRun(int runNo, int *rc_ints, float *rc_floats);
  void  compute (int flag, int inpL2EveId);
  bool  decision(int flag, int inpL2EveId);
  void  finishRun();

  // implement algo specific operations in the virtual functions
  virtual int   initRunUser(int runNo, int *rc_ints, float *rc_floats){ return 0;}
  virtual void  computeUser(int flag, int inpL2EveId){};
  virtual bool  decisionUser(int flag, int inpL2EveId){ return mAccept; }
  virtual void  finishRunUser(){}// at the end of each run

  // read-only access to current 'global' event, updated in compute
  const int *globEve_btow_hitSize; // it is just one value, use *-prefix
  const HitTower  *globEve_btow_hit;
};


#endif
