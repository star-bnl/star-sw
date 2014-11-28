#ifndef L2VirtualAlgo2008_h
#define L2VirtualAlgo2008_h


/*************************************************************
 * $Id: L2VirtualAlgo2008.h,v 1.6 2009/08/26 19:33:57 fine Exp $
 * \author Jan Balewski, IUCF, 2006 
 *************************************************************
 * Descripion:
 * all actual L2 algos should inherit from it the 4 methods
 *************************************************************
 */

//#include "/asm-i386/msr.h" /* for rdtscl */
// A great suggestion from Pibero, to use ASM macro directly
#define rdtscl_macro(low) \
     __asm__ __volatile__("rdtsc" : "=a" (low) : : "edx")
 
#define rdtsc_macro(low,high) \
     __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))

#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "trgStructures.h"
#else
  #include "StDaqLib/TRG/trgStructures.h"
#endif

#include <cassert>
#include <string>
#include "L2eventStream2008.h"
   
class L2EmcDb;
class L2Histo;
class L2VirtualAlgo2008 { 
 public:
  enum EmcSwitch { kIsBad=0, kIsBtow, kIsEtow, kIsB_Etow }; // changes action of some methods

 protected:  
  //..... main Barrel/Endcap/Both switch
   EmcSwitch mSwitch; // use enum above

  std::string mOutDir1, mName1;
  L2EmcDb *mDb;
  FILE    *mLogFile, *mHistFile;
  L2Histo *mhN; /*  Neve(case),  
		    bins: [0-4],[10-14], reserved for virtual08 algo
		    0 - # of input events
		    1 - # of calls computeUser()  
		    2 - # of calls decisionUser()  
		    3,4  - free
		    5...9 - user algo
		    10 - # of accepted events 
		    11..14 - free
		    15...19 - user algo
		*/
  L2Histo *mhTc,*mhTd,*mhTcd;/* time/eve : compute, decision, compute+decision */
  L2Histo *mhRc, *mhRd, *mhRa ; /*  rate/sec: compute, decision, accepted */
  int   oflTrigId; // important only for off-line analysis /w Makers
  int   mAccept;
  int   mEventsInRun;
  int   mSecondsInRun;
  int   mRunNumber;
  const  L2BtowCalibData08 *mEveStream_btow;
  const  L2EtowCalibData08 *mEveStream_etow;

  enum {par_cpuTicksPerSecond=1600000000};
  unsigned long  mComputeTimeStart,  mComputeTimeStop,  mComputeTimeDiff[L2eventStream2008::mxToken];
  unsigned long  mDecisionTimeStart, mDecisionTimeStop, mDecisionTimeDiff;
  unsigned long long mRunStartTicks;

  L2Histo **hA; // my private HBOOK@L2
  int mxHA; // set by user
  void setMaxHist(int k) {  assert(k>0);
    mxHA=k;  hA=new L2Histo *[mxHA];
  }
  int finishCommonHistos(); 

  void  computeStart();
  void  computeStop(int token);

 public:
  L2VirtualAlgo2008(const char* name, L2EmcDb* db, char*outDir);
  virtual ~L2VirtualAlgo2008(); //memory leak NOT taken care off
  void setOflTrigID(int x) {oflTrigId=x;} //only for Maker-analysis
  int  getOflTrigID() {return oflTrigId;} //only for Maker-analysis
  bool   isAccepted(){ return mAccept; } // only for Maker-analysis
  static int  readParams(const char *fileN, int mxPar, int *iPar, float *fPar);
  const char *getName() { return mName1.c_str();}

  int   initRun(int runNo, int *rc_ints, float *rc_floats);
  void  compute (int token);
  bool  decision(int token, void **myL2Result);
  void  finishRun();

  // implement algo specific operations in the virtual functions
  virtual int   initRunUser(int runNo, int *rc_ints, float *rc_floats){ return 0;}
  virtual void  computeUser(int token){};
  virtual bool  decisionUser(int token, void **myL2Result){myL2Result=0; return true;}
  virtual void  finishRunUser(){}// at the end of each run

  // read-only access to current 'global' event, updated in compute
  void printCalibratedData(int token);
};


#endif
