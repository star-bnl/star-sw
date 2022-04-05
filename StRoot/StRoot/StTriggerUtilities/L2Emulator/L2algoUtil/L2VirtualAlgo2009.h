#ifndef L2VirtualAlgo2009_h
#define L2VirtualAlgo2009_h


/*************************************************************
 * $Id: L2VirtualAlgo2009.h,v 1.6 2011/03/20 02:14:50 pibero Exp $
 * \author Jan Balewski, IUCF, 2006 
 *************************************************************
 * Descripion:
 * all actual L2 algos should inherit from it the 4 methods
 *************************************************************
 */

// A great suggestion from Pibero, to use ASM macro directly
#define rdtscl_macro(low) \
     __asm__ __volatile__("rdtsc" : "=a" (low) : : "edx")
 
#define rdtsc_macro(low,high) \
     __asm__ __volatile__("rdtsc" : "=a" (low), "=d" (high))


#include <string>
#include "L2eventStream2009.h"
 
class L2EmcDb;
class L2Histo;
class L2VirtualAlgo2009 { 
 public:
  enum EmcSwitch { kIsBad=0, kIsBtow, kIsEtow, kIsB_Etow }; // changes action of some methods
 protected:  
  bool algoIsOkay; //master 'this algorithm is working' flag.  
                   //Flipped false if it fails consistency checks or is missing anything it needs
  
  enum {kMaximumNumberOfDsmMasks=4}; //needed for running with old TCU only.
  bool useDsmMask; //needed for running with old TCU only.
  int nmasks; //needed for running with old TCU only.
  unsigned short DsmMask[kMaximumNumberOfDsmMasks][8]; //needed for running with old TCU only.

  //..... main Barrel/Endcap/Both switch
   EmcSwitch mSwitch; // use enum above

  std::string mOutDir1, mName1;
  L2EmcDb *mDb;
  FILE    *mLogFile, *mHistFile;
  L2Histo *mhN; /*  Neve(case),  
		    bins: [0-4],[10-14], reserved for virtual09 algo
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
  bool  mAccept, mRandomAccept;
  int   mEventsInRun;
  int   mSecondsInRun;
  int   mRunNumber;
  int   mResultOffset; // tells us where in L2Result we are allowed to write.

  int mRndAcceptCounter; //counter for random accept.
  int par_RndAcceptPrescale;//accept when counter%prescale=0, hence accept 1/prescale events. prescale=0==>don't rndaccept.

  bool  mNeeds_barrel;
  bool  mNeeds_endcap;
  const  L2BtowCalibData09 *mEveStream_btow;
  const  L2EtowCalibData09 *mEveStream_etow;

  enum {par_cpuTicksPerSecond=1600000000};
  unsigned long  mComputeTimeStart,  mComputeTimeStop,  mComputeTimeDiff[L2eventStream2009::mxToken];
  unsigned long  mDecisionTimeStart, mDecisionTimeStop, mDecisionTimeDiff;
  unsigned long long mRunStartTicks;

  L2Histo **hA; // my private HBOOK@L2
  int mxHA; // set by user
  void setMaxHist(int k) {  
    if (!(k>0)) {
      criticalError("setMaxHist called with negative argument.");
      k=0;
    }
    mxHA=k;  hA=new L2Histo*[mxHA];
    memset(hA,0,mxHA*sizeof(L2Histo*));
  }
  int finishCommonHistos(); 

  void  computeStart();
  void  computeStop(int token);
  unsigned short swap_bytes(unsigned short in);

 public:
  L2VirtualAlgo2009(const char* name, L2EmcDb* db, char*outDir, bool needsbarrel, bool needsendcap, int resOff);
  virtual ~L2VirtualAlgo2009(); //memory leak NOT taken care off
  void setOflTrigID(int x) {oflTrigId=x;} //only for Maker-analysis
  int  getOflTrigID() {return oflTrigId;} //only for Maker-analysis
  bool   isAccepted(){ return mAccept; } // only for Maker-analysis
  static int  readParams(const char *fileN, int mxPar, int *iPar, float *fPar);
  const char *getName() { return mName1.c_str();}

  int   initRun(int runNo, int *rc_ints, float *rc_floats);
  void  compute (int token);
  bool  decision(int token, bool barrel_is_in, bool endcap_is_in, int *myL2Result);
  void  finishRun();
  bool  checkDsmMask(unsigned short *lastDSM);
  int readDsmMask(const char *fileN);

  // implement algo specific operations in the virtual functions
  virtual int   initRunUser(int runNo, int *rc_ints, float *rc_floats){ return 0;}
  virtual void  computeUser(int token){};
  virtual bool  decisionUser(int token, int *myL2Result){return true;}
  virtual void  finishRunUser(){}// at the end of each run

  // read-only access to current 'global' event, updated in compute
  void printCalibratedData(int token);

  void criticalError(const char* message);
};


#endif
