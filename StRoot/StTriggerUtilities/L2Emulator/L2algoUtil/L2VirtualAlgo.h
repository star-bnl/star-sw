#ifndef L2VirtualAlgo_h
#define L2VirtualAlgo_h


/*********************************************************************
 * $Id: L2VirtualAlgo.h,v 1.5 2007/11/02 17:43:02 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * all actual L2 algos should inherit from it the 4 methods
 *********************************************************************
 */

//#include "/usr/src/kernels/2.6.9-42.0.10.EL-smp-i686/include/asm-i386/msr.h" /* for rdtscl */
// Great suggestion from Pibero, to use ASM macro directly
#define rdtscl_macro(low) \
     __asm__ __volatile__("rdtsc" : "=a" (low) : : "edx")


class L2EmcDb;
class TrgDataType;

class L2VirtualAlgo {
  enum {mxTxt=1000};
 protected:  
  char mName[mxTxt];    
  char mOutDir[mxTxt];
  L2EmcDb* mDb;
  int mResultOffset;
  
 public:
  L2VirtualAlgo(const char* name, L2EmcDb* db, char* outDir, int resOff);
  virtual ~L2VirtualAlgo()=0; // memory leak NOT taken care off
  virtual int   initRun(int runNo, int *rc_ints, float *rc_floats)=0;
  virtual bool  doEvent(int  L0trg, int inpEveId, TrgDataType* trgData, 
                        int  bemcIn, unsigned short *bemcData,
                        int  eemcIn, unsigned short *eemcData)=0;
  virtual void  finishRun()=0;// at the end of each run
  static int  readParams(const char *fileN, int mxPar, int *iPar, float *fPar);
  const char* name() const { return mName; }
};


#endif
