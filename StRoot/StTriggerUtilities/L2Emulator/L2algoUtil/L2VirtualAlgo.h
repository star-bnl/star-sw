#ifndef L2VirtualAlgo_h
#define L2VirtualAlgo_h


/*********************************************************************
 * $Id: L2VirtualAlgo.h,v 1.1 2007/10/11 00:33:15 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * all actual L2 algos should inherit from it the 4 methods
 *********************************************************************
 */


class L2EmcDb;
class TrgDataType;

class L2VirtualAlgo {
 protected:
  
 public:
  virtual ~L2VirtualAlgo()=0; // memory leak NOT taken care off
  virtual int   initRun(char* myName, int runNo, int *rc_ints, float *rc_floats)=0;
  virtual bool  doEvent(int  L0trg, int inpEveId, TrgDataType* trgData, 
                        int  bemcIn, ushort *bemcData,
                        int  eemcIn, ushort *eemcData)=0;
  virtual void  finishRun()=0;// at the end of each run
};


#endif
