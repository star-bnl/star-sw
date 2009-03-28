#ifndef L2wEemc2009_H
#define L2wEemc2009_H
/*********************************************************************
 * $Id: L2wEemc2009.h,v 1.1 2009/03/28 19:43:53 balewski Exp $
 * \author Jan Balewski, MIT, 2008 
 *********************************************************************
 * Descripion:
 *  example algo finding list of 2x2 BTOW clusters based on ET-seed list produced by L2-btow-calib algo
 * features: uses 2D array (ieta vs. iphi )
 * Limitations:
 *   - ignores seeds at the  edges
 *   - double processing  for neighbours seeds
 *********************************************************************
 */

#ifdef IS_REAL_L2  //in l2-ana  environment
  #include "L2VirtualAlgo2009.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2009.h"
#endif

class L2Histo;
class L2EmcGeom;

class L2wEemc2009 : public  L2VirtualAlgo2009 {
 private:

  //..................... params set in initRun
  int   par_dbg;
  float par_EtThresh;

 //............... preserved for Decision(),
  float highestEt[L2eventStream2009::mxToken]; 
  
  // utility methods
  void  createHisto();
  
 public:
  L2wEemc2009(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir, int resOff);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int token);
  bool  decisionUser(int token, int *myL2Result);
};

#endif 

/**********************************************************************
  $Log: L2wEemc2009.h,v $
  Revision 1.1  2009/03/28 19:43:53  balewski
  2009 code



*/

