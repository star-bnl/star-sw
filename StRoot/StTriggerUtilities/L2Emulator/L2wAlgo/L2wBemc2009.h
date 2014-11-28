#ifndef L2wBemc2008b_H
#define L2wBemc2008b_H
/*********************************************************************
 * $Id: L2wBemc2009.h,v 1.1 2009/03/28 19:43:53 balewski Exp $
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
#include "L2wResult2009.h"

class L2wBemc2009 ;
class L2Histo;
class L2EmcGeom;
// remember to clear in initRun() to avoid stale data
class L2wBemcEvent2009 {// WARN : all is in RAM x 4096 tokens!
 public:
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2wBemc2009 ;
  int   isFresh; // for QA    
  float clusterET;
  float seedET;
  unsigned long long tkCompute;
  L2wResult2009 resultBlob;
};

class L2wBemc2009 : public  L2VirtualAlgo2009 {
  /* this class fills the folowing bins
     of counter histo (mhN), see also L2VirtualAlgo2009.h
     6 - # of eve w/ STALE data - very bad, on input
  */
 public:
  enum {mxBtow=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; // shortcut
 private:

  //..................... params set in initRun
  int   par_dbg;
  float par_seedEtThres;
  float par_clustEtThres;

  //.............run-long, token independent variables
  L2EmcGeom *mGeom;// avaliable but not used in this example
  int mRdo2tower[mxBtow];
  int mTower2rdo[mxBtow];

  //---- event-long variables changed by COMPUTE() -----
  //...............  working, token independent 
  float wrkBtow_et[mxBtow]; // full event
  int   wrkBtow_tower_seed[mxBtow]; // above seed thresholds, not cleared
  int   wrkBtow_tower_seed_size;
 //............... preserved for Decision(),
  L2wBemcEvent2009 mBtow[L2eventStream2009::mxToken]; //it is a lot of RAM!
  
  // utility methods
  void  createHisto();
  void  clearEvent(int token);
  void  clearEvent();
  float sumET(int phi, int eta);
  
 public:
  L2wBemc2009(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir, int resOff);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int token);
  bool  decisionUser(int token, int *myL2Result);

  void print1(int token);
  void print2();
  void print3();
  void print4(int token, int hitSize);

};

#endif 

/**********************************************************************
  $Log: L2wBemc2009.h,v $
  Revision 1.1  2009/03/28 19:43:53  balewski
  2009 code



*/

