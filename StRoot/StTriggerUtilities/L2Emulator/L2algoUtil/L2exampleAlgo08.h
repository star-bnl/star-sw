#ifndef L2exampleAlgo08_H
#define L2exampleAlgo08_H
/*********************************************************************
 * $Id: L2exampleAlgo08.h,v 1.5 2008/01/30 00:47:17 balewski Exp $
 * \author Jan Balewski, MIT, 2008 
 *********************************************************************
 * Descripion:
 *  example algo finding list of 2x2 BTOW clusters based on ET-seed list produced by L2-btow-calib algo
 * features: uses 2D array (ieta vs. iphi )
 * Limitations:
 *   - ignores seeds at the  edges
 *   - double counts for neighbours seeds
 *   - no provision for wrap up in phi
 *********************************************************************
 */


#include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2008.h"
#include "L2exampleResult08.h"

class L2exampleAlgo08 ;
class L2Histo;
class L2EmcGeom;

// remember to clear in initRun() to avoid stale data
class L2exampleEvent08 {// WARN : all is in RAM x 4096 tokens!
 public: 
  enum {mxClust=50}; // keep the size down
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2exampleAlgo08 ;
  int   isFresh; // for QA    
  int   size;// size of used data in the array below
  float clusterET[mxClust]; //above seed thresholds, not cleared
  L2exampleResult08 resultBlob;
};

class L2exampleAlgo08 : public  L2VirtualAlgo2008 {
  /* this class fills the folowing bins
     of counter histo (mhN), see also L2VirtualAlgo2008.h
     5 - # of eve w/ overflow # of clusters, on input
     6 - # of eve w/ STALE data - very bad, on input

     15 - # of eve w/ overflow # of clusters, accepted
  */
 public:
  enum {mxBtow=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; // shortcut
 private:

  //..................... params set in initRun
  int   par_dbg;
  float par_seedEtThres;
  float par_clusterEtThres;
  float par_eventEtThres;

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
  L2exampleEvent08 mBtow[L2eventStream2008::mxToken]; //it is a lot of RAM!
  
  // utility methods
  void  createHisto();
  void  clearEvent(int token);
  float sumET(int phi, int eta);
  
 public:
  L2exampleAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int token);
  bool  decisionUser(int token, void **myL2Result);

  void print1(int token);
  void print2();
  void print3();

};

#endif 

/**********************************************************************
  $Log: L2exampleAlgo08.h,v $
  Revision 1.5  2008/01/30 00:47:17  balewski
  Added L2-Etow-calib

  Revision 1.4  2008/01/18 23:29:13  balewski
  now L2result is exported

  Revision 1.3  2008/01/17 23:15:52  balewski
  bug in token-addressed memory fixed

  Revision 1.2  2008/01/16 23:32:36  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:19  balewski
  new L2-btow-calib-2008



*/

