#ifndef L2bemcGamma2012_H
#define L2bemcGamma2012_H
/*********************************************************************
 * $Id: L2bemcGamma2012.h,v 1.3 2011/10/19 16:12:11 jml Exp $
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

#ifdef IS_REAL_L2  //in l2-ana  environment
  #include "L2VirtualAlgo2012.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2012.h"
#endif

#include "L2gammaResult2012.h"

class L2bemcGamma2012 ;
class L2Histo;
class L2EmcGeom2012;
// remember to clear in initRun() to avoid stale data
class L2bemcGammaEvent2012 {// WARN : all is in RAM x 4096 tokens!
 public:
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2bemcGamma2012 ;
  int   isFresh; // for QA    
  float clusterET;
  float seedET;
  int seedTwID;
  L2gammaResult2012 resultBlob;
};

class L2bemcGamma2012 : public  L2VirtualAlgo2012 {
  /* this class fills the folowing bins
     of counter histo (mhN), see also L2VirtualAlgo2012.h
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


  //.............run-long, token independent variables
  L2EmcGeom2012 *mGeom;// avaliable but not used in this example
  int mRdo2tower[mxBtow];
  int mTower2rdo[mxBtow];

  //---- event-long variables changed by COMPUTE() -----
  //...............  working, token independent 
  float wrkBtow_et[mxBtow]; // full event
  int   wrkBtow_tower_seed[mxBtow]; // above seed thresholds, not cleared
  int   wrkBtow_tower_seed_size;
 //............... preserved for Decision(),
  L2bemcGammaEvent2012 mBtow[L2eventStream2012::mxToken]; //it is a lot of RAM!
  
  // utility methods
  void  createHisto();
  void  clearEvent(int token);
  void  clearEvent();
  float sumET(int phi, int eta);
  
 public:
  L2bemcGamma2012(const char* name, const char *uid, L2EmcDb2012* db, L2EmcGeom2012 *geo, char* outDir, int resOff);
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
  $Log: L2bemcGamma2012.h,v $
  Revision 1.3  2011/10/19 16:12:11  jml
  more 2012 stuff

  Revision 1.2  2011/10/19 15:39:43  jml
  2012

  Revision 1.1  2011/10/18 15:11:42  jml
  adding 2012 algorithms

  Revision 1.1  2011/03/09 16:29:07  pibero
  Added L2gamma2009

  Revision 1.6  2008/11/18 00:00:00  rcorliss
  Switched to 2009, updated as necessary

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

