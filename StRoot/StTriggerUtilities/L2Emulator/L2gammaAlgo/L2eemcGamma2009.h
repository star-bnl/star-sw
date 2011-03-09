#ifndef L2eemcGamma2009_H
#define L2eemcGamma2009_H
/*********************************************************************
 * $Id: L2eemcGamma2009.h,v 1.1 2011/03/09 16:29:07 pibero Exp $
 * \author Jan Balewski, MIT, 2008 
 *********************************************************************
 * Descripion:
 *  example algo finding list of 2x2 ETOW clusters based on ET-seed list produced by L2-etow-calib algo
 * features: uses 2D array (ieta vs. iphi )
 * Limitations:
 *   - ignores seeds at the  edges
 *   - double counts for neighbours seeds
 *   - no provision for wrap up in phi
 *********************************************************************
 */


#ifdef IS_REAL_L2  //in l2-ana  environment
  #include "L2VirtualAlgo2009.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2009.h"
#endif

#include "L2gammaResult2009.h"

class L2eemcGamma2009 ;
class L2Histo;
class L2EmcGeom;

// remember to clear in initRun() to avoid stale data
class L2eemcGammaEvent2009 {// WARN : all is in RAM x 4096 tokens!
 public: 
  enum {mxClust=50}; // keep the size down
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2eemcGamma2009 ;
  int   isFresh; // for QA    
  int   size;// size of used data in the array below
  float clusterET[mxClust]; //above seed thresholds, not cleared
  float clusterEta[mxClust]; //Eta bin of cluster
  float clusterPhi[mxClust]; //Phi bin of cluster
  unsigned short int clusterSeedTow[mxClust]; //Seed Tower number
  unsigned short int clusterQuad[mxClust]; //Quadrant of cluster relative to seed tower [0,3]
  unsigned short int clusterSeedRank[mxClust]; //Rank of Seed for this cluster
  L2gammaResult2009 resultBlob;
};

class L2eemcGamma2009 : public  L2VirtualAlgo2009 {
  /* this class fills the folowing bins
     of counter histo (mhN), see also L2VirtualAlgo2009.h
     5 - # of eve w/ overflow # of clusters, on input
     6 - # of eve w/ STALE data - very bad, on input

     15 - # of eve w/ overflow # of clusters, accepted
  */
 public:
  enum {mxEtow=(EtowGeom::mxEtaBin) * (EtowGeom::mxPhiBin)}; // shortcut
 private:

  //..................... params set in initRun
  int   par_dbg;
  float par_seedEtThres;
  float par_clusterEtThres;
  float par_eventEtThres;

 
  //.............run-long, token independent variables
  L2EmcGeom *mGeom;// avaliable but not used in this example
  int mRdo2tower[mxEtow];
  int mTower2rdo[mxEtow];

  //---- event-long variables changed by COMPUTE() -----
  //...............  working, token independent 
  float wrkEtow_et[mxEtow]; // full event
  int   wrkEtow_tower_index[mxEtow]; // Tower numbers indexed by hit number
  int   wrkEtow_tower_seed[mxEtow]; // above seed thresholds, not cleared
  int   wrkEtow_tower_seed_size;
  //............... preserved for Decision(),
  L2eemcGammaEvent2009 mEtow[L2eventStream2009::mxToken]; //it is a lot of RAM!
  //............... event-long variables
  float etow_et[mxEtow]; // full event
  int  etow_used[mxEtow]; // flag set to 1 when tower has been "used" in a cluster
  int   etow_tower_seed[mxEtow]; // above seed thresholds, not cleared
  int   etow_tower_seed_size;
  float etow_clusterET[mxEtow]; // above seed thresholds, not cleared
  int   etow_clusterET_size;
 
  //............... cluster data type
  struct mCluster {
    int seedTower;
    int seedRank;
    int quadrant;
    float ETsum;
  };
 
  mCluster towerCluster[mxEtow];


  
  // utility methods
  void  createHisto();
  void  clearEvent(int token);
  void  clearEvent();
  int  countNonZeroTow(int phi, int eta);
  float sumET(int phi, int eta);
  void flagUsed(int phi, int eta);
  void averageEtaPhi(int phi, int eta, float *avePhi, float *aveEta);
  void quickSort(int array[], int start, int end);
  void swap(int array[], int index1, int index2);
  
 public:
  L2eemcGamma2009(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir, int resOff);
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
  $Log: L2eemcGamma2009.h,v $
  Revision 1.1  2011/03/09 16:29:07  pibero
  Added L2gamma2009

  Revision 1.5  2008/01/30 00:47:17  balewski
  Added L2-Etow-calib

  Revision 1.4  2008/01/18 23:29:13  balewski
  now L2result is exported

  Revision 1.3  2008/01/17 23:15:52  balewski
  bug in token-addressed memory fixed

  Revision 1.2  2008/01/16 23:32:36  balewski
  toward token dependent compute()

  Revision 1.1  2007/12/19 02:30:19  balewski
  new L2-etow-calib-2008



*/

