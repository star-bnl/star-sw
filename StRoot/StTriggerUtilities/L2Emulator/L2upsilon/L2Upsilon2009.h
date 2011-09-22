#ifndef L2Upsilon2009_H
#define L2Upsilon2009_H

#ifdef IS_REAL_L2  //in l2-ana  environment
  #include "L2VirtualAlgo2009.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2009.h"
#endif
#include "L2UpsilonResult2009.h"

class L2Upsilon2009 ;
class L2Histo;
class L2EmcGeom;
// remember to clear in initRun() to avoid stale data
class L2UpsilonEvent2009 {// WARN : all is in RAM x 4096 tokens!
 public:
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2Upsilon2009 ;
  int   isFresh; // for QA    
  L2UpsilonResult2009 resultBlob;
};

class L2Upsilon2009 : public  L2VirtualAlgo2009 {
 public:
  enum {mxBtow=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; // shortcut
 private:

  //..................... params set in initRun
	int   par_prescale;
	int fMaxDynamicMaskTowers;
	int fHowManyEventPerUpdateDynamicMask;
	int fHotTowerSeenTimesThreshold;
	float fMinL0ClusterEnergy,fMinL2ClusterEnergy;
	float fMinInvMass,fMaxInvMass,fMaxCosTheta;
	float fL0SeedThreshold,fL2SeedThreshold;
	float fHotTowerThreshold;
	float fThresholdRatioOfHotTower;
	
	int prescale;
  //.............run-long, token independent variables
	L2EmcGeom *mGeom;// avaliable but not used in this example
	int mRdo2tower[mxBtow];
	int mTower2rdo[mxBtow];
	int rdo2softID[mxBtow+1];
    int event_accept; 
    int EventSeen;

  //---- event-long variables changed by COMPUTE() -----
  float wrkBtow_ene[mxBtow+1];
  int wrkBtow_tower[mxBtow+1];
  int wrkL2_seed_tower[mxBtow+1];   //store tower ID   [0~wrkNumberOfL0]
  float wrkL2_seed_ClusterE[mxBtow+1];
  int wrkL0_seed_tower[mxBtow+1];  //store tower ID   [0~wrkNumberOfL2]
  float wrkL0_seed_ClusterE[mxBtow+1];
  int wrkNumberOfL2;
  int wrkNumberOfL0;
 
  int wrkDynamicMask_tower_stat[mxBtow+1];  //status of tower ID (use to judge hot towers)
  int wrkDynamicMasked_tower[101];  //tower ID
  int wrkNumberOfMasked;   // howmany tower have been masked

 //............... preserved for Decision(),
  L2UpsilonEvent2009 mBtow[L2eventStream2009::mxToken]; //it is a lot of RAM!
  
  // utility methods
  void  createHisto();
  void  clearEvent(int token);
  void  clearEvent();
  void update_DynamicMask();
  
 public:
  L2Upsilon2009(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir, int resOff);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int token);
  bool  decisionUser(int token, int *myL2Result);
};

#endif 

/**********************************************************************
  $Log: L2Upsilon2009.h,v $
  Revision 1.1  2011/09/22 20:46:52  pibero
  *** empty log message ***

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

