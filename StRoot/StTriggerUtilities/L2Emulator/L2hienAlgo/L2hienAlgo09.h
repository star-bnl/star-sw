#ifndef L2hienAlgo09_H
#define L2hienAlgo09_H
/*****************************************************
 * $Id: L2hienAlgo09.h,v 1.1 2011/09/22 20:44:13 pibero Exp $
 * \author Jan Balewski, MIT, 2008 
 *****************************************************
Descripion:
this algo selects high-energy towers  from  BTOW & ETOW data
 and take advantage of common calibration to be deployed
 at L2 in February of 2008.
The common (for B & E) ADC threshold is defined
 in units of adc above  ped i.e. in ET.
The output of this algo _one_ lists of pairs {adc,softID}.
You need 2 instances of this ago to cover E & B-EMC.
Saving of those lists to disk is beyond the scope of this algo.
SoftID is defined as follows:
*BTOW : traditional softID [1-4800] used since 20th centry.
*ETOW: range [0..719], eta index changes slower
    int ieta= (x->eta-1);
    int iphi= (x->sec-1)*EtowGeom::mxSubs + x->sub-'A' ;
    int softId= iphi+EtowGeom::mxPhiBin*ieta;

There is a hardcoded limit on the max list length at 256 towers.
In case of an overflow a random (not realy) selection of towers  will be  added to the list until the software limit is reached. 
******************************************************
*/

#ifdef  IS_REAL_L2  //in l2-ana  environmen
  #include "L2VirtualAlgo2009.h"
#else
  #include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2009.h"
#endif

#include "L2hienResult2009.h"

class L2hienAlgo09 ;
class L2Histo;
class L2EmcGeom;

// remember to clear in initRun() to avoid stale data
class L2hienList09 {// WARN : all sits in RAM x 4096 tokens!
 public: 
  enum {mxListSize=150}; // keep this size small
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2hienAlgo09 ;
  int   isFresh; // for QA    
  int   size;// # of elements on the list below
  unsigned int value[mxListSize]; //list, not cleared
};

//-------------------------------
class L2hienAlgo09 : public  L2VirtualAlgo2009 {
  /* this class fills the folowing bins
     of counter histo (mhN), see also L2VirtualAlgo2008.h
     5 - # of eve w/ overflow # of towers, in decision() input
     6 - # of eve w/ STALE data - very bad, in decision() input

  */
 public:
  // shortcuts
  enum {mxBtow=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; 
  enum {mxEtow=(EtowGeom::mxEtaBin) * (EtowGeom::mxPhiBin)};
 private:
  //..................... params set in initRun
  int  par_dbg;
  int  par_maxList;
  int  par_adcThres; // in ADC counts above peds

  //...................  BTOW lookup tables
  int  mRdo2towerID_B[mxBtow];
  int  mTowerID2etaBin_B[mxBtow]; // for QA only
  int  mTowerID2phiBin_B[mxBtow]; // for QA only

  //...................  ETOW lookup tables
  int  mRdo2towerID_E[mxEtow];
  int  mTowerID2etaBin_E[mxEtow]; // for QA only
  int  mTowerID2phiBin_E[mxEtow]; // for QA only

  //---- event-long variables changed by COMPUTE() -----
  //............... preserved for Decision(),
  L2hienList09 mHiEnTw[L2eventStream2009::mxToken]; //it is a lot of RAM!
  L2hienResult2009 mHienResult[L2eventStream2009::mxToken]; //so is this!
  // add similar for the endcap
  
  // utility methods
  void  createHisto();
  
 public:
  L2hienAlgo09(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir, int resOff, L2VirtualAlgo2009::EmcSwitch  beSwitch);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int token);
  bool  decisionUser(int token, int *myL2Result);//only QA
  // expert only, to export high towers 
  int   getListSize(int token) { return mHiEnTw[token].size;}
  const unsigned int  *getListData(int token) { return mHiEnTw[token].value;}
  
  void print2(int token);
 
};

#endif 

/****************************************************
  $Log: L2hienAlgo09.h,v $
  Revision 1.1  2011/09/22 20:44:13  pibero
  *** empty log message ***

  Revision 1.3  2008/02/01 00:16:44  balewski
  add mxListSize to BTOW/ETOW calibration

  Revision 1.2  2008/01/30 21:56:43  balewski
  E+B high-enery-filter L2-algo fuly functional

  Revision 1.1  2008/01/29 00:17:21  balewski
  new algo filtering high-energy towers




*/

