#ifndef L2hienAlgo08_H
#define L2hienAlgo08_H
/*********************************************************************
 * $Id: L2hienAlgo08.h,v 1.1 2008/01/29 00:17:21 balewski Exp $
 * \author Jan Balewski, MIT, 2008 
 *********************************************************************
Descripion:
this algo selecting high-energy towers  from  BTOW & ETOW data
 and taking advantage of common calibration to be deployed
 at L2 in February of 2008.
The common (for B & E) ADC threshold will be defined
 in units of adc above  ped i.e. in ET.
The output is list of pairs {adc,softID} from both E & B
There is a hardcoded limit on the max list length of 256 towers.
In case of an overflow the _random_ first 100 towers (software limit)
 will be  recorded  
 *********************************************************************
 */


#include "StTriggerUtilities/L2Emulator/L2algoUtil/L2VirtualAlgo2008.h"

class L2hienAlgo08 ;
class L2Histo;
class L2EmcGeom;

// remember to clear in initRun() to avoid stale data
class L2hienEvent08 {// WARN : all sits in RAM x 4096 tokens!
 public: 
  enum {mxListSize=150}; // keep this size small
  enum {kDataFresh=0}; // if used 1 or more times data are stale
 private:
  friend class L2hienAlgo08 ;
  int   isFresh; // for QA    
  int   size;// # of elements on the list below
  unsigned int value[mxListSize]; //list, not cleared
};

class L2hienAlgo08 : public  L2VirtualAlgo2008 {
  /* this class fills the folowing bins
     of counter histo (mhN), see also L2VirtualAlgo2008.h
     5 - # of eve w/ overflow # of towers, in decision() input
     6 - # of eve w/ STALE data - very bad, in decision() input

  */
 public:
  enum {mxBtow=(BtowGeom::mxEtaBin) * (BtowGeom::mxPhiBin)}; // shortcut
 private:

  //..................... params set in initRun
  int   par_dbg;
  int   par_maxList;
  int   par_adcThres;

  int mRdo2towerID[mxBtow];
  int mTowerID2etaBin[mxBtow];
  int mTowerID2phiBin[mxBtow];

  //---- event-long variables changed by COMPUTE() -----
  //............... preserved for Decision(),
  L2hienEvent08 mHiEnTw[L2eventStream2008::mxToken]; //it is a lot of RAM!
  // add similar for the endcap
  
  // utility methods
  void  createHisto();
  void  clearEvent(int token);
  
 public:
  L2hienAlgo08(const char* name, L2EmcDb* db, L2EmcGeom *geo, char* outDir);
  int   initRunUser( int runNo, int *rc_ints, float *rc_floats);
  void  finishRunUser();// at the end of each run
  void  computeUser(int token);
  bool  decisionUser(int token, void **myL2Result);//only QA
  // expert only, to export high towers 
  int   getListSize(int token) { return mHiEnTw[token].size;}
  const unsigned int  *getListData(int token) { return mHiEnTw[token].value;}
  
  void print2(int token);
 
};

#endif 

/****************************************************
  $Log: L2hienAlgo08.h,v $
  Revision 1.1  2008/01/29 00:17:21  balewski
  new algo filtering high-energy towers




*/

