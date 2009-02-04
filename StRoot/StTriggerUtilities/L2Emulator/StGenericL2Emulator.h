// $Id: StGenericL2Emulator.h,v 1.13 2009/02/04 20:25:38 rfatemi Exp $

/* \class  StGenericL2Emulator
\author Jan Balewski

Interfaces L2 algos to the STAR ofl software

 */


#ifndef STAR_StGenericL2Emulator
#define STAR_StGenericL2Emulator
#include <vector>

class  StTriggerSimuMaker;
class  StEEmcDb;
class  StEmcGeom;
class  StEmcDecoder;
class  L2EmcDb;

#include "L2algoUtil/L2VirtualAlgo.h"
#include "StVirtualTriggerSimu.h"


class StGenericL2Emulator  {
 private:
  int  mTotInpEve;
  TString  mOutPath;
  TString  mSetPath;

  void doBanksFromStRawData();
  void doBanksFromMuDst();

  // needed to regenerate raw data banks
  StEEmcDb      *mDbE;
  StEmcGeom     *mGeomB;
  StEmcDecoder  *mMappB; 
  // StTriggerSimuMaker *mHeadMaker;
  //void setHeadMaker(StTriggerSimuMaker *maker) { mHeadMaker = maker; }

  
 protected:
  int   mMCflag; // set mcFlag=0 for real data
  int   mYear;
  bool  mUseMuDst;
  
  // holds all instantiated L2algos
  L2VirtualAlgo **mL2algo; // actual algos
  int mL2algoN;  //# of existing algos (time-stamp dependent)

  L2EmcDb   *mL2EmcDb;
  int  mYearMonthDay,mHourMinSec;
  TString  mSetupPath;

  //replicas of oryginal daq data containers, tmp open for export
  unsigned short *mBTOW_BANK;
  unsigned short *mETOW_BANK;
  int  mBTOW_in, mETOW_in;
  void *mTrigData; // I do not want to deal with this content here

  void  init();
  void  make();
  void  addTriggerList();
  void  initRun1(); // before algos are initialized
  void  initRun2(int runNo); // after algos are initialized
  void  finish(); 
  void  clear();

 public: 
  StGenericL2Emulator();
  virtual       ~StGenericL2Emulator();
  void printBEtowers();///<  hits in StEvent
  void printBEblocks();///<  regenerated banks

  unsigned short *getBtowBank(){return mBTOW_BANK;}
  unsigned short *getEtowBank(){return mETOW_BANK;}
  int getBtowIn(){return mBTOW_in;}
  int getEtowIn(){return mETOW_in;}

  void  useStEvent() {mUseMuDst=false;}
  void  setMC(int x=true) {mMCflag=x;}
  void  setSetupPath(char *x) { mSetupPath=x;}
  void  setOutPath(char *x)   { mOutPath=x;}

  vector <int> mAcceptTriggerList;
  vector <int> mVetoTriggerList;
  StTriggerSimuDecision  isTrigger(int trigId);
  
  /// bag of 64 bytes whose interpretation changes year-by-year
  const unsigned int* result() const;
  
  ClassDef(StGenericL2Emulator,0) 
};

#endif

// $Log: StGenericL2Emulator.h,v $
// Revision 1.13  2009/02/04 20:25:38  rfatemi
// Update class StEemcDb
//
// Revision 1.12  2009/01/26 15:09:07  fisyak
// Add missing (in ROOT 5.22) includes
//
// Revision 1.11  2008/01/17 01:56:52  kocolosk
// export 128-byte emulated L2Result
//
// Revision 1.10  2007/12/09 15:56:52  rfatemi
// allow BEMC to take adc from StEvent instead of MuDst
//
// Revision 1.9  2007/11/19 22:18:17  balewski
// most L2algos provide triggerID's
//
// Revision 1.8  2007/11/18 21:58:53  balewski
// L2algos triggerId list fixed
//
// Revision 1.7  2007/11/13 00:12:26  balewski
// added offline triggerID, take1
//
// Revision 1.6  2007/11/08 21:29:10  balewski
// now L2emu runs on M-C
//
// Revision 1.5  2007/11/06 22:07:20  balewski
// added timeStamp controlled L2 setup from Jason
//
// Revision 1.4  2007/11/02 17:42:56  balewski
// cleanup & it started to work w/ L2upsilon
//
// Revision 1.3  2007/10/25 02:06:54  balewski
// added L2upsilon & binary event dump
//
// Revision 1.2  2007/10/23 02:47:11  balewski
// cleanup
//
// Revision 1.1  2007/10/22 23:09:58  balewski
// split L2 to generic and year specific, not finished
//
