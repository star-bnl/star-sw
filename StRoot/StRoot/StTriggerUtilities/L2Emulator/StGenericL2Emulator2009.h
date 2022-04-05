// -*- mode: c++ -*-
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 16 April 2010
// Original StGenericL2Emulator by Jan Balewski <balewski@mit.edu> and Renee Fatemi <rfatemi@pa.uky.edu>
// Interfaces L2 algos to the STAR ofl software

#ifndef STAR_StGenericL2Emulator2009
#define STAR_StGenericL2Emulator2009
#include <vector>
#include <set>

class  StTriggerSimuMaker;
class  StEEmcDb;
class  StEmcGeom;
class  StEmcDecoder;
class  L2EmcDb;
class  L2EmcGeom;

#include "L2algoUtil/L2VirtualAlgo2009.h"
#include "StVirtualTriggerSimu.h"


class StGenericL2Emulator2009  {
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
  vector<L2VirtualAlgo2009*> mL2algo; // actual algos

  L2EmcDb   *mL2EmcDb;
  L2EmcGeom *mL2EmcGeom;
  int  mYearMonthDay,mHourMinSec;
  TString  mSetupPath;

  //replicas of oryginal daq data containers, tmp open for export
  unsigned short *mBTOW_BANK;
  unsigned short *mETOW_BANK;
  int  mBTOW_in, mETOW_in;
  unsigned int mL2Result[128];

  void  init();
  void  make();
  void  addTriggerList();
  void  initRun1(); // before algos are initialized
  void  initRun2(int runNo); // after algos are initialized
  void  finish(); 
  void  clear();

 public: 
  StGenericL2Emulator2009();
  virtual       ~StGenericL2Emulator2009();
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

  set<int> mAcceptTriggerList;
  set<int> mVetoTriggerList;
  StTriggerSimuDecision  isTrigger(int trigId);
  
  /// bag of 64 bytes whose interpretation changes year-by-year
  const unsigned int* result() const { return mL2Result; }
  
  ClassDef(StGenericL2Emulator2009,0) 
};

#endif
