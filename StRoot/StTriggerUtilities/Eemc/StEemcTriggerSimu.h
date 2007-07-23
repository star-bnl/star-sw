//
//  StEemcTriggerSimu.h,v 0.01
//
#ifndef STAR_StEemcTriggerSimu
#define STAR_StEemcTriggerSimu

#include <TObject.h> 
#include <vector>

class StEEmcDbMaker;
class EEfeeTPTree;
class EEdsm0Tree;
class EEdsm1Tree;
class EMCdsm2Tree;
class EEdsm3;

class TH1;
class StEemcTriggerSimu {
 private:
  StEEmcDbMaker    *mDbE;
  int mMCflag; // set yo 0 for real data
  int * mBemcEsum5bit; // output from Bemc emulation
  int * mExternDsmSetup;

  int  eveId; 
  int mYear;
  int nInpEve;    
  bool mDumpEve;
  void getEemcAdc();
  void readPed4(char *path, int crate);
  void getDsm0123inputs();

  enum {mxAH=1000};
  TObjArray  *mHList; // output histo access point
  TH1* hA[mxAH];
  void initHisto();

  //  local event storage
  enum {mxCr=6, mxChan=128};
  int rawAdc [mxCr*mxChan]; // *** 'chan' is counting faster***,
  int feePed [mxCr*mxChan]; // do NOT change this memory allocation scheme w/o 
  int feeMask[mxCr*mxChan];// understanding details  of  EEfeeTPTree::compute()
 
  //   Endcap FEE_TP+DSM0...3 Tree emulators processing ADC
 public:
  EEfeeTPTree *feeTPTreeADC;
  EEdsm0Tree  * dsm0TreeADC;
  EEdsm1Tree  * dsm1TreeADC; 
  EMCdsm2Tree * dsm2TreeADC;

 private:
  // Endcap DSM0...3 Tree only unpacking TRG data, for QA
  EEdsm0Tree  *dsm0TreeTRG;
  EEdsm1Tree  *dsm1TreeTRG;
  EMCdsm2Tree *dsm2TreeTRG;
  EEdsm3      *dsm3TRG;

  // various QA methods validating inputs to DSMs
  void compareTRG0_TRG1();
  void compareTRG1_TRG2();
  void compareTRG2_TRG3();

  void compareADCfee_TRG0();
  void compareADC0_TRG1();
  void compareADC1_TRG2();
  void compareADC2_TRG3();
  
  void DSM2EsumSpectra();

 public:

  StEemcTriggerSimu();
  virtual     ~StEemcTriggerSimu();
  void Init();
  void InitRun();
  void setMC(int x) {mMCflag=x;}
  void setDsmSetup(int *x){ mExternDsmSetup=x;}
  void Clear();
  void Make();
  void getEemcFeeMask();
  void setHList(TObjArray * x){mHList=x;}
  void addTriggerList( void * );
  void connectBemcL0(int  *x) { mBemcEsum5bit=x;};

  ClassDef(StEemcTriggerSimu, 1)
 };


#endif

//
// $Log: StEemcTriggerSimu.h,v $
// Revision 1.2  2007/07/23 03:00:00  balewski
// cleanup, bbc for M-C still not working
//
