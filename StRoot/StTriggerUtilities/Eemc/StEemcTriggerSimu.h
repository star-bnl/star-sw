//
//  StEemcTriggerSimu.h,v 0.01
//
#ifndef STAR_StEemcTriggerSimu
#define STAR_StEemcTriggerSimu

#include <TObject.h> 
#include <vector>

#include "StTriggerUtilities/StVirtualTriggerSimu.h"

class StEEmcDb;
class EEfeeTPTree;
class EEdsm0Tree;
class EEdsm1Tree;
class EMCdsm2Tree;
class EEdsm3;
class EemcHttpInfo;
class TH1;

// #### modified by Liaoyuan ####

class DSMLayer_E001_2009;
class DSMLayer_E101_2009;

// #### modified end ####

class StEemcTriggerSimu : public StVirtualTriggerSimu {
 public:
  enum {kOnlyAdc=0,kAdcAndTrig, kAdcCompareTrig};
  void setConfig(int x) {mConfig=x;}
 private:
  StEEmcDb *mDbE;
  int * mBemcEsum5bit; // output from Bemc emulation, not working
  int * mExternDsmSetup;
  TString  mSetupPath;

  int mConfig; // see enum
  enum {nThr=3};
  int mHTthr[nThr], mTPthr[nThr],mHTTPthrSelc;

  int  eveId; 
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
 public:
  EMCdsm2Tree * dsm2TreeADC;
  
  // #### modified by Liaoyuan ####
  // 2009 Endcap DSM Layer 0 + 1
 private:
  DSMLayer_E001_2009* mE001;
  DSMLayer_E101_2009* mE101;

  void get2009_DSMLayer0();	// Reads output from feeTPTreeADC & process
  void get2009_DSMLayer1();     // Reads output from mE101 & process
  int  get2009_DSMRegisters(int runNumber);

  // Access to 2009 EEMC Layer 0 + 1
 public:
  DSMLayer_E001_2009* get2009_DSMLayer0_Result() { return mE001; };
  DSMLayer_E101_2009* get2009_DSMLayer1_Result() { return mE101; };

  // #### modified end ####
  
  

 private:
  // Endcap DSM0...3 Tree only unpacking TRG data, for QA
  EEdsm0Tree  *dsm0TreeTRG;
  EEdsm1Tree  *dsm1TreeTRG;
  EMCdsm2Tree *dsm2TreeTRG;
  EEdsm3      *dsm3TRG;
 private:

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
  void InitRun(int runnumber);
  void setDsmSetup(int *x){ mExternDsmSetup=x;} // discard
  void setSetupPath(char *x) { mSetupPath=x;}
  void Clear();
  void Make();
  void getEemcFeeMask();
  void setHList(TObjArray * x){mHList=x;}
  void addTriggerList( void * );
  StTriggerSimuDecision triggerDecision(int trigId);
  void connectBemcL0(int  *x) { mBemcEsum5bit=x;};
  bool getHttpInfo(int tpId, EemcHttpInfo &httpInfo);
  ClassDef(StEemcTriggerSimu, 1)
 };


#endif

//
// $Log: StEemcTriggerSimu.h,v $
// Revision 1.13  2009/09/20 06:46:42  pibero
// Updates for Run 9
//
// Revision 1.12  2009/02/20 23:40:04  pibero
// Updates for Run 9 by Liaoyuan
//
// Revision 1.11  2009/02/04 20:27:10  rfatemi
// Update StEemcDbMaker
//
// Revision 1.10  2007/11/08 20:59:53  kocolosk
// subdet isTrigger returns a bool
// triggerDecision returns enumerator including kDoNotCare
//
// Revision 1.9  2007/10/22 23:09:54  balewski
// split L2 to generic and year specific, not finished
//
// Revision 1.8  2007/10/12 20:11:33  balewski
// cleanup of setup path, now at inst/iucf
//
// Revision 1.7  2007/10/12 17:12:43  kocolosk
// rename ABC class for subdetector trigger simulators
// StTriggerSimu => StVirtualTriggerSimu
//
// Revision 1.6  2007/10/12 16:41:04  balewski
// *** empty log message ***
//
// Revision 1.5  2007/10/11 00:33:03  balewski
// L2algo added
//
// Revision 1.4  2007/09/24 18:08:43  kocolosk
// added inheritance from ABC clss StTriggerSimu
//
// Revision 1.3  2007/07/24 01:32:59  balewski
// added HTTP id for the endcap
//
// Revision 1.2  2007/07/23 03:00:00  balewski
// cleanup, bbc for M-C still not working
//
