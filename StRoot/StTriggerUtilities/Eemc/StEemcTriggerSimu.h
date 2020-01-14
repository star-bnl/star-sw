// -*- mode: c++ -*-
// $Id: StEemcTriggerSimu.h,v 0.01
//
#ifndef STAR_StEemcTriggerSimu
#define STAR_StEemcTriggerSimu

#include <TObject.h>
#include <vector>
#include <map>

using std::vector;
using std::map;

#include "StTriggerUtilities/StVirtualTriggerSimu.h"

class StEEmcDb;
class EEfeeTPTree;
class EEdsm0Tree;
class EEdsm1Tree;
class EMCdsm2Tree;
class EEdsm3;
class EemcHttpInfo;
class TH1;
class TObjArray;

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
  int mPedMode;
  TString mPedFile;
  enum {nThr=3};
  int mHTthr[nThr], mTPthr[nThr],mHTTPthrSelc;

  int  eveId; 
  int nInpEve;    
  bool mDumpEve;
  void getEemcAdc();
  static int computePed4(float ped);
  void getDsm0123inputs();

  enum {mxAH=1000};
  TObjArray  *mHList; // output histo access point
  TH1* hA[mxAH];
  void initHisto();

  //  local event storage
  enum {mxCr=6, mxChan=128};
  int getRdo(int crate, int chan) const { return (crate-1)*mxChan+chan; }
  int getCrate(int rdo) const { return rdo/mxChan+1; }
  int getChannel(int rdo) const { return rdo%mxChan; }
  int rawAdc [mxCr*mxChan]; // *** 'chan' is counting faster***,
  int feePed [mxCr*mxChan]; // do NOT change this memory allocation scheme w/o 
  int feeMask[mxCr*mxChan];// understanding details  of  EEfeeTPTree::compute()
  int highTowerMask[90]; // mask applied to high tower output of FEE
  int patchSumMask[90]; // mask applied to patch sum output of FEE
  float ped[mxCr*mxChan];
  float gain[mxCr*mxChan];

public: 
  //  pedestal treatment
  enum { kOnline, kOffline, kLocal };
  void setPedMode(int pedmode) { mPedMode = pedmode; }
  void setPedFile(const char* pedfile) { mPedFile = pedfile; }

  //   Endcap FEE_TP+DSM0...3 Tree emulators processing ADC
  EEfeeTPTree *feeTPTreeADC;
  EEdsm0Tree  * dsm0TreeADC;
  EEdsm1Tree  * dsm1TreeADC; 
  EMCdsm2Tree * dsm2TreeADC;
  
  // #### modified by Liaoyuan ####
  // 2009 Endcap DSM Layer 0 + 1
 private:
  DSMLayer_E001_2009* mE001;
  DSMLayer_E101_2009* mE101;

  void get2009_DSMLayer0();	// Reads output from feeTPTreeADC & process
  void get2009_DSMLayer1();     // Reads output from mE101 & process
 
  // Access to 2009 EEMC Layer 0 + 1
 public:
  DSMLayer_E001_2009* get2009_DSMLayer0_Result() { return mE001; };
  DSMLayer_E101_2009* get2009_DSMLayer1_Result() { return mE101; };

  // #### modified end ####
  
  vector<int> mTriggerIds;
  bool mTestMode;
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
  bool isCorrupted() const;
  void getEemcFeeMask();
  void setHList(TObjArray * x){mHList=x;}
  void addTriggerList(vector<int>& trgList);
  StTriggerSimuDecision triggerDecision(int trigId);
  const vector<int>& triggerIds() const { return mTriggerIds; }
  void connectBemcL0(int  *x) { mBemcEsum5bit=x;};
  bool getHttpInfo(int tpId, EemcHttpInfo &httpInfo);

  int endcapJetPatchTh(int i) const;
  int endcapHighTowerTh(int i) const;

  int endcapJetPatchAdc(int jp) const;
  int endcapHighTowerAdc(int towerId) const { return 0; }

  int getOutHT(int tp) const;
  int getOutTPsum(int tp) const;
  int getEndcapHighTower(int tp) const { return getOutHT(tp); }
  int getEndcapPatchSum(int tp) const { return getOutTPsum(tp); }

  // Fill StEmcTriggerDetector in MuDst for MC
  void fillStEmcTriggerDetector();

  ClassDef(StEemcTriggerSimu, 1)
 };


#endif

//
// $Log: StEemcTriggerSimu.h,v $
// Revision 1.29  2020/01/13 20:45:51  zchang
// removing old run13 dsm algo files
//
// Revision 1.28  2017/01/02 15:31:56  rfatemi
// Updated by Danny OLVITT for 2013 dijet analysiss
//
// Revision 1.27  2012/08/27 17:16:41  pibero
// Add logging of EEMC gains from DB
//
// Revision 1.26  2011/10/22 20:25:17  pibero
// Add getters for output of EEMC FEEs for backward-compatibility
//
// Revision 1.25  2011/10/16 17:41:59  pibero
// Implement EEMC FEE HT & TP masks
//
// Revision 1.24  2011/10/14 22:33:45  pibero
// Add functions to test for data corruption in calorimeters
//
// Revision 1.23  2011/09/22 15:55:21  pibero
// Added EEMC pedestal modes:
//
// kOnline  = use online pedestals
// kOffline = use offline pedestals
// kLocal   = use pedestals from a local file (format: crate channel pedestal ped4)
//
// Revision 1.22  2011/09/20 13:32:43  pibero
// Added support for using EEMC offline pedestals (default)
//
// Revision 1.21  2010/06/29 16:53:27  pibero
// Now, the trigger simulator fills in the StEmcTriggerDetector structure
// same as data for MC.
//
// Revision 1.20  2010/06/24 07:51:21  pibero
// Added hooks to overwrite DSM thresholds from the database.
//
// Revision 1.19  2010/04/16 01:47:46  pibero
// Oops, forgot to include triggers before 2009. Thanks, Liaoyuan.
//
// Revision 1.18  2010/03/01 18:48:42  pibero
// More updates for Run 9
//
// Revision 1.17  2010/02/18 20:07:10  pibero
// Run 9 updates
//
// Revision 1.16  2009/12/22 18:11:05  pibero
// Added ability to set input source (MuDst or StEvent) for BBC trigger simulator.
//
// Revision 1.15  2009/12/15 16:33:33  pibero
// Added support to set thresholds manually for Run 9
// and overwrite those from the database.
//
// Revision 1.14  2009/12/08 02:06:59  pibero
// Add support for StEvent when running in BFC.
//
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
