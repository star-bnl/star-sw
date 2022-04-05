// -*- mode:c++ -*-

#ifndef STAR_StBemcTriggerSimu
#define STAR_StBemcTriggerSimu

#include "TObject.h"
#include "TString.h"
#include <utility>
#include <vector>
#include <set>
#include <map>
#include "TDatime.h"

using std::pair;
using std::vector;
using std::map;

#include "StTriggerUtilities/StVirtualTriggerSimu.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096
#define kNCrates 30
#define kNChannels 160
#define kNSeq 10
#define kL0DsmModule 30
#define kL0DsmInputs 10
#define kNLayer0out 36
#define kL1DsmModule 6
#define kL1DsmInputs 6
#define kL2DsmModule 1

class StEvent;
class StEmcDecoder;
class StEmcGeom;
class StEmcADCtoEMaker;
class StBemcTables;
class St_db_Maker;
class StBemcTriggerDbThresholds;
class StTriggerSimuMaker;
class TDataSet;
class St_db_Maker;
class TList;
class TH2;

class DSMLayer_B001_2009;
class DSMLayer_B101_2009;

class StBemcTriggerSimu : public StVirtualTriggerSimu {
private:
  std::vector<int>  mFiredTriggers;
  std::set<int>     mAllTriggers;
  
  std::vector<int> layer0;
  std::vector<int> layer1;
  std::vector<int> layer2;

  TObjArray *mHList;
 
  // pointers to useful objects we own
  StEmcDecoder *mDecoder;
  StBemcTriggerDbThresholds *mDbThres;
  
  // pointers to useful objects -- but we don't own them
  StEvent *mEvent;
  StEmcGeom *mGeo;
  StEmcADCtoEMaker* mAdc2e;
  StBemcTables *mTables;
  St_db_Maker *starDb;
  StTriggerSimuMaker *mHeadMaker;
  int mConfig; // see enum  
  TString mBemcStatus;
  
  // DB information
  Int_t HT_FEE_Offset;                     //same as bitConvValue but set by support class
  Int_t DSM_HTStatus[kNPatches];           //DSM_HTStatus only set online
  Int_t DSM_TPStatus[kNPatches];           //DSM_TPStatus only set online
  Int_t TowerStatus[kNTowers];             //tower status as determined online or offline
  Float_t TowerGain[kNTowers];             //tower gain
  unsigned long bitConvValue[kNTowers];    //gives window used to determine HT6Bit from adc10
  Int_t year,yyyymmdd,hhmmss;
  UInt_t timestamp;

  // BEMC mapping from BEMC decoder
  int TriggerPatchFromTowerId[kNTowers];
  int TriggerPatchFromCrate[kNCrates][kNSeq];

  Int_t adc12[kNTowers];                   //12 bit adc from StEvent -> NOT pedestal adjusted!
  Int_t adc10[kNTowers],adc08[kNTowers];   //ped adjusted 10 and 8 bit adc
  Float_t ped12[kNTowers];                 //12 and 10 bit pedestal
  Int_t FEEped[kNTowers];                  //FEE pedestal
  Int_t HTadc06[kNTowers];                 //6bit HT ADC for each tower
  
  unsigned long pedTargetValue;            //value FEE shifts pedestal to (12 bit)
  Float_t ped12Diff, ped10Diff;
  Int_t ped10DiffI;
  Int_t LUT[kNPatches];
  Int_t formula[kNCrates][kNSeq], numMaskTow[kNPatches]; 
  Int_t LUTscale[kNCrates][kNSeq], LUTped[kNCrates][kNSeq], LUTsig[kNCrates][kNSeq], LUTpow[kNCrates][kNSeq];
  char buffer[10];

  //HT/TP 6bit ADC out of FEE and into DSM Layer0
  Int_t L0_HT_ADC[kNPatches], L0_TP_ADC[kNPatches], L0_TP_PED[kNPatches];

  //MAX TP ADC, MAX HT ADC and SUM TPC ADC for each DSM Module
  Int_t DSM0_TP_SUM[kL0DsmModule], DSM0_TP_SUM_J1[kL0DsmModule], DSM0_TP_SUM_J3[kL0DsmModule];
  Int_t DSM1_JP_ADC[kL0DsmModule];

  //DSM0 Bits for HT, TP and HTTP
  //Only modules 2,7,12,17,22,27 go into J3 and J1
  //DSM_HT_Bit[2,7,12,17,22,27]==0
  Int_t DSM0_HT_Bit[kL0DsmModule],DSM0_TP_Bit[kL0DsmModule],DSM0_HTTP_Bit[kL0DsmModule];
  Int_t DSM0_HT_Bit_J3[kL0DsmModule],DSM0_TP_Bit_J3[kL0DsmModule],DSM0_HTTP_Bit_J3[kL0DsmModule];
  Int_t DSM0_HT_Bit_J1[kL0DsmModule],DSM0_TP_Bit_J1[kL0DsmModule],DSM0_HTTP_Bit_J1[kL0DsmModule];
  Int_t DSM0_HT_2Bit[kL0DsmModule],DSM0_HT_2Bit_J1[kL0DsmModule],DSM0_HT_2Bit_J3[kL0DsmModule];
  Int_t DSM0_HT_Thr3_Bit[kL0DsmModule], DSM0_HT_Thr3_Bit_J1[kL0DsmModule], DSM0_HT_Thr3_Bit_J3[kL0DsmModule];
  Int_t DSM0_HT_Masked_Bit[kL0DsmModule], DSM0_HT_Masked_Bit_J1[kL0DsmModule], DSM0_HT_Masked_Bit_J3[kL0DsmModule];

  //Intermediate bits for each trigger patch input to DSMLayer0
  Int_t DSM0_HT_tp_Bit[kL0DsmInputs], DSM0_TP_tp_Bit[kL0DsmInputs], DSM0_HTTP_tp_Bit[kL0DsmInputs];
  Int_t DSM0_HT_Thr3_tp_Bit[kL0DsmModule], DSM0_HT_Thr3_tp_Bit_J1[kL0DsmModule], DSM0_HT_Thr3_tp_Bit_J3[kL0DsmModule];
  Int_t DSM0_HT_tp_Bit_J3[kL0DsmInputs], DSM0_TP_tp_Bit_J3[kL0DsmInputs],DSM0_HTTP_tp_Bit_J3[kL0DsmInputs];
  Int_t DSM0_HT_tp_Bit_J1[kL0DsmInputs], DSM0_TP_tp_Bit_J1[kL0DsmInputs],DSM0_HTTP_tp_Bit_J1[kL0DsmInputs];
  Int_t L0_16bit_Out[kNLayer0out];
  //DSM1 Bits for JP, HT, TP and HTTP
  Int_t DSM1_JP_Bit[kL1DsmModule], DSM1_HT_Bit[kL1DsmModule], DSM1_TP_Bit[kL1DsmModule], DSM1_HTTP_Bit[kL1DsmModule];
  Int_t DSM1_HTj1_Bit[kL1DsmModule], DSM1_HTj0_Bit[kL1DsmModule], DSM1_HT3_Bit[kL1DsmModule];
  Int_t DSM1_ETOT_ADC[kL1DsmModule];
  //Intermediate bits for each jet patch constructed in DSMLayer1
  Int_t DSM1_JP_jp_Bit[kNJet];

  //Storage of all HT/TP/JP for get*AboveThreshold functions
  Int_t HT6bit_adc_holder[kNTowers];
  Int_t TP6bit_adc_holder[kNPatches];
  Int_t JP_adc_holder[kNJet];

  // J/psi topology trigger candidate pair of towers
  vector< pair<int,int> > mJpsiCandidates;

  // DSM layers for 2009
  DSMLayer_B001_2009* mB001;
  DSMLayer_B101_2009* mB101;

  void getTowerStatus();
  void getDSM_TPStatus();
  void getDSM_HTStatus();
  void getLUT();
  void getPed();
  void GetTriggerPatchFromCrate(int crate, int seq, int& triggerPatch) const;
  void FEEout();
  void FEEini2009();
  void FEEini2009(int runNumber);
  void FEEout2009();
  void switchoff(int& x, int n) const { x &= ~(1 << n); }
  void switchon (int& x, int n) const { x |=  (1 << n); }
  void simulateFEEfailure();
  void get2006_DSMLayer0();
  void get2006_DSMLayer1();
  void get2006_DSMLayer2();
  void get2006_JpsiCandidates(const vector<int>& towerIds1, const vector<int>& towerIds2);
  void get2007_DSMLayer0();
  void get2007_DSMLayer1();
  void get2007_DSMLayer2();
  void get2008dAu_DSMLayer0();
  void get2008dAu_DSMLayer1();
  void get2008dAu_DSMLayer2();
  void get2008pp_DSMLayer0();
  void get2008pp_DSMLayer1();
  void get2008pp_DSMLayer2();
  void get2009_DSMLayer0();
  void get2009_DSMLayer1();
  
  //#define DEBUG			// Comment out to switch off debugging

#ifdef DEBUG
  
  TH2 *mBEMCLayer0HT6bit;
  TH2 *mBEMCLayer0TP6bit;
  TH2 *mBEMCLayer0HT6bitDiff;
  TH2 *mBEMCLayer0TP6bitDiff;
  TH2 *mBEMCLayer1HTBits;
  TH2 *mBEMCLayer1HTBitsDiff;
  TH2 *mBEMCLayer1TPBits;
  TH2 *mBEMCLayer1TPBitsDiff;
  TH2 *mBEMCLayer1HTTPBits;
  TH2 *mBEMCLayer1HTTPBitsDiff;
  TH2 *mBEMCLayer1PatchSum;
  TH2 *mBEMCLayer1PatchSumDiff;
  TH2 *mBEMCLayer1HTmaskBits;
  TH2 *mBEMCLayer1HTmaskDiff;
  TH2 *mBEMCLayer1HTthr3Bits;
  TH2 *mBEMCLayer1HTthr3Diff;
  TH2 *mBEMCLayer2PatchSum;
  TH2 *mBEMCLayer2PatchSumDiff;
  TH2 *mBEMCLayer2HT3Bits;
  TH2 *mBEMCLayer2HT3BitsDiff;
  TH2 *mBEMCLayer2HTTPBits;
  TH2 *mBEMCLayer2HTTPBitsDiff;
  TH2 *mBEMCLayer2TPBits;
  TH2 *mBEMCLayer2TPBitsDiff;
  TH2 *mBEMCLayer2JPBits;
  TH2 *mBEMCLayer2JPBitsDiff;
  TH2 *mBEMCLayer2HTj0Bits;
  TH2 *mBEMCLayer2HTj0BitsDiff;
  TH2 *mBEMCLayer2HTj1Bits;
  TH2 *mBEMCLayer2HTj1BitsDiff;

#endif
  
public:
  StBemcTriggerSimu();
  virtual     ~StBemcTriggerSimu();

  void Init();
  void InitRun(int runnumber);                                              
  void Clear();
  void Make();
  
  bool isCorrupted() const;
  StTriggerSimuDecision triggerDecision(int trigId);
  const vector<int>& triggerIds() const { return mFiredTriggers; }

  void setHeadMaker(StTriggerSimuMaker *maker) { mHeadMaker = maker; }
  
  void setHList(TObjArray * x){mHList=x;}

  int barrelJetPatchTh(int i) const;
  int barrelHighTowerTh(int i) const;

  int barrelJetPatchAdc(int jp) const;
  int barrelHighTowerAdc(int towerId) const { return getHT6bitAdc(towerId); }

public:
  enum {kOnline=1, kOffline, kExpert};
  void setConfig(int x) {mConfig=x;}
  void setBemcStatus(const char* bemcStatus) { mBemcStatus = bemcStatus; }
  
  /// default tables come from emcSim or adc2e, but you can supply your own if you prefer
  void setTables(StBemcTables *tab) { mTables = tab; }
  StEmcDecoder* getDecoder() const { return mDecoder; }
  StBemcTables* getTables() const { return mTables; }
  
  //out of FEE into DSM layer0
  Int_t* getBEMC_FEE_HT_ADC() {return L0_HT_ADC;}
  Int_t* getBEMC_FEE_TP_ADC() {return L0_TP_ADC;}
  int getHT6bitAdc(int towerId) const { return HT6bit_adc_holder[towerId-1]; }
  int getTP6bitAdc(int triggerPatch) const { return TP6bit_adc_holder[triggerPatch]; }

  //out of DSM layer0 to DSM layer1 
  Int_t* getBEMC_L0_OUT() {return L0_16bit_Out;}
  Int_t* getBEMC_L0_SUM() {return DSM0_TP_SUM;}
  Int_t* getBEMC_L0_SUM_J1() {return DSM0_TP_SUM_J1;}
  Int_t* getBEMC_L0_SUM_J3() {return DSM0_TP_SUM_J3;}
  Int_t* getBEMC_L0_HT_Bit() {return DSM0_HT_Bit;}
  Int_t* getBEMC_L0_HT_Bit_J1() {return DSM0_HT_Bit_J3;}
  Int_t* getBEMC_L0_HT_Bit_J3() {return DSM0_HT_Bit_J1;}
  Int_t* getBEMC_L0_TP_Bit() {return DSM0_TP_Bit;}
  Int_t* getBEMC_L0_TP_Bit_J1() {return DSM0_TP_Bit_J3;}
  Int_t* getBEMC_L0_TP_Bit_J3() {return DSM0_TP_Bit_J1;}
  Int_t* getBEMC_L0_HTTP_Bit() {return DSM0_HTTP_Bit;}
  Int_t* getBEMC_L0_HTTP_Bit_J1() {return DSM0_HTTP_Bit_J3;}
  Int_t* getBEMC_L0_HTTP_Bit_J3() {return DSM0_HTTP_Bit_J1;}
 

  //out of DSM layer 1 to DSM layer 2
  Int_t* getBEMC_L1_JP() {return DSM1_JP_Bit;}
  Int_t* getBEMC_L1_HT() {return DSM1_HT_Bit;}
  Int_t* getBEMC_L1_TP() {return DSM1_TP_Bit;}
  Int_t* getBEMC_L1_HTTP() {return DSM1_HTTP_Bit;}
  Int_t* getBEMC_L1_ETOT_ADC() {return DSM1_ETOT_ADC;}

  //access to towers, patches
  const vector< pair<int, int> > getTowersAboveThreshold(int trigId) const;
  const vector< pair<int, int> > getTriggerPatchesAboveThreshold(int trigId) const;
  const vector< pair<int, int> > getJetPatchesAboveThreshold(int trigId) const;
  const vector< pair<int,int> >& getJpsiCandidates() const { return mJpsiCandidates; }
  int numberOfJpsiCandidates() const { return mJpsiCandidates.size(); }
  const pair<int,int>& jpsiCandidate(int i) const { return mJpsiCandidates[i]; }
  int jpsiCandidateFirstTowerId(int i) const { return mJpsiCandidates[i].first; }
  int jpsiCandidateSecondTowerId(int i) const { return mJpsiCandidates[i].second; }

  //access to HT,TP,JP thresholds
  int getTowerThreshold(int trigId, int dsmid) const;
  int getTriggerPatchThreshold(int trigId, int dsmid) const;
  int getJetPatchThreshold(int trigId, int dsmid) const;

  // Access to 2009 BEMC layer 0 and 1 results
  DSMLayer_B001_2009* get2009_DSMLayer0_Result() { return mB001; }
  DSMLayer_B101_2009* get2009_DSMLayer1_Result() { return mB101; }

  // Fill StEmcTriggerDetector in MuDst for MC
  void fillStEmcTriggerDetector();
  // test mode zchang
  bool mTestMode;
  ClassDef(StBemcTriggerSimu, 1);
};
#endif
