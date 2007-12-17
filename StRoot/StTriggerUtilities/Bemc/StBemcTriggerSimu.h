#ifndef STAR_StBemcTriggerSimu
#define STAR_StBemcTriggerSimu

#include "TObject.h"
#include "TString.h"
#include <vector>
#include <set>
#include "TDatime.h"

#include "StTriggerUtilities/StVirtualTriggerSimu.h"

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

class StEvent;
class StEmcDecoder;
class StEmcGeom;
class StBemcTables;
class St_db_Maker;
class StBemcTriggerDbThresholds;
class StTriggerSimuMaker;
class TDataSet;
class St_db_Maker;

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
  StBemcTables *mTables;
  St_db_Maker *starDb;
  StTriggerSimuMaker *mHeadMaker;
  int mConfig; // see enum  
  
  // DB information
  Int_t HT_FEE_Offset;                     //same as bitConvValue but set by support class
  Int_t DSM_HTStatus[kNPatches];           //DSM_HTStatus only set online
  Int_t DSM_TPStatus[kNPatches];           //DSM_TPStatus only set online
  Int_t TowerStatus[kNTowers];             //tower status as determined online or offline
  unsigned long bitConvValue[kNTowers];    //gives window used to determine HT6Bit from adc10
  Int_t year,timestamp;

  Int_t adc12[kNTowers];                   //12 bit adc from StEvent -> NOT pedestal adjusted!
  Int_t adc10[kNTowers],adc08[kNTowers];   //ped adjusted 10 and 8 bit adc
  Float_t ped12[kNTowers];                 //12 and 10 bit pedestal
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
  //Intermediate bits for each trigger patch input to DSMLayer0
  Int_t DSM0_HT_tp_Bit[kL0DsmInputs], DSM0_TP_tp_Bit[kL0DsmInputs], DSM0_HTTP_tp_Bit[kL0DsmInputs];
  Int_t DSM0_HT_tp_Bit_J3[kL0DsmInputs], DSM0_TP_tp_Bit_J3[kL0DsmInputs],DSM0_HTTP_tp_Bit_J3[kL0DsmInputs];
  Int_t DSM0_HT_tp_Bit_J1[kL0DsmInputs], DSM0_TP_tp_Bit_J1[kL0DsmInputs],DSM0_HTTP_tp_Bit_J1[kL0DsmInputs];
  Int_t L0_16bit_Out[kNLayer0out];
  //DSM1 Bits for JP, HT, TP and HTTP
  Int_t DSM1_JP_Bit[kL1DsmModule], DSM1_HT_Bit[kL1DsmModule], DSM1_TP_Bit[kL1DsmModule], DSM1_HTTP_Bit[kL1DsmModule];
  Int_t DSM1_ETOT_ADC[kL1DsmModule];
  //Intermediate bits for each jet patch constructed in DSMLayer1
  Int_t DSM1_JP_jp_Bit[kNJet];
  
  void getTowerStatus();
  void getDSM_TPStatus();
  void getDSM_HTStatus();
  void getLUT();
  void getPed();
  void FEEout();
  void get2006_DSMLayer0();
  void get2006_DSMLayer1();
  
public:
  StBemcTriggerSimu();
  virtual     ~StBemcTriggerSimu();

  void Init();
  void InitRun(int runnumber);                                              
  void Clear();
  void Make();
  
  StTriggerSimuDecision triggerDecision(int trigId);

  void setHeadMaker(StTriggerSimuMaker *maker) { mHeadMaker = maker; }
  
  void setHList(TObjArray * x){mHList=x;}
  
  public:
  enum {kOnline=1, kOffline, kExpert};
  void setConfig(int x) {mConfig=x;}
  
  /// default tables come from emcSim or adc2e, but you can supply your own if you prefer
  void setTables(StBemcTables *tab) { mTables = tab; }
  
  //out of FEE into DSM layer0
  Int_t* getBEMC_FEE_HT_ADC() {return L0_HT_ADC;}
  Int_t* getBEMC_FEE_TP_ADC() {return L0_TP_ADC;}

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

  ClassDef(StBemcTriggerSimu, 1);
};
#endif
