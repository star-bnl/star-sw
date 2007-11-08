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
  
  // simple iterators -- C++ style manual would say declare in local scope
  //Int_t did;                               //BEMC tower id  (1-4800)
  //Int_t tpid;                              //BEMC trigger Patch id (0-300)
  //Int_t cr;                                //BEMC crate id (1-30)
  //Int_t ch;                                //BEMC crate ch (0-159)
  //Int_t seq;                               //BEMC start point for TP in crate (0-10)
  
  
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
  Int_t *LUTbit[kNCrates][kNSeq], LUTtag[kNCrates][kNSeq];
  char buffer[10];

  //HT/TP 6bit ADC out of FEE and into DSM Layer0
  Int_t L0_HT_ADC[kNPatches], L0_TP_ADC[kNPatches], L0_TP_PED[kNPatches];

  //MAX TP ADC, MAX HT ADC and SUM TPC ADC for each DSM Module
  Int_t DSM0_TP_ADC[kL0DsmModule],DSM0_HT_ADC[kL0DsmModule],DSM0_TP_SUM[kL0DsmModule];

  //Thresholds set by database in DSM Layer0
  Int_t HT_DSM0_threshold[kL0DsmModule], TP_DSM0_threshold[kL0DsmModule],HTTP_DSM0_threshold[kL0DsmModule];

  void getTowerStatus();
  void getDSM_TPStatus();
  void getDSM_HTStatus();
  void getLUT();
  void getPed();
  void FEEout();
  void DSMLayer0();
  void DSMLayer1();
  void DSMLayer2();
  
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
  
  Int_t* getBEMC_L0_HT_ADC() {return L0_HT_ADC;}
  Int_t* getBEMC_L0_TP_ADC() {return L0_TP_ADC;}

  ClassDef(StBemcTriggerSimu, 1);
};
#endif
