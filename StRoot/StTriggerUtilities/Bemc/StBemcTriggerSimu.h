#ifndef STAR_StBemcTriggerSimu
#define STAR_StBemcTriggerSimu

#include "TObject.h"
#include "StMessMgr.h"
#include "TString.h"
#include <TObject.h> 
#include <vector>

#include "StTriggerSimu.h"

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096
#define kNCrates 30
#define kNChannels 160
#define kNSeq 10
#define kL0DsmModule 30
#define kL0DsmInputs 10

class StMuDstMaker;
class StMuDst;
class StEvent;
class StEmcDecoder;
class StEmcGeom;
class StBemcTables;
class St_db_Maker;
class StBemcTriggerDbThresholds;
class TDataSet;

class StBemcTriggerSimu : public StTriggerSimu {

 private:
  
  TObjArray *mHList;
  StMuDstMaker *mMuDstMaker;
  StMuDst *muDst;
  StEvent *mEvent;
  StEmcDecoder *mDecoder;
  St_db_Maker *starDb;
  StEmcGeom *mGeo;
  StBemcTables *mTables;
  StBemcTriggerDbThresholds *mDbThres;
  TDataSet *dbOnline;

  TString *config;                         //"online" or "offline" or "expert"   
  Int_t did;                               //BEMC tower id  (1-4800)
  Int_t tpid;                              //BEMC trigger Patch id (0-300)
  Int_t cr;                                //BEMC crate id (1-30)
  Int_t ch;                                //BEMC crate ch (0-159)
  Int_t seq;                               //BEMC start point for TP in crate (0-10)
  Int_t HT_FEE_Offset;                     //same as bitConvValue but set by support class
  Int_t DSM_HTStatus[kNPatches];           //DSM_HTStatus only set online
  Int_t DSM_TPStatus[kNPatches];           //DSM_TPStatus only set online
  Int_t TowerStatus[kNTowers];             //tower status as determined online or offline
  Int_t adc12[kNTowers];                   //12 bit adc from StEvent -> NOT pedestal adjusted!
  Int_t adc10[kNTowers],adc08[kNTowers];   //ped adjusted 10 and 8 bit adc
  Int_t ped12[kNTowers];                   //12 and 10 bit pedestal
  Int_t HTadc06[kNTowers];                 //6bit HT ADC for each tower
  unsigned long pedTargetValue;            //value FEE shifts pedestal to (12 bit)
  unsigned long bitConvValue[kNTowers];//gives window used to determine HT6Bit from adc10
  unsigned long LUTbit0[kNCrates][kNSeq],LUTbit1[kNCrates][kNSeq],LUTbit2[kNCrates][kNSeq];
  unsigned long LUTbit3[kNCrates][kNSeq],LUTbit4[kNCrates][kNSeq],LUTbit5[kNCrates][kNSeq];
  unsigned long LUTtag[kNCrates][kNSeq];
  Float_t ped12Diff,ped10Diff;

  Int_t L0_HT_ADC[kNPatches], L0_TP_ADC[kNPatches], L0_TP_PED[kNPatches];

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
  
  short isTrigger(int trigId);

  //void setMC(int x) {mMCflag=x;}
  void setHList(TObjArray * x){mHList=x;}
  void addTriggerList( void * );

  void setTableMaker(StBemcTables *tab) { mTables=tab; }
  void setBemcDbMaker(St_db_Maker *dbMk) { starDb=dbMk; }
  void setBemcConfig(TString *CONFIG) { config=CONFIG; }
  void setEvent(StEvent* e) { mEvent = e; }
  
  Int_t* getBEMC_L0_HT_ADC() {return L0_HT_ADC;}
  Int_t* getBEMC_L0_TP_ADC() {return L0_TP_ADC;}

  ClassDef(StBemcTriggerSimu, 1);
 };


#endif
