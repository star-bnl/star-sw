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
  Int_t did;                               //always used as BEMC tower id
  //Int_t mMCflag;                           //0= false and 1 =true
  unsigned long pedTargetValue;            //value FEE shifts pedestal to (10 or 12 bit?)
  unsigned long bitConvValue[kNCrates][kNSeq];//gives window used to determine HT6Bit
  Int_t HT_FEE_Offset;                     //same as bitConvValue but set by support class
  Int_t HT6bit,TP6bit;                     //6 bit HT and TP word which is passed out of FEE and into DSM L0
  Int_t DSM_HTStatus[kNPatches];           //DSM_HTStatus only set online
  Int_t DSM_TPStatus[kNPatches];           //DSM_TPStatus only set online
  Int_t TowerStatus[kNTowers];             //tower status as determined online or offline
  Int_t adc12[kNTowers];                   //12 bit adc from StEvent -> NOT pedestal adjusted!
  Int_t adc10[kNTowers],adc08[kNTowers];   //ped adjusted 10 and 8 bit adc
  Int_t ped12[kNTowers],ped10[kNTowers];   //12 and 10 bit pedestal
  Int_t HTadc06[kNTowers];
  Float_t ped12Diff,ped10Diff;
  Int_t L0_HT_ADC[kNPatches], L0_TP_ADC[kNPatches];

  void getTowerStatus();
  void getDSM_TPStatus();
  void getDSM_HTStatus();
  void getLUT();
  void getPed();
  
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
