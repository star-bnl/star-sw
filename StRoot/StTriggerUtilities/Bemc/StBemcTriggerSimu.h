#ifndef STAR_StBemcTriggerSimu
#define STAR_StBemcTriggerSimu

#include "TObject.h"
#include "StMessMgr.h"
#include "TString.h"
#include <TObject.h> 
#include <vector>

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096
#define kNCrates 30
#define kNChannels 160

class StMuDstMaker;
class StMuDst;
class StEvent;
class StEmcDecoder;
class StEmcGeom;
class StBemcTables;
class St_db_Maker;
class StBemcTriggerDbThresholds;
class TDataSet;
class StBemcTriggerSimu {

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
  Int_t mMCflag;                           //0= false and 1 =true
  Float_t adc;                             //12 bit BEMC adc from StEvent
  Float_t ped,rms;                           //12 bit BEMC ped and rms from offline database 
  unsigned long pedTargetValue;            //value FEE shifts pedestal to (10 or 12 bit?)
  unsigned long bitConvValue[kNCrates][10];//gives window used to determine HT6Bit
  Int_t HT_FEE_Offset;                     //same as bitConvValue but set by support class
  Int_t HT6bit,TP6bit;                     //6 bit HT and TP word which is passed out of FEE and into DSM L0
  Int_t DSM_HTStatus[kNPatches];           //DSM_HTStatus only set online
  Int_t DSM_TPStatus[kNPatches];           //DSM_TPStatus only set online
  Int_t TowerStatus[kNTowers];             //tower status as determined online or offline
  Int_t adc12[kNTowers],adc10[kNTowers],adc08[kNTowers];
  Int_t ped12[kNTowers],ped10[kNTowers],ped08[kNTowers];


  void setTowerStatus();
  void setDSM_TPStatus();
  void setDSM_HTStatus();
  void setLUT();
  void getPed();
  
 public:

  StBemcTriggerSimu();
  virtual     ~StBemcTriggerSimu();

  void Init();
  void InitRun();
  void Clear();
  void Make();

  void setMC(int x) {mMCflag=x;}
  void setHList(TObjArray * x){mHList=x;}
  void addTriggerList( void * );

  void setTableMaker(StBemcTables *tab) { mTables=tab; }
  void setBemcDbMaker(St_db_Maker *dbMk) { starDb=dbMk; }
  void setBemcConfig(TString *CONFIG) { config=CONFIG; }
  void setEvent(StEvent* e) { mEvent = e; }
  ClassDef(StBemcTriggerSimu, 1);
 };


#endif
