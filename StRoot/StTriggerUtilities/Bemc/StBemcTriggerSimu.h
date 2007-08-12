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

  Int_t mMCflag; // 0 for real data  
  Int_t did;
  Float_t adc,ped,rms;
  Int_t HT6bit,TP6bit,HT_FEE_Offset;
  Int_t adc12[kNTowers],adc10[kNTowers],adc08[kNTowers];
  Int_t ped12[kNTowers],ped10[kNTowers],ped08[kNTowers];
  Int_t PatchStatus[kNPatches];
  TString *config;

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

  Int_t simulateHT_FEEaction(int,int,int);
  Int_t simulateTP_FEEaction(int,int);

  void setTableMaker(StBemcTables *tab) { mTables=tab; }
  void setBemcDbMaker(St_db_Maker *dbMk) { starDb=dbMk; }
  void setBemcConfig(TString *CONFIG) { config=CONFIG; }
  void setEvent(StEvent* e) { mEvent = e; }
  ClassDef(StBemcTriggerSimu, 1);
 };


#endif
