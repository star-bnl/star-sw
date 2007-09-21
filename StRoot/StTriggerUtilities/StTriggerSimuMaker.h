//////////////////////////////////////////////////////////////////////////
//
//
// StTriggerSimuMaker R.Fatemi, Adam Kocoloski , Jan Balewski  (Fall, 2007)
//
// Goal: generate trigger response based on ADC
// implemented BEMC,EEMC,....
// >StTriggerSimu/*SUB*/St*SUB*TriggerSimu.h
// >where *SUB* are the subsystems: Eemc, Bemc, Bbc, L2,.... 
//
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StTriggerSimuMaker
#define STAR_StTriggerSimuMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#define kNPatches 300
#define kNJet 12
#define kNTowers 4800
#define k12bits 4096
#define kNCrates 30
#define kNChannels 160

class StEvent;
class StBemcTables;
class StBbcTriggerSimu;
class StEemcTriggerSimu;
class StBemcTriggerSimu;
class St_db_Maker;

class StTriggerSimuMaker : public StMaker {

 private:
  
  TString *config;
  int mYear,mMCflag; // set yo 0 for real data
  StEvent *event;
  St_db_Maker *mDbMk;
  StBemcTables *mTables;
  void addTriggerList();
  void fillStEvent(StEvent*);
  void setTableMaker(StBemcTables *bemcTab); 
  
 public:
  
  StTriggerSimuMaker(const char *name="StarTrigSimu");
  virtual           ~StTriggerSimuMaker();
  
  void    useEemc();
  void    useBbc();
  void    useBemc();
  void    setMC(int x) {mMCflag=x;}
  
  virtual Int_t     Init();
  virtual Int_t     Make();
  virtual Int_t     Finish();
  virtual void      Clear(const Option_t* = "");
  virtual Int_t     InitRun  (int runumber);
  
  TObjArray  *mHList; // output histo access point
  void setHList(TObjArray * x){mHList=x;}
  bool isTrigger(int trigId);   
  vector <int> mTriggerList;
  void setDbMaker(St_db_Maker *dbMk) { mDbMk=dbMk;}
  void setConfig(TString *CONFIG) {config=CONFIG;}
  
  //hang all activated trigger detectors below
  StEemcTriggerSimu *eemc;
  StBbcTriggerSimu *bbc;
  StBemcTriggerSimu *bemc;
  
  Int_t BEMC_L0_HT_ADC[kNPatches],*BEMC_L0_TP_ADC[kNPatches];
  
  ClassDef(StTriggerSimuMaker,0)
    
};
   
#endif



// $Id: StTriggerSimuMaker.h,v 1.7 2007/09/21 18:45:51 rfatemi Exp $
//
// $Log: StTriggerSimuMaker.h,v $
// Revision 1.7  2007/09/21 18:45:51  rfatemi
// End of week update
//
// Revision 1.6  2007/08/12 01:03:22  rfatemi
// Added flag for offline/online/expert settings
//
// Revision 1.5  2007/08/07 15:48:38  rfatemi
// Added BEMC access methods
//
// Revision 1.4  2007/07/23 03:03:39  balewski
// fix
//
// Revision 1.3  2007/07/22 23:09:51  rfatemi
// added access to Bbc
//
// Revision 1.2  2007/07/21 23:35:24  balewski
// works for M-C
//
// Revision 1.1  2007/07/20 21:01:41  balewski
// start
//
