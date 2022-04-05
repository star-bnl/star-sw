#ifndef STAR_StMyEventMaker
#define STAR_StMyEventMaker

#include <StMaker.h>

class TH1F;
class TH2F;
class TTree;

class St_db_Maker;
class StEmcADCtoEMaker;
class StBemcTables;
class StEmcPoint;
class StMuDst;

class MyEvent;
class MyMcTrack;
class MyPoint;

class StMyEventMaker : public StMaker 
{
 private:
 protected:

  const char *mFileName;//!
  Bool_t mEmbed;//!
  Bool_t mMc;//!
  Bool_t mReal;//!

  Bool_t mDAU;//!
  Bool_t mPP04;//!
  Bool_t mPP05;//!
  Bool_t mAUAU200;//!

  Int_t mRunId;//!
  Int_t mRunPrev;//!
  Int_t mEventId;//!

  Int_t mN;//!
  Int_t mTrigger;//!
  Int_t mTrig[4];//!

  Int_t mPs_mb;//!
  Int_t mPs_mb2;//!
  Int_t mPs_ht1;//!
  Int_t mPs_ht2;//!
  Int_t mDate;//!
  Int_t mTime;//!

  Bool_t mDebug;//!

  St_db_Maker *mDbMaker;//!
  StEmcADCtoEMaker *mAdcMaker;//!
  StBemcTables *mBemcTables;//!

  TH1F *h_EvSum;//!
  
  TTree *mEventTree;//!
  MyEvent *mEvent;//!
  

 public: 
  
  StMyEventMaker(const char *name="niets",const char *filename="bla.root",
		const char *flag="real",const char *coll="CuCu",Bool_t debug=kFALSE);
  virtual ~StMyEventMaker();	
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  void saveHistograms();
  void setDbMaker(St_db_Maker* maker) {mDbMaker=maker;}
  void setAdcMaker(StEmcADCtoEMaker *maker) {mAdcMaker=maker;}
  Bool_t calcDistanceTrackToPoint(StEmcPoint*,StMuDst*,Float_t&);
 
  ClassDef(StMyEventMaker, 1)
};

#endif
