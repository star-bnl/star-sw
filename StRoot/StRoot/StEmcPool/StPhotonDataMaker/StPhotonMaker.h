#ifndef STAR_StPhotonMaker
#define STAR_StPhotonMaker

#include <StMaker.h>

class TH1F;
class TH2F;
class TTree;

class St_db_Maker;
class StEmcPoint;
class StEmcADCtoEMaker;
class StBemcTables;

class MyEvent;
class MyMcTrack;
class MyPoint;

class StPhotonMaker : public StMaker 
{
 private:
 protected:

  const char *mFileName;//!
  Bool_t mEmbed;//!
  Bool_t mMc;//!
  Bool_t mPythia;
  Bool_t mHijing;
  Bool_t mReal;//!

  Bool_t mDAU;//!
  Bool_t mPP04;//!
  Bool_t mPP05;//!

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
  StBemcTables *mBemcTables;//!
  StEmcADCtoEMaker *mAdcMaker;//!

  TH1F *h_EvSum;//!
  TH1F *h_bsmdeAdc;//!  
  TH1F *h_bsmdpAdc;//!
  TH1F *h_bsmdeEn;//!
  TH1F *h_btowAdc;
  TH1F *h_btowEn;
  TH2F *h_btowEnVsAdc;

  TTree *mEventTree;//!
  MyEvent *mEvent;//!
  

 public: 
  
  StPhotonMaker(const char *name="niets",const char *filename="bla.root",
		const char *flag="real",const char *coll="CuCu",Bool_t debug=kFALSE);
  virtual ~StPhotonMaker();	
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  void saveHistograms();
  void setDbMaker(St_db_Maker*);
  void setAdcMaker(StEmcADCtoEMaker*);
  Bool_t calcDistanceTrackToPoint(StEmcPoint*,Float_t&);
 
  ClassDef(StPhotonMaker, 1)
};

#endif
