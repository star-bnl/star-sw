#ifndef STAR_StFcsMIPMaker
#define STAR_StFcsMIPMaker                              

//#ifndef StMaker_H
//#include "StMaker.h"

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEvent/StEnumerations.h"
#include "StMaker.h"
#include <map>

class StFcsDbMaker;
class StFcsCollection;
class TH1F;
class TH2F;

class StFcsMIPMaker : public StMaker {
 public: 
  StFcsMIPMaker(const char *name="FcsMIP");
  virtual ~StFcsMIPMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish();
  void setPedSub(int v) {mPedSub=v;}
  void setRun(int v) {mRun=v;}
  void setSubRun(int v) {mSub=v;}

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
 
 protected:
  
 private:
  StFcsDbMaker *mFcsDbMkr=0; 
  StFcsCollection *mFcsCollection=0; 
  int mRun=0;
  int mSub=0; //need this to get file name from .cxx
  TFile *mFile=0;
  char mFilename[100];
  int mNTimeBins=160;
  int mMinTB=210;   
  int mMaxTB=225;
  int mMaxAdc=4096;
  int mMaxAdcSum=7000;
  int mPedSub=0;
 
  //Ecal & Hcal correlation map
  std::map<int,int> MapofCals;
  
  int thresh=100;     //set adc threshold
  int Hcal_array[16];
  int Hcaltowid;
  int Ecaltowid;

  //declare variables for cluster correlation
  float deltax;
  float deltaxmin;
  float deltay;
  float deltaymin;
  float deltar;
  float deltarmin;
  float matchHcalClusE;
  
  TH2F *mAdcTb2[kFcsNDet], *mAdcTbthresh2[kFcsNDet];
  TH2F *mAdcTb[kFcsNDet][kFcsEcalMaxId], *mAdcTbthresh[kFcsNDet][kFcsEcalMaxId];
  TH1F *maxAdc[kFcsNDet][kFcsEcalMaxId], *maxAdcthresh[kFcsNDet][kFcsEcalMaxId];
  TH2F* mAdcId[kFcsNDet];
  TH2F* mAdcSumId[kFcsNDet];
  TH1F* mAdcSum[kFcsNDet][kFcsEcalMaxId];
  TH1F* mNHit[kFcsNDet];
  TH2F* mHitmap[kFcsNDet];
  
  //Variables for cluster making
  TH1F* mNClu[kFcsNDet];
  TH1F* mNTowClu[kFcsNDet];
  TH1F* mNNeiClu[kFcsNDet];
  TH2F* mNTowEClu[kFcsNDet][kFcsEcalMaxId];
  TH2F* mNTowECluIso[kFcsNDet][kFcsEcalMaxId];
  TH2F* mNTowECluIsoH[kFcsNDet][kFcsEcalMaxId];
  //location of clusters
  TH2F* mNClusX;
  TH2F* mNClusY;
  TH2F* mEcalClust;
  TH2F* mHcalClust;

  //check deltarmin condition
  TH2F* deltar_Ecaltow;
  TH2F* HcalE_Ecaltow;
  
  TH1F* towenergy[kFcsNDet][kFcsEcalMaxId];
  TH1F* towadcsum[kFcsNDet][kFcsEcalMaxId];
  TH1F* nocut_towadcsum[kFcsNDet][kFcsEcalMaxId];
  TH1F* hitsperevent;
  TH1F* rcluster; // <-- spatial sep of Hcal clust and nearest Ecal clust
  TH1F* rmincluster;
  TH1F* hmatchHcalClusE;
  TH1F* minbias_events;

  ClassDef(StFcsMIPMaker,1)   //StAF chain virtual base class for Makers
};

#endif

 
