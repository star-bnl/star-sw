/*
 *
 * \class StFcsQaMaker
 *
 */

#ifndef STAR_StFcsQaMaker_HH
#define STAR_StFcsQaMaker_HH

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEvent/StEnumerations.h"
#include "StMaker.h"

class StFcsDbMaker;
class StFcsCollection;
class TH1F;
class TH2F;

class StFcsQaMaker : public StMaker {
 public: 
   StFcsQaMaker( const Char_t* name = "FcsQA");
   virtual ~StFcsQaMaker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual Int_t Finish();
   void setRun(int v) {mRun=v;}
   void setMaxTimeBins(int v) {mNTimeBins=v;}
   void setSumTimeBins(int min, int max) {mMinTB=min; mMaxTB=max;}
   void setMaxAdc(int v) {mMaxAdc=v;}
   void setMaxAdcSum(int v) {mMaxAdcSum=v;}
   void setDump(int v) {mDump=v;}
   void setPedSub(int v) {mPedSub=v;}

 protected:

 private:
   StFcsDbMaker *mFcsDbMkr=0;
   StFcsCollection *mFcsCollection=0;
   int mRun=0; 
   TFile *mFile=0;
   char mFilename[100];
   int mNTimeBins=16;
   int mMinTB=210;   
   int mMaxTB=225;
   int mMaxAdc=4096;
   int mMaxAdcSum=7000;
   int mDump=0;
   int mPedSub=0;

   static const int mNTRG=64;

   TH1F* mDataSize;
   TH2F* mAdcTb2[kFcsNDet];
   TH2F* mAdcTb[kFcsNDet][kFcsEcalMaxId];
   TH2F* mAdcId[kFcsNDet];
   TH2F* mAdcSumId[kFcsNDet];
   TH1F* mAdcSum[kFcsNDet][kFcsEcalMaxId];
   TH1F* mNHit[kFcsNDet];
   TH2F* mHitmap[kFcsNDet];

   TH1F* mNClu[kFcsNDet];
   TH1F* mNTowClu[kFcsNDet];
   TH1F* mNNeiClu[kFcsNDet];
   TH2F* mNTowEClu[kFcsNDet][kFcsEcalMaxId];
   TH2F* mNTowECluIso[kFcsNDet][kFcsEcalMaxId];
   TH2F* mNTowECluIsoH[kFcsNDet][kFcsEcalMaxId];

   ClassDef(StFcsQaMaker,1);
};

#endif

/*
 * $Id: StFcsQaMaker.h,v 1.2 2019/06/21 17:44:47 akio Exp $
 * $Log: StFcsQaMaker.h,v $
 * Revision 1.2  2019/06/21 17:44:47  akio
 * added cluster plots
 *
 * Revision 1.1  2019/06/07 19:06:44  akio
 * *** empty log message ***
 *
 */
