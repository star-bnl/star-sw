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
   void setEvent(int v) {mEvent=v;}
   void setMaxTimeBins(int v) {mNTimeBins=v;}
   void setSumTimeBins(int min, int max) {mMinTB=min; mMaxTB=max;}
   void setPedTimeBins(int min, int max) {mMinTBp=min; mMaxTBp=max;}
   void setMaxAdc(int v) {mMaxAdc=v;}
   void setMaxAdcSum(int v) {mMaxAdcSum=v;}
   void setDump(int v) {mDump=v;}
   void setPedSub(int v) {mPedSub=v;}
   void setFilename(char* v) {mSetFile=v;}

 protected:

 private:
   StFcsDbMaker *mFcsDbMkr=0;
   StFcsCollection *mFcsCollection=0;
   int mRun=0; 
   int mEvent=0; 
   TFile *mFile=0;
   char* mSetFile=0;
   char mFilename[100];
   int mNTimeBins=16;
   int mMinTB=103;   
   int mMaxTB=109;
   int mMinTBp=0;   
   int mMaxTBp=10;
   int mMaxAdc=4096;
   int mMaxAdcSum=12000;
   int mDump=0;
   int mPedSub=0;

   static const int mNTRG=64;

   TH1F* mDataSize;
   TH1F* mEsum[3];

   TH2F* mAdcTb2[kFcsNDet];
   TH2F* mAdcTb[kFcsNDet][kFcsEcalMaxId];
   TH2F* mAdcId[kFcsNDet];
   TH2F* mAdcIdp[kFcsNDet];
   TH2F* mAdcSumId[kFcsNDet];
   TH2F* mTimeId[kFcsNDet];
   TH2F* mFitIntg[kFcsNDet];
   TH2F* mFitSigm[kFcsNDet];
   TH2F* mFitTime[kFcsNDet];
   TH2F* mFitChi2[kFcsNDet];
   TH1F* mAdcSum[kFcsNDet][kFcsEcalMaxId];
   TH1F* mNHit[kFcsNDet];
   TH2F* mHitMap[3];
   TH2F* mTimeEvt;
   float mTimeE[kFcsNDet][kFcsEcalMaxId][100];

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
 * $Id: StFcsQaMaker.h,v 1.5 2021/02/13 21:41:09 akio Exp $
 * $Log: StFcsQaMaker.h,v $
 * Revision 1.5  2021/02/13 21:41:09  akio
 * sector avg peak time
 *
 * Revision 1.4  2021/01/11 14:40:31  akio
 * Many changes for FCS 2021 comissioning & LED monitor.
 * Includingplots for backview, fit plots and more.
 *
 * Revision 1.3  2020/12/17 21:09:54  akio
 * add esum
 *
 * Revision 1.2  2019/06/21 17:44:47  akio
 * added cluster plots
 *
 * Revision 1.1  2019/06/07 19:06:44  akio
 * *** empty log message ***
 *
 */
