/*
 *
 * \class StFpsQaMaker
 *
 */

#ifndef STAR_StFpsQaMaker_HH
#define STAR_StFpsQaMaker_HH

#include "StRoot/St_base/Stypes.h"
#include "StMaker.h"

class StFmsDbMaker;
class StFmsCollection;
class TH1F;
class TH2F;

class StFpsQaMaker : public StMaker {
 public: 
   StFpsQaMaker( const Char_t* name = "FpsQA");
   virtual ~StFpsQaMaker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual Int_t Finish();
   void setRun(int v) {mRun=v;}
   void setPed(int v) {mPed=v;}

 protected:

 private:
   StFmsDbMaker *mFmsDbMkr;
   StFmsCollection *mFmsCollectionPtr;
   int mRun; 
   int mPed;
   TFile *mFile;
   char mFilename[100];

   static const int mNPREPOST=3;
   static const int mNQ=4;
   static const int mNL=3;
   static const int mNS=21;
   static const int mNID=4*3*21;
   static const int mNTRG=64;

   TH1F* mDataSize[2];
   TH1F* mXing[mNPREPOST*2+1];
   TH2F* mAdc2[2];   
   TH1F* mAdc[mNID][2];
   TH1F* mNHit[mNQ][mNL];
   TH1F* mHit[mNQ][mNL];
   TH1F* mNHitTrg[mNTRG+1];
   TH2F* mNHitTrg2;

   ClassDef(StFpsQaMaker,1);
};

#endif

/*
 * $Id: StFpsQaMaker.h,v 1.2 2015/05/30 16:08:00 akio Exp $
 * $Log: StFpsQaMaker.h,v $
 * Revision 1.2  2015/05/30 16:08:00  akio
 * *** empty log message ***
 *
 * Revision 1.1  2015/02/25 20:03:26  akio
 * new fps qa maker
 *
 */
