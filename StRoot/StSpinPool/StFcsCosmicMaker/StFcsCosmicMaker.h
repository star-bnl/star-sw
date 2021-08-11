/*
 *
 * \class StFcsCosmicMaker
 *
 */

#ifndef STAR_StFcsCosmicMaker_HH
#define STAR_StFcsCosmicMaker_HH

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEvent/StEnumerations.h"
#include "StMaker.h"

class StFcsDb;
class StFcsCollection;
class TH1F;
class TH2F;

class StFcsCosmicMaker : public StMaker {
 public: 
   StFcsCosmicMaker(const Char_t* name = "FcsCosmic");
   virtual ~StFcsCosmicMaker();
   virtual Int_t Init();
   virtual Int_t Make();
   virtual Int_t Finish();

   void setRun(int v) {mRun=v;}
   void setNTowerThre(int ecal, int hcal) {mNTowerThre[0]=ecal; mNTowerThre[1]=hcal;}
   void setSigmaMaxThre(float ecal, float hcal) {mSigmaMaxThre[0]=ecal; mSigmaMaxThre[1]=hcal;}
   void setSigmaMinThre(float ecal, float hcal) {mSigmaMinThre[0]=ecal; mSigmaMinThre[1]=hcal;}
   void setThetaThre(float ecal, float hcal) {mThetaThre[0]=ecal; mThetaThre[1]=hcal;}
   void setFilename(char* v) {mSetFile=v;}

 protected:

 private:
   StFcsDb *mFcsDb=0;
   StFcsCollection *mFcsCollection=0;
   int mRun;
   TFile *mFile=0;
   char* mSetFile=0;
   char mFilename[100];

   float mNTowerThre[2];
   float mSigmaMaxThre[2];
   float mSigmaMinThre[2];
   float mThetaThre[2];

   TH1F* mAdc[2];
   TH1F* mNTower[2];
   TH1F* mSigmaMax[2];
   TH1F* mSigmaMin[2];
   TH2F* mSigma[2];
   TH2F* mSigmaNtow[2];
   
   virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(StFcsCosmicMaker,1);
};

#endif

/*
 * $Id: StFcsCosmicMaker.h,v 1.6 2021/03/30 13:29:27 akio Exp $
 * $Log: StFcsCosmicMaker.h,v $
 *
 */
