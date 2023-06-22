#ifndef STAR_StFcsEcalHcalMipMaker
#define STAR_StFcsEcalHcalMipMaker                              

//#ifndef StMaker_H
//#include "StMaker.h"

#include "StRoot/St_base/Stypes.h"
#include "StRoot/StEvent/StEnumerations.h"
#include "StMaker.h"
#include <map>

class StFcsDb;
class StFcsCollection;
class TH1F;
class TH2F;

class StFcsEcalHcalMipMaker : public StMaker {
public: 
  StFcsEcalHcalMipMaker(const char *name="FcsEcalHcalMip");
  virtual ~StFcsEcalHcalMipMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t Finish();

  void setRun(int v){mRun=v;}

protected:
  
private:
  StFcsDb *mFcsDb=0; 
  StFcsCollection *mFcsCollection=0; 
  int mRun=0;
  TFile *mFile=0;
  char mFilename[100];

  TH2F *mAdc[4];
  TH2F *mAdcSingleTower[4];
  TH2F *mAdcEcalMatch[4];
  
  TH1F* mNClu[2];
  TH1F* mNTowClu[2];
  TH1F* mNNeiClu[2];

  //location of clusters and distance
  TH2F* mX;
  TH2F* mY;
  TH2F* mDXX;
  TH2F* mDXY;
  TH2F* mDYX;
  TH2F* mDYY;
  TH2F* mDR;

  ClassDef(StFcsEcalHcalMipMaker,1)   //StAF chain virtual base class for Makers
};

#endif

 
