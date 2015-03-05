#ifndef ST_FMS_TRGQA_MAKER_H
#define ST_FMS_TRGQA_MAKER_H

#include "StMaker.h"
#include "TH1F.h"
#include "TH2F.h"
class StFmsTriggerMaker;

class StFmsTrgQaMaker : public StMaker {
public:
  StFmsTrgQaMaker(const char* name = "TrgQa");

  void Clear(Option_t* option = "");
  int Init();  
  int Make();
  int Finish();
  void setRun(int v) {mRun=v;}
  void setPrint(int v) {mPrint=v;}

private:
  int mRun;
  int mPrint;
  StFmsTriggerMaker* mSIM;
  
  void fillBSsum();
  void fillJPsum();
  void fillBS();
  void fillJP();

  TFile *mFile;
  char mFilename[100];

  static const int NBS=68; //6+1+6+6+1+6=26 for small, 10+1+10+10+1+10=42 for large
  static const int NJP=6;   
  static const int NTHR=3;

  TH1F *hBS[NBS];
  TH1F *hJP[NJP];
  TH1F *mBS[NTHR];
  TH1F *mJP[NTHR];
  TH2F *mDIBS;
  TH2F *mDIJP;

  ClassDef(StFmsTrgQaMaker,0);
};

#endif 

