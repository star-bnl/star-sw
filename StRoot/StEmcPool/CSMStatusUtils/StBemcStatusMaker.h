#ifndef STELECTRONBEMCSTATUSMAKER_H
#define STELECTRONBEMCSTATUSMAKER_H

#include "StMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
class StEEmcDb;

#include "TFile.h"
#include "TTree.h"
#include "TH2.h"

#include <string>
using namespace std;

class StBemcStatusMaker : public StMaker {
 public:
  StBemcStatusMaker(StMuDstMaker* mudstmaker);
  ~StBemcStatusMaker(){}
  
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
  
  void setOutputDirectory(const Char_t *directory){mOutputDirectory = directory;}
  void setOutputFilePrefix(const Char_t *prefix){mOutputFilePrefix = prefix;}
  
 //private:
  TH2F* getBemcAdcHist(Int_t runnumber);
  TH2F* getBemcEnergyHist(Int_t runnumber);
  TH2F* getEemcAdcHist(Int_t runnumber);
  TH2F* getEemcEnergyHist(Int_t runnumber);
 private:
  string mOutputDirectory; //!
  string mOutputFilePrefix; //!
  TFile* mOutputFile; //!
  TTree* mOutputTree; //!
  Float_t mFillNumber;
  Int_t mEemcStatusBits[720];
  Int_t mEemcFailBits[720];
  Int_t mBemcStatusBits[4800];
  Int_t mTheDate;
  Int_t mTheTime;
  Bool_t mFirstEvent;
  StMuDstMaker* mMuDstMaker;//!
  StEEmcDb* eeDb;

  ClassDef(StBemcStatusMaker,0)
};

#endif
