#ifndef STELECTRONBEMCSTATUSMAKER_H
#define STELECTRONBEMCSTATUSMAKER_H

#include "StMaker.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "TFile.h"
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
  StMuDstMaker* mMuDstMaker;//!

  ClassDef(StBemcStatusMaker,0)
};

#endif
