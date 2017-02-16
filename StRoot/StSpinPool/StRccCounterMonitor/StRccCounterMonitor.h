#ifndef STAR_StRccCounterMonitor
#define STAR_StRccCounterMonitor

#include "StMaker.h"

class TH1F;
class TH2F;
class TTree;
class StTriggerData;

class StRccCounterMonitor : public StMaker {
 public:
  StRccCounterMonitor(int run, const char *name="bbcqa");
  virtual ~StRccCounterMonitor(){};
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
   
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}
  
  void setDebug(int v=1) {mDebug=v;}
  
 protected:

 private:  
  int mDebug;
  int mRun;
  int mEventCounter;
  TFile *mFile;
  char mFilename[100];
  
  TH1F *mHist[14];
  TTree* mTree;

  ClassDef(StRccCounterMonitor, 0)   //StAF chain virtual base class for Makers
};
#endif
