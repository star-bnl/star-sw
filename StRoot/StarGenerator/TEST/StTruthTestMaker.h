#ifndef __StTruthTestMaker_h__
#define __StTruthTestMaker_h__

#include "StMaker.h"
class TH1F;
class TH2F;

class StTruthTestMaker : public StMaker
{

 public:
  StTruthTestMaker( const Char_t *name="testGeant" ):
    StMaker(name),
    mDoGeant(false),
    hMatchedEta(0),
    hMatchedPhi(0),
    hMatchedPt(0),
    hMatchedInv(0),
    hMatchedPID(0),
    hMatchedQA(0),
    hMatchedEtaRes(0),
    hMatchedPhiRes(0),
    hMatchedPtRes(0),
    hMatchedInvRes(0),
    hNumMismatched(0),
    hPerMismatched(0),
    hPidMismatched(0)
  { 
    /* nada */ 
  };
  ~StTruthTestMaker(){ /* nada */ };
    
  Int_t Init();
  Int_t Make();
  Int_t MakeGeant();
  Int_t MakeRecord();

 private:
 protected:

  Bool_t mDoGeant;

  TH2F *hMatchedEta; 
  TH2F *hMatchedPhi;
  TH2F *hMatchedPt ;
  TH2F *hMatchedInv;
  TH1F *hMatchedPID;
  TH1F *hMatchedQA ;
  
  TH1F *hMatchedEtaRes;
  TH1F *hMatchedPhiRes;
  TH1F *hMatchedPtRes ;
  TH1F *hMatchedInvRes;
  
  TH1F *hNumMismatched;
  TH1F *hPerMismatched;
  TH1F *hPidMismatched;

  ClassDef( StTruthTestMaker, 1 );
};

#endif
