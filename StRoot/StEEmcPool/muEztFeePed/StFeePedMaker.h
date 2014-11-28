// $Id: StFeePedMaker.h,v 1.2 2014/08/06 11:43:03 jeromel Exp $

#ifndef STAR_StFeePedMaker
#define STAR_StFeePedMaker

/************************************************************
 * $Id: StFeePedMaker.h,v 1.2 2014/08/06 11:43:03 jeromel Exp $
 ************************************************************
 Goal: detects stale EEMC data
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StEEmcUtil/EEfeeRaw/EEdims.h"

class TObjArray  ;
class StMuDstMaker;
class StTriggerData ;
class EztEmcRawData;

class StFeePedMaker : public StMaker{

 private: 

  const static int  MxTwFeeCh= MaxTwCrates* MaxTwCrateCh;
  TH1F *hped[MxTwFeeCh];// pedestals for each tower crate and FEE ch
  TObjArray  *HList; /// output histo access point
  int nInpEve; /// no. of input events
  StMuDstMaker* mMuDstMaker;  

 public: 
  StFeePedMaker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual       ~StFeePedMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  Int_t  fitPed(TH1F *h, int Xlow=5, int xHigh=4);

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFeePedMaker.h,v 1.2 2014/08/06 11:43:03 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StFeePedMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StFeePedMaker.h,v $
// Revision 1.2  2014/08/06 11:43:03  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2005/01/11 22:14:32  balewski
// start
//
