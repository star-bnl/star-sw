#ifndef STAR_StTrigOnlyPanitkinMaker
#define STAR_StTrigOnlyPanitkinMaker

/************************************************************
 * $Id: StTrigOnlyPanitkinMaker.h,v 1.2 2014/08/06 11:43:03 jeromel Exp $
 ************************************************************
 Goal: wrap EEMC-Panitkin code to be used in the BFC
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif

class EEdsmAna;

class StTrigOnlyPanitkinMaker : public StMaker{

 private: 
  EEdsmAna *dsm;
  TObjArray * HList;
  int nTrigEve,nInpEve;

 public: 
  StTrigOnlyPanitkinMaker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual       ~StTrigOnlyPanitkinMaker();
  virtual Int_t Init();
  
  virtual void Clear(const Option_t* = "");
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StTrigOnlyPanitkinMaker.h,v 1.2 2014/08/06 11:43:03 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StTrigOnlyPanitkinMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StTrigOnlyPanitkinMaker.h,v $
// Revision 1.2  2014/08/06 11:43:03  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2005/06/17 17:41:13  balewski
// *** empty log message ***
//
//
//
