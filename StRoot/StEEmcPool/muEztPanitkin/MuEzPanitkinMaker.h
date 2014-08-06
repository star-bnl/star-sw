#ifndef STAR_MuEzPanitkinMaker
#define STAR_MuEzPanitkinMaker

/************************************************************
 * $Id: MuEzPanitkinMaker.h,v 1.4 2014/08/06 11:43:03 jeromel Exp $
 ************************************************************
 Goal: wrap EEMC-Panitkin code to be used in the BFC
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif


class TObjArray  ;
class StMuDstMaker;
class StTriggerDataMother ;
class EztEmcRawData;
class EztEventHeader;
class EEqaSorter;
class EztTrigBlob;
class RawPixels;

class MuEzPanitkinMaker : public StMaker{

 private: 
  StMuDstMaker* mMuDstMaker;  
  int nInpEve, nTrigEve; /// no. of input events
  TObjArray  *HList; /// output histo access point
  EztEventHeader *eHead;
  EztEmcRawData  *eETow;
  EztEmcRawData  *eESmd;
  EztTrigBlob    *eTrig;
  StTriggerDataMother *trgAkio;//  $STAR/StRoot/StEvent/StTriggerData2004.h
  int trigID; // filter only one trigger if non-zero

  RawPixels *rawPixels;
  bool pixlesOn;

  bool eeSpyOn;

 public: 
  EEqaSorter *qaSort;// tmp public
  MuEzPanitkinMaker(const Char_t *self="myPanitkin", const Char_t * muDstMakerName="MuDst");
  virtual       ~MuEzPanitkinMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual void Clear(const Option_t* = "");
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void SetHistoPixels(bool x=true) {pixlesOn=x;}
  void SetSpy(bool x=true) {eeSpyOn=x;}
  void saveHisto(TString fname="fixMe3");
  void SetTrigIdFilter(int id) {trigID=id;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: MuEzPanitkinMaker.h,v 1.4 2014/08/06 11:43:03 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(MuEzPanitkinMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: MuEzPanitkinMaker.h,v $
// Revision 1.4  2014/08/06 11:43:03  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.3  2009/12/03 22:35:03  ogrebeny
// Fixed compiler warnings, mostly char* -> const char*
//
// Revision 1.2  2005/07/15 15:37:36  balewski
// *** empty log message ***
//
// Revision 1.1  2005/04/28 20:54:46  balewski
// start
//
//
//
