#ifndef STAR_MuEzSoloPi0Maker
#define STAR_MuEzSoloPi0Maker

/************************************************************
 * $Id: MuEzSoloPi0Maker.h,v 1.3 2005/03/11 15:39:50 balewski Exp $
 ************************************************************
 Goal: wrap EEMC-Panitkin code to be used in the BFC
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "EEsoloPi0.h"

class TObjArray  ;
class StMuDstMaker;
class StTriggerDataMother ;
class EztEmcRawData;
class EztEventHeader;
class EztTrigBlob;

class MuEzSoloPi0Maker : public StMaker, public  EEsoloPi0 {

 private: 
  StMuDstMaker* mMuDstMaker;  
  int nInpEve, nTrigEve, nAcceptEve,nCorrEve; /// no. of input events
  EztEventHeader *eHead;
  EztEmcRawData  *eETow;
  EztEmcRawData  *eESmd;
  EztTrigBlob    *eTrig;
  StTriggerDataMother *trgAkio;//  $STAR/StRoot/StEvent/StTriggerData2004.h
  int trigID; // filter only one trigger if non-zero
  int maxCtbSum; // filter on CTB sum if non-zero
  bool unpackMuEzTowers(int token); // EEMC data

 public: 
  MuEzSoloPi0Maker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual ~MuEzSoloPi0Maker();
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual void Clear(const Option_t* = "");
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  void SetTrigIdFilter(int id) {trigID=id;}
  void SetMaxCtbSum(int x) {maxCtbSum=x;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: MuEzSoloPi0Maker.h,v 1.3 2005/03/11 15:39:50 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(MuEzSoloPi0Maker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: MuEzSoloPi0Maker.h,v $
// Revision 1.3  2005/03/11 15:39:50  balewski
// use corruption method from muEzt
//
// Revision 1.2  2005/03/01 20:02:15  balewski
// hack to access 2005 trigger data
//
// Revision 1.1  2005/02/05 04:56:31  balewski
// reads ezTree from muDst
//
//
//
