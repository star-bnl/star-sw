#ifndef STAR_MuEzSmdCalMaker
#define STAR_MuEzSmdCalMaker

/************************************************************
 * $Id: MuEzSmdCalMaker.h,v 1.1 2005/03/11 15:44:25 balewski Exp $
 ************************************************************
 Goal: wrap EEMC-Panitkin code to be used in the BFC
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "EEsmdCal.h"

class TObjArray  ;
class StMuDstMaker;
class StTriggerDataMother ;
class EztEmcRawData;
class EztEventHeader;
class EztTrigBlob;

class MuEzSmdCalMaker : public StMaker, public  EEsmdCal {

 private: 
  StMuDstMaker* mMuDstMaker;  
  int nInpEve, nTrigEve, nAcceptEve,nCorrEve; ///  event counters
  EztEventHeader *eHead;
  EztEmcRawData  *eETow;
  EztEmcRawData  *eESmd;
  EztTrigBlob    *eTrig;
  StTriggerDataMother *trgAkio;//  $STAR/StRoot/StEvent/StTriggerData2004.h
  int trigID; // filter only one trigger if non-zero
  int maxCtbSum; // filter on CTB sum if non-zero
  void unpackMuEzt(EztEmcRawData  *eRaw);


 public: 
  MuEzSmdCalMaker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual ~MuEzSmdCalMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual void Clear(const Option_t* = "");
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  void SetTrigIdFilter(int id) {trigID=id;}
  void SetMaxCtbSum(int x) {maxCtbSum=x;}
  void SetSector(int x);
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: MuEzSmdCalMaker.h,v 1.1 2005/03/11 15:44:25 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(MuEzSmdCalMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: MuEzSmdCalMaker.h,v $
// Revision 1.1  2005/03/11 15:44:25  balewski
// works with muEzt, cucu200
//
//
