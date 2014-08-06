#ifndef STAR_MuEzSmdCalMaker
#define STAR_MuEzSmdCalMaker

/************************************************************
 * $Id: MuEzSmdCalMaker.h,v 1.4 2014/08/06 11:43:01 jeromel Exp $
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
  int  nTrigEve, nAcceptEve,nCorrEve; ///  event counters
  EztEventHeader *eHead;
  EztEmcRawData  *eETow;
  EztEmcRawData  *eESmd;
  EztTrigBlob    *eTrig;
  bool useEZtree;
  StTriggerDataMother *trgAkio;//  $STAR/StRoot/StEvent/StTriggerData2004.h
  int trigID; // filter only one trigger if non-zero
  int  maxCtbSum; // filter on CTB sum if non-zero
  void unpackMuEzt(EztEmcRawData  *eRaw);
  void unpackMuTails();
  void unpackMuSmd();
  void killTail( const  EEmcDbItem  *x, int iT);
  void recordTail( const  EEmcDbItem *x, float  adc, bool aboveThr,int iT); 
  int  stripReMap(const  EEmcDbItem  *x); // use only be expert
  void tileReMap( int &iT, int &sec , char &sub , int &eta); //by expert

  Int_t  MakeEZtree();   // different source of input data
  Int_t  MakeRegular();

 public: 
  MuEzSmdCalMaker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual ~MuEzSmdCalMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual void Clear(const Option_t* = "");
  virtual Int_t Finish();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  void setTrigIdFilter(int id) {trigID=id;}
  void setMaxCtbSum(int x) {maxCtbSum=x;}
  void setSector(int x);
  void setEZtree(bool x=true){useEZtree=x;}
  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: MuEzSmdCalMaker.h,v 1.4 2014/08/06 11:43:01 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(MuEzSmdCalMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: MuEzSmdCalMaker.h,v $
// Revision 1.4  2014/08/06 11:43:01  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.3  2005/09/29 13:57:57  balewski
// after SMD gains were rescaled
//
// Revision 1.2  2005/05/04 17:00:32  balewski
// tuned for MIP detection in CuCu200
//
// Revision 1.1  2005/03/11 15:44:25  balewski
// works with muEzt, cucu200
//
//
