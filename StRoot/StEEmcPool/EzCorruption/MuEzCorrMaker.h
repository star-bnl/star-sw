#ifndef STAR_MuEzCorrMaker
#define STAR_MuEzCorrMaker

/************************************************************
 * $Id: MuEzCorrMaker.h,v 1.1 2004/11/19 15:50:53 rfatemi Exp $
 ************************************************************
 Goal: XXXXXX detects stale EEMC data
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "CorrAna.h"

class TObjArray  ;
class StMuDstMaker;
class StTriggerData ;
class EztEmcRawData;
class EztEventHeader;
class MuEzCorrMaker : public StMaker, public CorrAna{

 private: 
  enum {mxH=16};
  StMuDstMaker* mMuDstMaker;  
  void   unpackTrigEzt();
  StTriggerData *trgAkio;
  TH1F *h[mxH]; // some  histograms
  TH2F *hTow[6];//Tower crate histos
  int nInpEve; /// no. of input events
  TObjArray  *HList; /// output histo access point
  int mode; /// type of action
  void scanETowCorrupt();
  void scanESmdCorrupt();
  void printCorrupt();
  EztEventHeader *eHead;
  EztEmcRawData  *eETow;
  EztEmcRawData  *eESmd;


 public: 
  MuEzCorrMaker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual       ~MuEzCorrMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  void test1(EztEmcRawData *, int flag=0);
  void DoPrintout() {mode=1;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: MuEzCorrMaker.h,v 1.1 2004/11/19 15:50:53 rfatemi Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(MuEzCorrMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: MuEzCorrMaker.h,v $
// Revision 1.1  2004/11/19 15:50:53  rfatemi
// Maker to check corruption from ezTree branch in MuDst
//
// Revision 1.2  2004/11/10 03:20:31  balewski
// trig fixed
//
// Revision 1.1  2004/11/02 14:37:16  balewski
// exampl eof stale data monitor
//
//
