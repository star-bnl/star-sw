// $Id: StEEstaleMaker.h,v 1.1 2004/11/02 14:37:16 balewski Exp $

#ifndef STAR_StEEstaleMaker
#define STAR_StEEstaleMaker

/************************************************************
 * $Id: StEEstaleMaker.h,v 1.1 2004/11/02 14:37:16 balewski Exp $
 ************************************************************
 Goal: detects stale EEMC data
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TObjArray  ;
class StMuDstMaker;
class StTriggerData ;
class EztEmcRawData;

class PastEve {
 public:
  EztEmcRawData *tw;
  int token;
  int n; // total number of events
};


class StEEstaleMaker : public StMaker{

 private: 
  enum {mxTkn=4097, mxH=8};
  PastEve past[mxTkn];

  StMuDstMaker* mMuDstMaker;  
  void   unpackTrigEzt();
  StTriggerData *trgAkio;
  TH1F *h[mxH]; // some  histograms
  int nInpEve; /// no. of input events
  TObjArray  *HList; /// output histo access point
  int timeFirst; // unix time of firts event in the file

  float  getTwChi2(EztEmcRawData* tw0,EztEmcRawData* tw1);

 public: 
  StEEstaleMaker(const char *self="EEstale", const char* muDstMakerName="muDstMaker");
  virtual       ~StEEstaleMaker();
  virtual Int_t Init();
  virtual Int_t Finish();
  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StEEstaleMaker.h,v 1.1 2004/11/02 14:37:16 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(StEEstaleMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StEEstaleMaker.h,v $
// Revision 1.1  2004/11/02 14:37:16  balewski
// exampl eof stale data monitor
//
//
