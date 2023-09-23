#ifndef STAR_StDetectorDbMaker
#define STAR_StDetectorDbMaker

#ifndef STAR_StMaker
#include "StMaker.h"
#endif

class StDetectorDbMaker : public StMaker {
 public: 
 StDetectorDbMaker(const char *name="DetectorDb") : StMaker(name) {fgStDetectorDbMaker = this; }
  virtual       ~StDetectorDbMaker() {fgStDetectorDbMaker = 0;}
  virtual Int_t        InitRun(Int_t runumber);
  virtual Int_t  Make();
  static StDetectorDbMaker *instance() {return fgStDetectorDbMaker;}
 private:
  static  Int_t  _debug;  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StDetectorDbMaker.h,v 1.5 2015/12/23 23:34:09 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  static StDetectorDbMaker *fgStDetectorDbMaker;
  ClassDef(StDetectorDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif
