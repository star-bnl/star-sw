#ifndef STAR_StDetectorDbMaker
#define STAR_StDetectorDbMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StDetectorDbMaker : public StMaker {
 public: 
  StDetectorDbMaker(const char *name="DetectorDb") : StMaker(name) {}
  virtual       ~StDetectorDbMaker() {}
  virtual Int_t  Make();
  static  Int_t  _debug;  
  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StDetectorDbMaker.h,v 1.5 2015/12/23 23:34:09 fisyak Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
  
  ClassDef(StDetectorDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif
