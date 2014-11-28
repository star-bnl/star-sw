#ifndef STAR_StFgtDbFileMaker
#define STAR_StFgtDbFileMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

#include "StFgtUtil/geometry/StFgtGeom.h"
#include "StRoot/StFgtUtil/StFgtConsts.h"

class StFgtDbFileMaker : public StMaker {
 private:
  
  Short_t disk,quad,strip;
  Char_t layer;
  Int_t rdo,arm,apv,apvMod,channel;
  Int_t r_rdo, r_arm, r_apv, r_apvMod, r_channel;
  Int_t geoId,electId, realElectId;
  Int_t mapping[51200];

  Int_t getElectId(Int_t r, Int_t a, Int_t v, Int_t c);
  void printIdealDbMappingFile();
  void printRealDbMappingFile();
  void printBigFgtGeomMap();
  Int_t searchPhiStripId(Double_t);
  Int_t searchRStripId_HighPhi(Double_t);
  Int_t searchRStripId_LowPhi(Double_t);

 protected:
 

 public: 
  StFgtDbFileMaker(const char *name="TLA");
  virtual       ~StFgtDbFileMaker();
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();

  StFgtGeom *geom;
  
 
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StFgtDbFileMaker.h,v 1.3 2014/08/06 11:43:11 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StFgtDbFileMaker,0)   //StAF chain virtual base class for Makers
};

#endif

