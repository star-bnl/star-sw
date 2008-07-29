//  $Id: StEmcAsciiDbMaker.h,v 1.1 2008/07/29 14:43:11 balewski Exp $ 
// export snapshot of E+B-EMC STAR DB in ASCII format
//  \author Jan Balewski, IUCF, 2006 
// Interface to online/L2jetAlgo/

#ifndef EMCASCIIDBMAKER_H
#define EMCASCIIDBMAKER_H


#include "StMaker.h"

class  StEEmcDbMaker;
class  StEmcGeom;
class  StEmcDecoder;

class StEmcAsciiDbMaker : public StMaker {
 public:
  StEmcAsciiDbMaker();
  ~StEmcAsciiDbMaker();

  Int_t Init(); 
  virtual Int_t InitRun(int runNo);
  Int_t Make();
  Int_t Finish();
  virtual void Clear(const Option_t* = "");

  //Histogram
  void SetHList(TObjArray * x){mHList=x;}
  void SetGain60Et() { mgain60Et=true; }
 
 private:

  StEEmcDbMaker *mEeDb;
  StEmcGeom     *mGeomB;
  StEmcDecoder  *mMappB; 
  bool mgain60Et;

  void exportBtowDb(TString fname, int runNo, int yyyymmdd,int hhmmss);
  void exportEtowDb(TString fname, int runNo, int yyyymmdd,int hhmmss);

  TObjArray  *mHList; /// output histo access point
  enum {mxH=4};
  TH1 * hA[mxH];

  void initAuxHisto(); 
  ClassDef(StEmcAsciiDbMaker,0)
};

#endif

// $Log: StEmcAsciiDbMaker.h,v $
// Revision 1.1  2008/07/29 14:43:11  balewski
// start
//
// Revision 1.1  2006/03/09 01:33:11  balewski
// start
//

