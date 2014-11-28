// $Id: StJanBarrelDbMaker.h,v 1.2 2014/08/06 11:43:06 jeromel Exp $

#ifndef STAR_StJanBarrelDbMaker
#define STAR_StJanBarrelDbMaker

/*!
 *                                                                     
 * \class  StJanBarrelDbMaker
 * \author Jan Balewski
 * \date   November , 2008
 *
 * this class interfaces all barrel DB & geometry
 * it allowes to bypass any missing or wrong information
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class  TH2S;
class  TH2I;
class  TH1I;
class  TObjArray  ;

class  StEmcGeom;
class  StEmcDecoder;
class  StBemcTables;

#include "JanBarrelConst.h"

class StJanBarrelDbMaker : public StMaker {
 private:
  enum {kStarDb=0, kJanDb=1};
  int  par_dbType;

  // my private DB
  TH2S *janDb_bprsPed2D, *janDb_bprsSigPed2D;  
  TH1S *janDb_bprsStat;  
  TH1I *janDb_bprsReMap, *janDb_btowReMap;
  // TH1I *janDb_bprsSoft2Hard;

  // mip calibration
  TH1F *janDb_mipMean[mxBTile]; 
  TH1F *janDb_mipSig[mxBTile];  // width of the distribution
  TH1F *janDb_mipStat[mxBTile]; // QA flag from MIP analysis
  
  TObjArray *HList; 
 
  StBemcTables* mTables; // used to acBcess EMC status and pedestal info

  StEmcDecoder*   mMappB;

  void initBprsGeometry();

 public: 
  StEmcGeom  *mBtowGeom, *mBprsGeom, * mSmdEGeom, * mSmdPGeom;// tmp
  StJanBarrelDbMaker(const char *name="janBarrelDb");
  virtual       ~StJanBarrelDbMaker();
  virtual Int_t Init();
  virtual Int_t  Make();
  virtual Int_t InitRun  (int runumber);

  void setDbType(int i) {par_dbType=i;}
  void setHList(TObjArray * x){HList=x;} 

  int bprsCrate(int softID);

  float pedTile(int ibp, int softID, int capID);
  float sigPedTile(int ibp, int softID, int capID);
  int   statTile(int ibp, int softID);
  TH1I *bprsReMap() const{ return  janDb_bprsReMap;}
  TH1I *btowReMap() const{ return  janDb_btowReMap;}
  TH1F *mipMean(int ibp) const { assert(ibp>=0 && ibp<mxBTile); return janDb_mipMean[ibp];}
  TH1F *mipSig(int ibp) const { assert(ibp>=0 && ibp<mxBTile); return janDb_mipSig[ibp];}
  TH1F *mipStat(int ibp) const { assert(ibp>=0 && ibp<mxBTile); return janDb_mipStat[ibp];}

  float cut_mipAdcL[mxBTile][mxBtow];
  float cut_mipAdcH[mxBTile][mxBtow];
  void cut_mipAdcLH(int ibp, int id, float &L, float &H) { 
    assert(ibp>=0 && ibp<mxBTile); assert(id>0 && id<=mxBtow);
    L=cut_mipAdcL[ibp][id-1]; H=cut_mipAdcH[ibp][id-1]; }
  

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StJanBarrelDbMaker.h,v 1.2 2014/08/06 11:43:06 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StJanBarrelDbMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StJanBarrelDbMaker.h,v $
// Revision 1.2  2014/08/06 11:43:06  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2008/11/24 23:06:37  balewski
// start
//
