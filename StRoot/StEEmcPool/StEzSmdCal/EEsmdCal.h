// \class  EEsmdCal
// \author Jan Balewski
#ifndef EEsmdCal_h
#define EEsmdCal_h
/*********************************************************************
 * $Id: EEsmdCal.h,v 1.1 2004/06/12 04:09:20 balewski Exp $
 *********************************************************************
 * Descripion:
 *  Calibration of SMD/pre/post using MIPs from UxV
 *********************************************************************/

class EEmcGeomSimple;
class TVector3;
class TH1F ;
class EEmcDbItem;
class EEsmdPlain;
class  EEmcSmdGeom;

/// the ultimate source of dimensions is in this header
#include "StEEmcUtil/EEfeeRaw/EEdims.h"


/// the trick to switch between two DB readers
#ifdef StRootFREE
  class EEmcDb;
  typedef EEmcDb EEDB;
#else
  class StEEmcDbMaker;
  typedef StEEmcDbMaker EEDB;
#endif


class EEsmdCal {
 protected:
  enum {kTile=4}; // 0=tower, 1=pres1, 2=pres2, 3=post

 private:
  EEmcGeomSimple *geoTw;
  EEmcSmdGeom *geoSmd;

  int sectID; /// calibrate only one sector
  int iSect; /// the same info, counted from 0
  
  float thrMipSmdE; // threshold on MIP signal in SMD strip
  // cuts: 0=inclusive, 1=VxV, 2=vetoUxV, 3&4=1&2 but with fiducial tower cut
  enum {kCut=5}; 

  TH1F *hA[32]; // some global (test) histograms
  // all histograms are created for only one sector

  TH1F *hT[kCut][kTile][MaxEtaBins][MaxPhiBins]; // tower histograms 
  TH1F *hSs[MaxSmdPlains][MaxSmdStrips]; // individual SMD strips ,inclusive
  TH1F *hSp[kCut][MaxSmdPlains][MaxSmdStrips]; // pair of SMD strips

  void initTileHist(char cut, char * title); 
  void initSmdHist(char cut, char * title); 
  void initAuxHisto();
  
  void fillTailHisto_a();
  void fillSmdHisto_a();
  void fillTailHisto1(char cut, int iEta, int iPhi);

  int getUxVmip();

  // extension of event storage, also cleared
  EEsmdPlain *smdHitPl; // auxil. for MIP search in U/V planes

 protected:
  // only this variables can be altered by extrenal classes
  int nInpEve; /// no. of input events
  void setSector(int x){sectID=x; iSect=x-1;}

  /// local event storage for all instrumented sectors
  /// remeber to clear all variables bewlow for every event
  float tileAdc[kTile][MaxEtaBins][MaxPhiBins]; // adc-ped for : T,P,Q,R
  int tileThr[kTile][MaxEtaBins][MaxPhiBins]; // adc-ped>thr for : T,P,Q,R
  float smdE[MaxSectors][MaxSmdPlains][MaxSmdStrips]; // energy in arb.uni.

  void clear();
  void findSectorMip();
  void findOneMip(int iStrU, int iStrV);
  EEDB *eeDb; /// DB access point
  TObjArray  *HList; /// output histo access point
  
 public:
  
  EEsmdCal();
  virtual ~EEsmdCal();
  void finish();

  void init(); 
  void saveHisto(TString fname="fixMe3");
 
  ClassDef(EEsmdCal,1) 
};
#endif



/*****************************************************************
 * $Log: EEsmdCal.h,v $
 * Revision 1.1  2004/06/12 04:09:20  balewski
 * start
 *
 * Revision 1.1  2004/06/06 04:54:10  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

