// \class  EEsmdCal
// \author Jan Balewski
#ifndef EEsmdCal_h
#define EEsmdCal_h
/*********************************************************************
 * $Id: EEsmdCal.h,v 1.5 2004/07/08 01:20:20 balewski Exp $
 *********************************************************************
 * Descripion:
 *  Calibration of SMD/pre/post using MIPs from UxV
 *********************************************************************/

class EEmcGeomSimple;
class TVector3;
class TH1F ;
class TMath ;
class EEmcDbItem;
class EEsmdPlain;
class EEmcSmdGeom;
class EEmcSmdMap;

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
  enum {mxTile=4,kT=0,kP=1, kQ=2,kR=3, kU=0, kV=1}; // 0=tower, 1=pres1, 2=pres2, 3=post

 private: 
  
  float thrMipSmdE; // threshold on MIP signal in SMD strip
  float twMipRelEneLow, twMipRelEneHigh; // relative maximal energy deviation for MIP in towers 
  float presMipElow,presMipEhigh ; // relative maximal energy deviation for MIP in pres1,2,post 

  float offCenter;// cut on deviation of UxV from tower center in eta & phi

  int emptyStripCount; // minimal # of SMD strops below threshold
  float towerMipE [MaxEtaBins]; // mean energy for MIP
  int dbMapped;// flag indicating local DB is mapped
  const EEmcDbItem *dbT[mxTile][MaxEtaBins][MaxPhiBins]; // local fast access to DB
  
  // various utility classes
  EEmcSmdMap *mapSmd;  
  EEmcGeomSimple *geoTw;
  EEmcSmdGeom *geoSmd;
  
  // cuts: 0=inclusive, 1=tagged with PostShower,  2=Tagged & UxVinTower, etc.
  enum {kCut=7}; 

  TH1F *hA[32]; // some global (test) histograms
  // all histograms are created for only one sector

  TH1F *hT[kCut][mxTile][MaxEtaBins][MaxPhiBins]; // tower histograms 
  TH1F *hSs[kCut][MaxSmdPlains][MaxSmdStrips]; // individual SMD strips ,inclusive
  TH1F *hSp[kCut][MaxSmdPlains][MaxSmdStrips]; // pair of SMD strips
 
  enum {stripGang=20, MaxAt=15}; // study of attenuation in SMD
  //stripGang- how many strips are added together 
  TH1F *hSc[MaxSmdPlains][MaxSubSec][MaxAt];// SMD attenuation study  
  TH1F *hSeta[MaxSmdPlains][MaxSubSec][MaxAt];//    -//-
  TH1F *hSdist[MaxSmdPlains][MaxSubSec][MaxAt];//   -//-


  void initTileHistoAdc(char cut, char * title, int col=1); 
  void initTileHistoEne(char cut, char * title, int col=1); 
  void initSmdHist(char cut, char * title, int col=1);
  void initSmdAttenHist(); 
  void initAuxHisto();
  void mapTileDb();
  void addTwMipEbarsToHisto (int col, char mxC);
  void addPresMipEbarsToHisto (int col, char cT);
  
  void fillSmdHisto_a();
  void fillOneTailHisto(char cut, int iEta, int iPhi);

  int getUxVmip();

  // extension of event storage, also cleared for every eve
  EEsmdPlain *smdHitPl; // auxil. for MIP search in U/V planes

 protected:
  // only this variables can be altered by extrenal classes
  int nInpEve; /// no. of input events
  int sectID; /// calibrate only one sector
  int iSect; /// the same info, counted from 0
  void setSector(int x){sectID=x; iSect=x-1;}

  /// local event storage for all instrumented sectors
  /// remeber to clear all variables below for every event
  /// 360 deg (just incase)
  float tileAdc[mxTile][MaxEtaBins][MaxPhiBins]; // adc-ped for : T,P,Q,R
  float tileEne[mxTile][MaxEtaBins][MaxPhiBins]; // adc-ped/gain (if exist)
  bool  tileThr[mxTile][MaxEtaBins][MaxPhiBins]; //  == adc-ped>thr 

  /// 30 deg (only for  this sector)
  float smdAdc[MaxSmdPlains][MaxSmdStrips]; // adc-ped
  float smdEne[MaxSmdPlains][MaxSmdStrips]; // adc-ped/gain (if exist)

  void clear();
  void findSectorMip();
  void calibAllwithMip(int iStrU, int iStrV);
  EEDB *eeDb; /// DB access point
  TObjArray  *HList; /// output histo access point

 public:
  
  EEsmdCal();
  virtual ~EEsmdCal();
  void finish();

  void init(); 
  void initRun(int runID);// must be called after DB timestamp is known
  void setSmdCuts(float xs, int n1){ thrMipSmdE=xs; emptyStripCount=n1;}
  void setTwCuts( float e1, float e2 ){twMipRelEneLow=e1; twMipRelEneHigh=e2;}
  void setPQRCuts(float a, float b ){ presMipElow=a; presMipEhigh=b;  }
  
  void saveHisto(TString fname="fixMe3");
 
  ClassDef(EEsmdCal,1) 
};
#endif



/*****************************************************************
 * $Log: EEsmdCal.h,v $
 * Revision 1.5  2004/07/08 01:20:20  balewski
 * merged with Murad
 *
 * Revision 1.4  2004/06/29 16:37:41  balewski
 * towards SMD calib
 *
 * Revision 1.3  2004/06/22 23:31:11  balewski
 * few more gadgets added
 *
 * Revision 1.2  2004/06/15 20:03:26  balewski
 * to match web-descriptio
 *
 * Revision 1.1  2004/06/12 04:09:20  balewski
 * start
 *
 * Revision 1.1  2004/06/06 04:54:10  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

