// \class  EEsmdCal
// \author Jan Balewski
#ifndef EEsmdCal_h
#define EEsmdCal_h
/*********************************************************************
 * $Id: EEsmdCal.h,v 1.3 2004/06/22 23:31:11 balewski Exp $
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
  enum {kTile=4,kT=0,kP=1, kQ=2,kR=3}; // 0=tower, 1=pres1, 2=pres2, 3=post

 private: 
  int sectID; /// calibrate only one sector
  int iSect; /// the same info, counted from 0
  
  float thrMipSmdE; // threshold on MIP signal in SMD strip
  float twMipEdev; // maximal energy deviation for MIP in towers 
  int emptyStripCount; // minimal # of SMD strops below threshold
  float towerMipElow [MaxEtaBins]; // lower energy range for MIP
  float towerMipEhigh[MaxEtaBins]; // high  energy range for MIP
  int dbMapped;// flag indicating local DB is mapped
  const EEmcDbItem *dbT[kTile][MaxEtaBins][MaxPhiBins]; // local fast access to DB
  // cuts: 0=inclusive, 1=tagged with PostShower,  2=Tagged & UxVinTower
  enum {kCut=5}; 

  TH1F *hA[32]; // some global (test) histograms
  // all histograms are created for only one sector

  TH1F *hT[kCut][kTile][MaxEtaBins][MaxPhiBins]; // tower histograms 
  TH1F *hSs[MaxSmdPlains][MaxSmdStrips]; // individual SMD strips ,inclusive
  TH1F *hSp[kCut][MaxSmdPlains][MaxSmdStrips]; // pair of SMD strips

  void initTileHist(char cut, char * title, int col=1); 
  void initSmdHist(char cut, char * title, int col=1); 
  void initAuxHisto();
  void mapTileDb();
  void addTwMipEbarsToHisto (int col);
  
  void fillSmdHisto_a();
  void fillOneTailHisto(char cut, int iEta, int iPhi);

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
  float tileEne[kTile][MaxEtaBins][MaxPhiBins]; // adc-ped/gain (if exist)
  bool  tileThr[kTile][MaxEtaBins][MaxPhiBins]; //  == adc-ped>thr 
  float smdEne[MaxSectors][MaxSmdPlains][MaxSmdStrips]; // adc-ped/gain (if exist)

  void clear();
  void findSectorMip();
  void findOneMip(int iStrU, int iStrV);
  EEDB *eeDb; /// DB access point
  TObjArray  *HList; /// output histo access point
  
 public:
  
  EEmcGeomSimple *geoTw;
  EEmcSmdGeom *geoSmd;

  EEsmdCal();
  virtual ~EEsmdCal();
  void finish();

  void init(); 
  void initRun(int runID);// must be called after DB timestamp is known
  void setMipCuts(float x, int y, float z) 
    { thrMipSmdE=x; emptyStripCount=y; twMipEdev=z;}

  void saveHisto(TString fname="fixMe3");
 
  ClassDef(EEsmdCal,1) 
};
#endif



/*****************************************************************
 * $Log: EEsmdCal.h,v $
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

