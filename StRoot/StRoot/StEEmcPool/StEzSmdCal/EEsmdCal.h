// \class  EEsmdCal
// \author Jan Balewski
#ifndef EEsmdCal_h
#define EEsmdCal_h
/*******************************************************
 * $Id: EEsmdCal.h,v 1.15 2009/12/03 22:35:03 ogrebeny Exp $
 *******************************************************
 * Descripion:
 *  Calibration of SMD/pre/post using MIPs from UxV
 *******************************************************/

class EEmcGeomSimple;
class TVector3;
class TH1F ;
class EEmcDbItem;
class EEsmdPlain;
class EEmcSmdGeom;
class EEmcSmdMap;

/// the ultimate source of dimensions is in this header
#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include "TString.h"
/// the trick to switch between two DB readers
#ifdef StRootFREE
  class EEmcDb;
  typedef EEmcDb EEDB;
#else
  class StEEmcDb;
  typedef StEEmcDb EEDB;
#endif

class EEsmdCal {
 protected:
  enum {mxTile=4,kT=0, kP=1, kQ=2, kR=3, kU=0, kV=1}; // 0=tower, 1=pres1, 2=pres2, 3=post
  int thrMipPresAdc; // (ADC) threshold on MIP signal in pre/post

 private: 
  float thrMipSmdE; //(GeV)  threshold on MIP signal in SMD strip
  float twMipRelEneLow, twMipRelEneHigh; // relative maximal energy deviation for MIP in towers 

  float offCenter;// cut on deviation of UxV from tower center in eta & phi

  int emptyStripCount; // minimal # of SMD strops below threshold
  float towerMipE [MaxEtaBins]; // mean EM equiv energy for MIP in towers
  float presMipE  [MaxEtaBins]; // mean Elos energy for MIP in pre/post
  float smdAvrMipE   ; // mean Elos energy for MIP in SMD strip
  float twTghEta[MaxEtaBins]; // mean tgh(eta) for towers

  int dbMapped;// flag indicating local DB is mapped
  const EEmcDbItem *dbT[mxTile][MaxEtaBins][MaxPhiBins]; // local fast access to DB
  const EEmcDbItem *dbS[MaxSmdPlains][MaxSmdStrips];// local fast access to DB
  
  // various utility classes
  EEmcSmdMap *mapSmd;  
  EEmcGeomSimple *geoTw;
  EEmcSmdGeom *geoSmd;

  // cuts: 0=inclusive, 1=tagged with PostShower,  2=Tagged & UxVinTower, etc.
  enum {kCut='h'-'a'}; 

  TH1F *hA[32]; // some global (test) histograms
  // all histograms are created for only one sector

  TH1F *hT[kCut][mxTile][MaxEtaBins][MaxPhiBins]; // tower histograms 
  TH1F *hSs[kCut][MaxSmdPlains][MaxSmdStrips]; // individual SMD strips ,inclusive
#if 0  //smdMap verification
  TH1F *hM[12];
  void scanSpike(float adc1, TH1F *h);
#endif

  void initTileHistoAdc(char cut, const char * title, int col=1); 
  void initTileHistoEne(char cut, const char * title, int col=1); 
  void initSmdHist(char cut, const char * title, int col=1);
  void initSmdEneHist(char cut, const char * title, int col=1);
  void initAuxHisto();
  void mapTileDb();
  void histoGains();
  void addTwMipEbarsToHisto (int col, char mxC);
  void addPresMipEbarsToHisto (int col, char cT);
  void addSmdMipEbarsToHisto (int col, char cT);
  
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
  bool killT[mxTile][MaxEtaBins][MaxPhiBins]; // mark tiles excluded from ana

  void clear();
  void findSectorMip();
  void calibAllwithMip(int iStrU, int iStrV);
  EEDB *eeDb; /// DB access point
  unsigned short killStat; // fatal stat bits
  float maxStripAdc; // suppress large jump in ped or sticky bits

  TObjArray  *HList; /// output histo access point

 public:
  
  EEsmdCal();
  virtual ~EEsmdCal();
  void finish(int k=0);

  void init(); 
  void initRun(int runID);// must be called after DB timestamp is known
  void setSmdCuts(float xs, int n1){ thrMipSmdE=xs; emptyStripCount=n1;}
  void setPreCuts(int n1){ thrMipPresAdc=n1;}
  void setTwCuts( float e1, float e2 ,float r){
    twMipRelEneLow=e1; twMipRelEneHigh=e2; offCenter=r; 
  }
  
  void saveHisto(TString fname="fixMe3");
 
  ClassDef(EEsmdCal,1) 
};
#endif



/*****************************************************************
 * $Log: EEsmdCal.h,v $
 * Revision 1.15  2009/12/03 22:35:03  ogrebeny
 * Fixed compiler warnings, mostly char* -> const char*
 *
 * Revision 1.14  2009/02/04 20:33:22  ogrebeny
 * Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
 *
 * Revision 1.13  2007/07/12 19:27:19  fisyak
 * Add includes for TMath for ROOT 5.16
 *
 * Revision 1.12  2005/05/04 17:00:32  balewski
 * tuned for MIP detection in CuCu200
 *
 * Revision 1.11  2005/02/05 00:41:37  perev
 * Remove forward decl of TMath. Conflicts with ROOT
 *
 * Revision 1.10  2004/10/08 14:34:39  balewski
 * as used for PQRUV calib for pp200, 2004
 *
 * Revision 1.9  2004/09/22 00:45:50  balewski
 * ready for calib of smd
 *
 * Revision 1.8  2004/09/11 04:57:34  balewski
 * cleanup
 *
 * Revision 1.7  2004/07/27 21:59:46  balewski
 * now runs on muDst as well
 *
 * Revision 1.6  2004/07/10 18:40:54  balewski
 * use now first and last 8 strips in 00xx00
 *
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

