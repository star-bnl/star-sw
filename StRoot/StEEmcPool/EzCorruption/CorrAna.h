// \class  CorrAna
// \author Renee Fatemi
#ifndef CorrAna_h
#define CorrAna_h
/*********************************************************************
 * $Id: CorrAna.h,v 1.3 2007/07/12 19:24:31 fisyak Exp $
 * modified by rfatemi 2004/06/22
 *********************************************************************/

class EEmcGeomSimple;
class TVector3;
class TH1F;
class EEmcDbItem;
class StEmcDecoder;
class TH2F;

/// the ultimate source of dimensions is in this header

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include "StEEmcPool/EzCorruption/BEdims.h"
#include "TString.h"

class CorrAna {

 private:

  EEmcGeomSimple *geom;
  StEmcDecoder *bemcDec;
  float th1,th2;/// some thresholds
  TH2F *hBadc[MaxBTwCrate]; //good  BEMC crate histograms
  TH2F *cBadc[MaxBTwCrate]; //corrupt BEMC crate histograms
  TH2F *hEadc[MaxTwCrates]; //good  EEMC crate histograms
  TH2F *cEadc[MaxTwCrates]; //corrupt EEMC crate histograms
  TH2F *hESadc[MaxEsmdCrate]; //good  EEMC crate histograms
  TH2F *cESadc[MaxEsmdCrate]; //corrupt EEMC crate histograms
  TH1F *hBdiag[2];//BEMC diagnostics
  TH1F *hEdiag[2];//EEMC disgnostics
  TH1F *hESdiag[2];//EEMC disgnostics
  TH1F *hDiag;//Corruption disgnostics

 protected:
  // only this variables cna be altered by extrenal classes
  int nInpEve; /// no. of input events

  /// local event storage
  float towerE[MaxEtaBins][MaxPhiBins];
  int corruptE[MaxTwCrates][MaxTwCrateCh];//EEMC adc for corrupt events
  int corruptES[MaxEsmdCrate][MaxMapmtCrateCh];//EEMC adc for corrupt events
  int corruptB[MaxBTwCrate][MaxBTwCrateCh];//BEMC adc for corrupt events
  int crateE[MaxTwCrates][MaxTwCrateCh];//EEMC adc for good events
  int crateES[MaxEsmdCrate][MaxMapmtCrateCh];//EEMC adc for good events
  int crateB[MaxBTwCrate][MaxBTwCrateCh];//BEMC adc for good events
  int badEcrate[MaxTwCrates+1];//Crate #s which are bad in an event
  int badEScrate[MaxEsmdCrate+1];//Crate #s which are bad in an event
  int badBcrate[MaxBTwCrate+1];//Crate #s which are bad in an event
  int Blist[MaxBTwCrate][8];
  int Elist[MaxTwCrates][8];
  int ESlist[MaxEsmdCrate][8];  
  UChar_t Esanity;
  UChar_t ESsanity;
  UChar_t Bsanity;

  void clear();
  void taskEgood();
  void taskEbad();
  void taskESgood();
  void taskESbad();
  void taskBgood();
  void taskBbad();
  void taskDiag();
  void taskBall();

  int allBad;
  int mode;

  //  EEDB *eeDb; /// DB access point
  TObjArray  *HList; /// output histo access point
  
 public:
  
  CorrAna();
  virtual ~CorrAna();
  void print();
  void finish();

  void init(); 
  void setThres(float a, float b ){ th1=a; th2=b;}
  void saveHisto(TString fname="fixMe3");
 
  ClassDef(CorrAna,1) 
};
#endif


/*****************************************************************
 * $Log: CorrAna.h,v $
 * Revision 1.3  2007/07/12 19:24:31  fisyak
 * Add includes for TString and TMath for ROOT 5.16
 *
 * Revision 1.2  2004/07/26 22:54:26  rfatemi
 * Corruption Update
 *
 * Revision 1.1  2004/07/24 22:51:08  balewski
 * first
 *
 * Revision 1.1  2004/06/06 04:54:10  balewski
 * dual analyzis
 *
 *
 ********************************************************************/

