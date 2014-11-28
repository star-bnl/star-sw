#ifndef StPhiEtaHitList_h
#define StPhiEtaHitList_h
#include "TMath.h"
#include "TString.h"
#include "TArrayC.h"
#include "TNamed.h"
#include <vector>
#include "TH2.h"
typedef std::vector<Int_t>  IntVec;
//________________________________________________________________________________
class StPhiEtaHitList {
 public:
  StPhiEtaHitList();
  virtual  ~StPhiEtaHitList();
  void clear() {mFired->Reset(); mTrack->Reset();}
  void initRun() {mActive->Reset(); clear();}
  Int_t etaBin(Double_t eta) {return mActive->GetYaxis()->FindBin(eta);}
  Int_t phiBin(Double_t phi) {return mActive->GetXaxis()->FindBin(phi);}
  Int_t PhiEtaBin(Double_t phi, Double_t eta) {Int_t binx = phiBin(phi); Int_t biny = etaBin(eta); 
    return (binx < 0 || biny < 0) ? -1 : biny*(mActive->GetYaxis()->GetNbins()+2) + binx;}
  Int_t addTrack(Float_t eta, Float_t phi);
  Int_t getActive(Int_t iBin) { return mActive->GetBinContent(iBin);}
  Int_t getFired(Int_t iBin)  { return mFired->GetBinContent(iBin);}
  Int_t getTrack(Int_t iBin)  { return mTrack->GetBinContent(iBin);}
  Bool_t isMatched(Int_t iBin);
  Bool_t isVetoed(Int_t iBin);
  Float_t getWeight(Bool_t active, Bool_t matched, Bool_t vetoed) {
    Float_t Wdunno = 1;
    if (! active  ) return Wdunno;
    if (  matched ) return Wmatch;  
    if (  vetoed  ) return Wveto;
    return Wdunno;
  }
  Float_t getWeight(Int_t iBin) {
    return getWeight(getActive(iBin) > 1, isMatched(iBin), isVetoed(iBin));
  }
  Int_t    getnFired(){ return mFired->GetEntries(); }
  Int_t    iPhiEta2bin(Int_t iPhi,Int_t iEta) {return mActive->GetBin(iPhi,iEta);}
  void     iBin2iPhiEta(Int_t iBin,Int_t &iPhi,Int_t &iEta) {Int_t binz; mActive->GetBinXYZ(iBin, iPhi, iEta, binz);}
  void     setActive(Int_t iBin) { mActive->AddBinContent(iBin); }
  void     setFired(Int_t iBin)  { mFired->AddBinContent(iBin); }
  void     setTrack(Int_t iBin)  { mTrack->AddBinContent(iBin); }
  Double_t MinPhi() {return phiMin;}
  Int_t    NoHits() {return mFired ? mFired->GetEntries():0;}
  TH2C    *Active() {return mActive;}
  TH2C    *Fired()  {return mFired;}
  TH2C    *Track()  {return mTrack;}
  static void     SetDebug(Int_t k = 1);
  static Int_t    Debug();
  static Double_t W(Double_t energy);
 protected:
  Char_t  beg[1]; //!
  TH2C    *mActive;
  TH2C    *mFired;
  TH2C    *mTrack;
  Double_t phiMin;
  Char_t   end[1]; //!
  Float_t  Wmatch, Wveto;
  static  const Char_t *names[3];
  ClassDef(StPhiEtaHitList,0)
};
//________________________________________________________________________________
#include "StBTofUtil/StBTofGeometry.h"
class StBTofCollection;
class StBtofHitList : public  StPhiEtaHitList {
 public:
  enum {mxTray=120,mxModule=32,mxCell=6,mxHalfTray=mxTray/2,mxTrayCell=mxHalfTray*mxCell};
  StBtofHitList();
  virtual        ~StBtofHitList();
  static StBtofHitList* instance() {return fgInstance;}
  void            initRun();
  void            build(StBTofCollection *btofColl);
  Double_t        Phi(Int_t tray, Int_t /* module */, Int_t cell) { return mxCell*((tray-1)%mxHalfTray) + cell + 0.5;}
  Double_t        Eta(Int_t tray, Int_t     module  , Int_t /* cell */) { return (tray <= mxHalfTray) ? module -0.5 : -module + 0.5;}
  Int_t           addBtofTrack(Int_t tray, Int_t module, Int_t cell);
  Bool_t          isMatched(IntVec ibinVec);
  Bool_t          isMatched(Int_t iBin) {return StPhiEtaHitList::isMatched(iBin);}
  Bool_t          isVetoed(IntVec ibinVec);
  Float_t         getWeight(IntVec ibinVec);
 private:
  Int_t tmc2bin[mxTray][mxModule][mxCell]; // map {t,m,c}--> my bin
  static StBtofHitList* fgInstance;
  ClassDef(StBtofHitList,0)
};
//________________________________________________________________________________
class StTriggerData;
class TDataSet;
class StCtbHitList : public StPhiEtaHitList {
 public:
  enum {mxPhi= 60, mxPhi1=61,mxEta1=5};
  enum {mxSlat=2, mxTray=120};
  StCtbHitList(); 
  virtual ~StCtbHitList() {fgInstance = 0;}
  static StCtbHitList* instance() {return fgInstance;}
  
  void clear() { StPhiEtaHitList::clear(); mgeantE->Reset();}
  void initRun(Float_t fac=1.); 
  void buildFromMC(TDataSet *gds);
  void buildFromData(StTriggerData *trgD);
  static void ctb_get_slat_from_data(Int_t slat, Int_t tray, Float_t & phiRad, Float_t &eta);
  static void ctb_get_slat_from_geant(Int_t volume, Int_t &i_phi, Int_t &i_eta, Float_t & phi, Float_t &eta);
  TH2F   *GeantE() {return mgeantE;}
 private:
  Float_t mCtbThres_mev; // M-C hits
  Int_t   mCtbThres_ch;// data hits 
  TH2F   *mgeantE;
  static StCtbHitList* fgInstance;
  ClassDef(StCtbHitList,0)
};
class StEmcDetector;
#include "StEmcRawMaker/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

class StBemcHitList : public StPhiEtaHitList {
 public:
  enum {mxm=120,mxe=20,mxs=2};
  StBemcHitList();
  virtual  ~StBemcHitList() {fgInstance = 0;}
  void initRun();
  void build( StEmcDetector*det, Float_t adcMin);
  static StBemcHitList* instance() {return fgInstance;}
  StBemcTables   *Table() {return myTable;}
 private:
  StBemcTables *myTable;
  StEmcGeom *geomB;
  // params:
  Float_t kSigPed;
  static StBemcHitList* fgInstance;
  ClassDef(StBemcHitList,0)
};
#include "StEEmcUtil/EEfeeRaw/EEdims.h"
class StEmcDetector;
class StEEmcDb;
class EEmcGeomSimple;

class StEemcHitList : public StPhiEtaHitList {
 public:
  StEemcHitList(StEEmcDb* x, UInt_t y, EEmcGeomSimple *z);
  virtual  ~StEemcHitList() {fgInstance = 0;}
  void initRun();
  void build( StEmcDetector*det, Float_t adcMin);
  static StEemcHitList* instance() {return fgInstance;}
 private:
  StEEmcDb* eeDb; 
  EEmcGeomSimple *geomE;
  UInt_t killStatEEmc;
  static StEemcHitList* fgInstance;
  ClassDef(StEemcHitList,0)
};

#endif
