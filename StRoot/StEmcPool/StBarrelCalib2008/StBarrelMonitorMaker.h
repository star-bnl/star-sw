#ifndef STAR_StBarrelMonitorMaker
#define STAR_StBarrelMonitorMaker

/************************************************************
 * $Id: StBarrelMonitorMaker.h,v 1.3 2014/08/06 11:43:06 jeromel Exp $
 ************************************************************
 Goal: Unpack & monitor barrel events
 *
 ******************************************************/

#ifndef StMaker_H
#include "StMaker.h"
#endif

class TObjArray  ;
class StMuDstMaker;
class StEmcDecoder;
class  TH3F;
class  TH2F;
class  TH2D;
class  TH2I;
class  TH1I;
class  BprsCapPolygraph;
class  BarrelMipCalib;

#include "JanBarrelConst.h"
#include "JanBarrelEvent.h"
#include "JanBprsEveA.h"

class  StEmcGeom;
class  StMuDstMaker;
class  StJanBarrelDbMaker;

//------- main class 
class StBarrelMonitorMaker : public StMaker {

 private: 

  JanBarrelEvent janEve;
  JanBprsEveA    janBprsEveA[mxBprsCrate]; // to monitor BPRS cpaID corruption
  BprsCapPolygraph *bprsPolygraph;
  BarrelMipCalib * mipCalib;

  int  nTrigEve, nAcceptEve,nCorrEve; ///  event counters
  int  trigID; // filter only one trigger if non-zero
  int  nInpEve; // no. of input events
  int  eventID; // recorded by DAQ
  int  mGeantEveInp;
  StMuDstMaker       *mMuDstMaker;
  StJanBarrelDbMaker *mJanDbMaker;

  // variable setup
  int  isMC; //0 for real data

  // interanal params
  enum {kPassPedSub=0x1, kPassCapFix=0x2};// WARN: bits
  int  par_calibPass; //  0=raw, 1=capPed , 2=capIdfix... stages of calibration
  int  par_bprsHisto; // 1=comCap, 2=128cap   

  char cTile[mxBTile];
  const char *cTile4[mxBTile];

  // output histograms
  TH1  *hTile[mxBTile]; // ADC spectra, one per channel
  TH1  *hBprsA[mxBprsCrate];
  TH3F *hBprs3D;// LARGE (softId,adc,capID)
  TH2D *hTonko0,*hTonko1, *hTonko2; 

  TObjArray *HList; 

  void initHistos();
  void initHistosTiles(int ibp);
  void initAuxBprsHistos();
  TH1F *addBprsEveHisto(int *cap);

  void unpackStTiles(int ibp); // BTOW=0 or BPRS=1 

  void  calibrateTiles(int ibp);
  void  populateBprsEveA();
  void  doTonkosBprsPeds();
  void  test1();
 
  StEmcGeom  *mBtowGeom, *mBprsGeom, * mSmdEGeom, * mSmdPGeom;
  StEmcDecoder*   mMappB;

 public: 
  StBarrelMonitorMaker(const char *self="janBarrelMonitor");
  virtual ~StBarrelMonitorMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(int);
  virtual void Clear(const Option_t* = "");
  virtual Int_t Finish();
  virtual Int_t  Make();
  void setHList(TObjArray * x){HList=x;} 
  void saveHisto(TString fname="fixMe3");
  void uploadJanCalib(char* path);
  void setTrigIdFilter(int id) {trigID=id;}
  void setMC(int x){isMC=x;}

  void setBprsHisto(int i) {par_bprsHisto=i;}
  void setCalibPass(int i) {par_calibPass=i;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StBarrelMonitorMaker.h,v 1.3 2014/08/06 11:43:06 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StBarrelMonitorMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StBarrelMonitorMaker.h,v $
// Revision 1.3  2014/08/06 11:43:06  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2009/08/25 16:08:04  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.1  2008/11/24 23:06:36  balewski
// start
//
// Revision 1.3  2005/09/29 13:57:57  balewski
// after SMD gains were rescaled
//
// Revision 1.2  2005/05/04 17:00:32  balewski
// tuned for MIP detection in CuCu200
//
// Revision 1.1  2005/03/11 15:44:25  balewski
// works with muEzt, cucu200
//
//

#if 0
  // full raw event: 360 deg, eta [-1,1]
  // ...... BSMD .....
  float     smdAdc[mxBSmd][mxBStrips]; // adc-ped
  float     smdEne[mxBSmd][mxBStrips]; // adc-ped/gain (if exist)
  float     smdDE[mxBSmd][mxBStrips] ; // geant dE
  int     smdUsed[mxBSmd][mxBStrips] ; // tags used hits
  unsigned int smdKill[mxBSmd][mxBStrips]; // mark strips excluded from ana

 // ...... BTOW .....
  float btowAdc[mxBTail][mxBtow]; // adc-ped
  float btowEne[mxBTail][mxBtow]; // adc-ped/gain (if exist)
  unsigned int btowKill[mxBTail][mxBtow]; // mark strips excluded from ana
  float btowDE[mxBTail][mxBtow] ; // geant dE
#endif


#if 0
  // private Jan's gain correction, maping , status tables
  bool jhOK;
  TH1F *jhGCorr[mxBSmd]; // gain corr vs. ID, full barrel
  TH1I *jhStat[mxBSmd];  //  status table vs. ID, full barrel
  TH2I *jhMap2id[mxBSmd];// mapping iEta-iPhi--> ID, full barrel
  TH1I *jhMapid2[mxBSmd];//mapping ID --> iEta-iPhi, full barrel
  TH1F *jhS2Tmap[mxBSmd]; // mapping strip ID --> tower ID + center distance, full barrel
  TH1F *jhSped[mxBSmd]; // strip pedestals
  float calibC0data, *calibC1;// BSMD
  enum {mxTw20=20};
  float calibTdata[mxTw20];// towers
#endif

#if 0 // iso-gamma params
  int   par_strWinLen; //length of strip window for cluster search
  float par_strEneThr; // GeV, energy threshold for strip
  float par_cluEneThr; // GeV, energy threshold for cluster
  float par_isoMinT3x3adc; 
  float par_isoMaxT3x3adc; 
  float par_isoTowerEneR;
  float par_isoRms;
  int   par_isoStripMargin;
  float par_kSigPed; // thres=ped+kSig*sigPed

  enum{ mxhS=16, mxhC=256};
  TH1 *hS[mxBSmd][mxhS]; // specific SMD-plane histos
  char cPlane[mxBSmd];
  TH1 *hC[mxhC]; // isolated clusters
  TH1 *hStrip[mxCutH][mxBSmd]; // raw spectra
  TH2F  *hX;

  void initHistosSMD(int iep,int modID);
  void initHistosIsoSmdCluster();
  void initHistosStrips(int iep,int iCut);

  void scanSmdEtaModule(int modID); // one physical module
  void scanSmdPhiBand(int iEtaBin_bsmdP); // slice from 60 modules
  bool scanStrips(char plane, int mxStr, float *eneA, int *usedA, unsigned int *killA, int *idA, BsmdCluster1D &cluster);// working horse for 1D cluster finder
  void pickIsoSmdCluster();
  float sum3x3towerADC(int towID,int size);
  void sete3x3TwoAdc(float x, float y) {par_isoMinT3x3adc=x;  par_isoMaxT3x3adc=y;} 
#endif
