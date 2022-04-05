// $Id: St2009WMaker.h,v 1.17 2014/08/06 11:43:41 jeromel Exp $

#ifndef STAR_St2009WMaker
#define STAR_St2009WMaker

/*!
 *                                                                     
 * \class  St2009WMaker
 * \author Jan Balewski, MIT; 
 * \author Endcap: Justin Stevens, IUCF
 * \author JetFinder/JetReader interface: Ilya Selyuzhenkov, IUCF
 * \date   August 2009
 * \brief  muDst based extraction of W-signal from pp500 data from 2009
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TRandom3.h>

#include "Wevent2009.h"
#include "WtpcFilter.h"

class TObjArray;
class TH1I;
class TH2I;
class StMuDstMaker;
class  StEmcGeom;
class  StEmcDecoder;
class  StBemcTables;
class  StEEmcDb;       
class  EEmcGeomSimple; 
class  WeventDisplay;
class  St2009pubWanaMaker;
class  St2009pubJSMaker;
class  St2009pubSpinMaker;
class  St2009pubMcMaker;
class  St2009WjjMaker;
class  StMcJetCalibMaker;

class  StJetReader;
class  StJets;
class  StJet;
class  TClonesArray;
class  TRandom3;


class St2009WMaker : public StMaker {
 friend class WeventDisplay;
 friend class St2009pubWanaMaker;
 friend class St2009pubJSMaker;
 friend class St2009pubSpinMaker;
 friend class St2009pubMcMaker;
 friend class St2009WlumiMaker;
 friend class St2009ZMaker;
 friend class  St2009WjjMaker;
 friend class  StMcJetCalibMaker;

 private:
  StMuDstMaker* mMuDstMaker;
  StJetReader* mJetReaderMaker;
  int nJets;
  TString mJetTreeBranch,mJetTreeBranch_noEEMC;
  TClonesArray* mJets;
  Wevent2009 wEve;
  WeventDisplay *wDisaply;
  WtpcFilter mTpcFilter[mxTpcSec]; // allows sector dependent track filtering 
  int  nInpEve,nTrigEve, nAccEve; //  event counters
  int mRunNo;
  int isMC; //0 for real data

  //... internal params
  int   par_l0emulAdcThresh;
  float par_l2emulSeedThresh, par_l2emulClusterThresh;

  int   par_bht3TrgID, par_l2wTrgID;
  float par_vertexZ;
  int   par_minPileupVert;

  int   par_nFitPts;
  float par_nHitFrac, par_trackRin,  par_trackRout, par_trackPt;
    
  int   par_kSigPed, par_AdcThres;
  float par_maxADC, par_clustET, par_clustFrac24, par_nearTotEtFrac;
  float par_nearDeltaR, par_awayDeltaPhi, par_smallNearDeltaR;
  float par_delR3D, par_highET,  par_ptBalance;
  float par_leptonEta; // 
  int   par_inpRunNo; // to control run dependent cuts, ugly solution JB

  float par_countTrPt,par_countTowEt; 
  int   par_useEtow;                    
  float par_etowScale;
  float par_btowScale;

  float par_mcJetNeutScale;
  float par_mcJetChrgScale;
  
  char* gains_file;
  int use_gains_file;
  float gains_BTOW[4801];

  TH1F* hReweight; char* nameReweight;
  
 public: // to overwrite default params from .C macro
  void useEtow(int x){ par_useEtow=x; }
  void setVertexCuts(float zm, int npv) {
    par_vertexZ=zm; par_minPileupVert=npv; }
  void setEleTrackCuts(int nfp, int hfr, float rin, float rout, float mpt) {
    par_nFitPts=nfp;  par_nHitFrac=hfr; 
    par_trackRin=rin;  par_trackRout=rout; par_trackPt=mpt;}
  void setWbosonCuts(float a, float fr2,  float bal) {
    par_highET=a; par_nearTotEtFrac=fr2;  par_ptBalance=bal;}
  void setEmcCuts(int ksp , float madc, float clet, float fr1, float dr){
    par_kSigPed=ksp; par_maxADC=madc; par_clustET=clet; 
    par_clustFrac24=fr1;}
  void setEtowScale(float x){ par_etowScale=x; }
  void setBtowScale(float x){ par_btowScale=x; }
  void setL0AdcThresh(int x){ par_l0emulAdcThresh=x; }
  void setL2ClusterThresh(float x){ par_l2emulClusterThresh=x; }
  void setL2SeedThresh(float x){ par_l2emulSeedThresh=x; }
  void setJetTreeBranch(TString jetTreeBranch, TString jetTreeBranch_noEEMC){ mJetTreeBranch = jetTreeBranch; mJetTreeBranch_noEEMC = jetTreeBranch_noEEMC; }
  void setJetNeutScaleMC(float x){ par_mcJetNeutScale=x; }
  void setJetChrgScaleMC(float x){ par_mcJetChrgScale=x; }

  void setGainsFile(char* x) {gains_file=x; use_gains_file=1;}
  void setNameReweight(char* x) {nameReweight=x;}

 private:   

  //.... not used in the algo
  int par_DsmThres; 
  int par_maxDisplEve;

  StBemcTables* mBarrelTables; // used to acBcess EMC status and pedestal info
  StEmcGeom  *mBtowGeom, * mBSmdGeom[mxBSmd];
  int mapBtowIJ2ID[mxBTetaBin*mxBTphiBin];// vs. (iEta, iPhi)
  TVector3 positionBtow[mxBtow]; // vs. tower ID
  TVector3 positionBsmd[mxBSmd][mxBStrips]; // vs. strip ID
 
  StEEmcDb        *mDbE; // access to EEMC database        
  EEmcGeomSimple  *geomE;// access to EEMC geometry        
  TVector3 positionEtow[mxEtowSec*mxEtowSub][mxEtowEta];  
  
  int   accessTrig();
  int   accessVertex();
  int   accessTracks();
  int   accessBTOW();
  void  accessBSMD();
  int   accessETOW();   

  bool  passes_L0();
  bool  passes_L2();
  void  find_W_boson();
  void  tag_Z_boson();
  int   extendTrack2Barrel();
  int   matchTrack2Cluster();
  void  findNearJet();
  void  findAwayJet();
  void  findPtBalance();
  void  hadronicRecoil();

  // jets
  StJet* getJet(int i){return (StJet*)mJets->At(i);}
  TClonesArray* getJets(TString branchName);

  TRandom3 *mRand;

  // tools
  float sumTpcCone( int vertID, TVector3 refAxis, int flag, int &nTrCnt);
  float sumBtowCone( float zVert,  TVector3 refAxis, int flag,int &nTow);
  WeveCluster maxBtow2x2(int iEta, int iPhi, float zVert);
  WeveCluster sumBtowPatch(int iEta, int iPhi, int Leta,int  Lphi,float zVert);
  float sumEtowCone(float zVert, TVector3 refAxis,int flag,int &nTow);
  void patchToEtaPhi(int patch, int*eta, int*phi);

  // histograms
  TObjArray *HList;
  enum {mxHA=350}; TH1 * hA[mxHA];
    
  void initHistos();
  void initGeom();
  int L2algoEtaPhi2IJ(float etaF,float phiF,int &kEta, int &kPhi);
  
 public: 
  St2009WMaker(const char *name="2009WalgoB5.2");
  virtual  ~St2009WMaker(){};
  virtual Int_t Init();
  virtual Int_t  Make();

  virtual Int_t InitRun  (int runumber);
  virtual void Clear(const Option_t* = "");
  virtual Int_t FinishRun(int runumber);

  void setTrigID(int bht3, int l2w, int runNo) { par_bht3TrgID=bht3; par_l2wTrgID=l2w; par_inpRunNo=runNo;}
  void setHList(TObjArray * x){HList=x;}
  void setMC(int x){isMC=x;}
  void setMaxDisplayEve(int n) { par_maxDisplEve=n;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009WMaker.h,v 1.17 2014/08/06 11:43:41 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(St2009WMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009WMaker.h,v $
// Revision 1.17  2014/08/06 11:43:41  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.16  2011/09/14 14:23:20  stevens4
// update used for cross section PRD paper
//
// Revision 1.15  2010/12/02 18:31:43  rcorliss
// updated lumi code to match the starnote version
//
// Revision 1.14  2010/05/01 01:31:44  balewski
// added W->JJ code & JES calibration
//
// Revision 1.13  2010/04/27 16:53:45  stevens4
// add code to remove events tagged as Zs from W candidates
//
// Revision 1.12  2010/04/16 14:35:32  balewski
// fix borken header
//
// Revision 1.11  2010/03/23 15:33:55  seelej
// Edit to files to allow the use of a text file for the gains instead of using the DB.
//
// Revision 1.10  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.9  2010/02/18 22:34:50  stevens4
// add tpc effic study and allow energy scaling for data and MC
//
// Revision 1.8  2010/01/27 22:12:24  balewski
// spin code matched to x-section code
//
// Revision 1.7  2010/01/23 02:35:38  stevens4
// add ability to scale jet et and use real btow peds for rcf mc
//
// Revision 1.6  2010/01/21 00:15:25  balewski
// added sector & run  dependent TPC cuts on Rin, Rout
//
// Revision 1.5  2010/01/18 03:26:15  balewski
// expanded TPC track filtering, not finished
//
// Revision 1.4  2010/01/09 00:07:16  stevens4
// add jet finder
//
// Revision 1.3  2010/01/06 19:16:47  stevens4
// track cuts now on primary component, cleanup
//
// Revision 1.2  2009/12/07 20:37:56  rcorliss
// Start
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
