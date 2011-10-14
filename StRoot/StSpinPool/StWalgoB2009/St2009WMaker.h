// $Id: St2009WMaker.h,v 1.1 2009/11/23 23:00:18 balewski Exp $

#ifndef STAR_St2009WMaker
#define STAR_St2009WMaker

/*!
 *                                                                     
 * \class  St2009WMaker
 * \author Jan Balewski, MIT; 
 * \author Endcap: Justin Stevens, IUCF
 * \date   August 2009
 * \brief  muDst based extraction of W-signal from pp500 data from 2009
 *
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "Wevent2009.h"

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

class St2009WMaker : public StMaker {
 friend class WeventDisplay;
 friend class St2009pubWanaMaker;
 friend class St2009pubJSMaker;
 friend class St2009pubSpinMaker;
 friend class St2009pubMcMaker;
 friend class St2009WlumiMaker;
 private:
  StMuDstMaker* mMuDstMaker;
  Wevent2009 wEve;
  WeventDisplay *wDisaply;
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
  float par_nearDeltaR, par_awayDeltaPhi, par_awayDeltaR;
  float par_delR3D, par_highET, par_awayTotET;
 
  float par_countTrPt,par_countTowEt; 
  int par_useEtow;                    
  float par_mcEtowScale;
  float par_mcBtowScale;

 public: // to overwrite default params from .C macro
  void useEtow(int x){ par_useEtow=x; }
  void setVertexCuts(float zm, int npv) {
    par_vertexZ=zm; par_minPileupVert=npv; }
  void setEleTrackCuts(int nfp, int hfr, float rin, float rout, float mpt) {
    par_nFitPts=nfp;  par_nHitFrac=hfr; 
    par_trackRin=rin;  par_trackRout=rout; par_trackPt=mpt;}
  void setWbosonCuts(float a, float fr2, float b) {
    par_highET=a; par_nearTotEtFrac=fr2;  par_awayTotET=b;}
  void setEmcCuts(int ksp , float madc, float clet, float fr1, float dr){
    par_kSigPed=ksp; par_maxADC=madc; par_clustET=clet; 
    par_clustFrac24=fr1;}
  void setEtowScaleMC(float x){ par_mcEtowScale=x; }
  void setBtowScaleMC(float x){ par_mcBtowScale=x; }
  void setL0AdcThresh(int x){ par_l0emulAdcThresh=x; }
  void setL2ClusterThresh(float x){ par_l2emulClusterThresh=x; }
  void setL2SeedThresh(float x){ par_l2emulSeedThresh=x; }
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
  //  float etowR[mxEtowEta];                                

  int   accessTrig();
  int   accessVertex();
  int   accessTracks();
  int   accessBTOW();
  void  accessBSMD();
  int   accessETOW();   

  bool  passes_L0();
  bool  passes_L2();
  void  find_W_boson();
  int   extendTrack2Barrel();
  int   matchTrack2Cluster();
  void  findNearJet();
  void  findAwayJet();
  void  findAwayCone();
  void  hadronicRecoil();

  // tools
  float sumTpcCone( int vertID, TVector3 refAxis, int flag, int &nTrCnt, TVector3 &maxTrVec);
  float sumBtowCone( float zVert,  TVector3 refAxis, int flag,int &nTow, TVector3 &maxTowVec);
  WeveCluster maxBtow2x2(int iEta, int iPhi, float zVert);
  WeveCluster sumBtowPatch(int iEta, int iPhi, int Leta,int  Lphi,float zVert);
  float sumEtowCone(float zVert, TVector3 refAxis,int flag,int &nTow, TVector3  &maxTowVec);

  // histograms
  TObjArray *HList;
  enum {mxHA=128}; TH1 * hA[mxHA];
    
  void initHistos();
  void initGeom();
  int L2algoEtaPhi2IJ(float etaF,float phiF,int &kEta, int &kPhi);
  
 public: 
  St2009WMaker(const char *name="2009WalgoB4.3s");
  virtual  ~St2009WMaker(){};
  virtual Int_t Init();
  virtual Int_t  Make();

  virtual Int_t InitRun  (int runumber);
  virtual void Clear(const Option_t* = "");
  virtual Int_t FinishRun(int runumber);

  void setTrigID(int bht3, int l2w) { par_bht3TrgID=bht3; par_l2wTrgID=l2w;}
  void setHList(TObjArray * x){HList=x;}
  void setMC(int x){isMC=x;}
  void setMaxDisplayEve(int n) { par_maxDisplEve=n;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: St2009WMaker.h,v 1.1 2009/11/23 23:00:18 balewski Exp $ built "__DATE__" "__TIME__ ; 
    return cvs;
  }

  ClassDef(St2009WMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: St2009WMaker.h,v $
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
