///
#ifndef _ST_FGT_GEN_AGV_EFF_MAKER_
#define _ST_FGT_GEN_AGV_EFF_MAKER_

#include "StMaker.h"
//#include "StFgtQaMaker.h"
#include <TH2D.h>
#include <TROOT.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>
#include <TF1.h>
#include "StFgtGeneralBase.h"
//#include "StRoot/StEvent/StFgtCollection.h"

class StFgtHit;


struct AVPoint
{
  Double_t x;
  Double_t y;
  Double_t z;
  Double_t r;
  Double_t phi;
  Double_t rCharge;
  Double_t phiCharge;
  Double_t rSize;
  Double_t phiSize;
  Int_t rMaxAdc;
  Int_t phiMaxAdc;
  Int_t rMaxAdcInt;
  Int_t phiMaxAdcInt;
  Int_t geoIdR;
  Int_t geoIdPhi;
  Int_t dID;
  Int_t quadID;
  StFgtHit* fgtHitR;
  StFgtHit* fgtHitPhi;

  AVPoint(){};
  AVPoint(Double_t mx, Double_t my, Double_t mz, Double_t mr, Double_t mPhi, Int_t mdID,Int_t mQuad,  Double_t mRCharge, Double_t mPhiCharge, Double_t mRSize, Double_t mPhiSize)
  {
    geoIdR=-1;
    geoIdPhi=-1;
    x=mx;
    y=my;
    z=mz;
    r=mr;
    phi=mPhi;
    dID=mdID;
    quadID=mQuad;
    rCharge=mRCharge;
    phiCharge=mPhiCharge;
    rSize=mRSize;
    phiSize=mPhiSize;
    rMaxAdc=-1;
    phiMaxAdc=-1;
    rMaxAdcInt=-1;
    phiMaxAdcInt=-1;


  }
};

struct AVTrack
{
  Double_t mx;
  Double_t my;
  Double_t ax;
  Double_t ay;
  Double_t ipZEv;
  Double_t ipZ;
  Int_t vtxRank;
  Double_t chi2;
  Double_t dca;
  Double_t trkZ;
  vector<AVPoint>* points;
  AVTrack(){};
  AVTrack(Double_t m_mx, Double_t m_my, Double_t m_ax, Double_t m_ay, Double_t m_ipZ=0.0,Double_t m_chi2=0.0)
  {
    mx=m_mx;
    my=m_my;
    ax=m_ax;
    ay=m_ay;
    ipZEv=m_ipZ;
    chi2=m_chi2;
    dca=-9999;
    trkZ=-9999;
    ipZ=-9999;
  }
};

class StFgtCollection;

class StFgtGenAVEMaker : public StFgtGeneralBase {
 public:
  StFgtGenAVEMaker(const Char_t* name="FgtGenAVEMaker");
  pair<Double_t,Double_t> findCluChargeSize(Int_t iD,Char_t layer, Double_t ordinate);
  virtual ~StFgtGenAVEMaker();
   Int_t Init();
   Int_t Make();
   Int_t Finish();
   void setUseChargeMatch(Bool_t use=true);
   //   Bool_t checkPulse(StFgtHit* pClus);
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtGenAVEMaker.h,v 1.22 2014/08/06 11:43:10 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
 protected:
   vector<TH2D*> v_hClusP;
   vector<TH2D*> v_hClusR;
   Bool_t useChargeMatch;
   Int_t printCounter;
   Int_t fitCounter;
   ofstream* outTxtFile;
   ofstream* cluNotFoundTxt;
   Short_t getQuadFromCoo(Double_t x, Double_t y);
   pair<Double_t,Double_t> getChargeRatio(Float_t r, Float_t phi, Int_t iD, Int_t iq);
   Bool_t printArea(Float_t r, Float_t phi, Int_t iD, Int_t iq);
   Bool_t printArea1D(Int_t iD, Int_t iq,Int_t centerGeoId);
   Bool_t getTrack(vector<AVPoint>& points, Double_t ipZ);
   pair<double,double> getDca(  vector<AVTrack>::iterator it);
   vector<AVTrack> m_tracks;
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;
   void doNormalize(TH2D** hEff, TH2D** hNonEff);
   void saveSigs(Double_t* sigR, Double_t* sigP, Double_t r, Double_t phi,Int_t maxR, Int_t maxPhi, Int_t discId, Int_t quad);
   Double_t getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd);
   Double_t findClosestPoint(float mx, float bx, float my, float by, double xE, double yE, Int_t iD);
   Bool_t isSomewhatEff(Float_t r, Float_t phi, Int_t iD, Int_t iq);
   void fillStripHistos(Float_t r, Float_t phi, Int_t iD, Int_t iq);
   Double_t findClosestStrip(Char_t layer, double ord, Int_t iD, Int_t iQ);
   // for knowing what & how to plot
   Bool_t fitTheStrip(generalStrip* pStrip, generalStrip* pStripOtherLayer, float* amp, float* t0, float* chi2Ndf, int iD, int iq, int apvBin, Char_t layer);

   // threshold, in units of # sigma above average
   Float_t mPedThres;
   //   Double_t getRPhiRatio(StSPtrVecFgtHitConstIterator hitIterBegin, StSPtrVecFgtHitConstIterator hitIterEnd);
   //   Double_t getRPhiRatio();

   TF1* mPulseShapePtr;
   TH1F* mHistPtr;
   TH1F* mHistPtr2;
   TCanvas* mCanvas;
   TH2D** radioPlotsEff;
   TH2D** radioPlotsNonEff;

   TH2D** radioPlotsEffR;
   TH2D** radioPlotsNonEffR;

   TH2D** radioPlotsEffPhi;
   TH2D** radioPlotsNonEffPhi;

   TH2D** radioPlotsEffLoose;
   TH2D** radioPlotsNonEffLoose;


   TH2D* chargeCorrSum3;
   //   TH2D* chargeCorrCluster;
      TH2D* chargeCorrMaxStrip;
   TH2D* chargeCorrMaxAdc;


   TH1F** firstTbSigCloseClusterR;
   TH1F** firstTbSigCloseClusterP;
   TH1F** firstTbSigTrackClusterR;
   TH1F** firstTbSigTrackClusterP;


   TH1I** maxTbCloseClusterR;
   TH1I** maxTbCloseClusterP;

   TH1I** maxTbTrackClusterR;
   TH1I** maxTbTrackClusterP;

   TH1I** maxAdcTrackClusterR;
   TH1I** maxAdcCloseClusterR;

   TH1I** maxSigTrackClusterR;
   TH1I** maxSigCloseClusterR;


   TH1I** numClustersR;
   TH1I** numClustersPhi;

   TH1I** numTrackHits;
   TH1I* numTracks;


   TH1I** numFSigTrackClusterR;
   TH1I** numFSigCloseClusterR;

   TH1I** numFirstHighTrackClusterR;
   TH1I** numFirstHighCloseClusterR;

   TH1I** maxAdcTrackClusterP;
   TH1I** maxAdcCloseClusterP;

   TH1F** maxSigTrackClusterP;
   TH1F** maxSigCloseClusterP;

   TH1I** numFSigTrackClusterP;
   TH1I** numFSigCloseClusterP;

   TH1I** numFirstHighTrackClusterP;
   TH1I** numFirstHighCloseClusterP;

   TH1F** secondToLastRatioCloseClusterP;
   TH1F** secondToLastRatioCloseClusterR;

   TH1F** secondToLastRatioTrackClusterP;
   TH1F** secondToLastRatioTrackClusterR;

   TH1F** APVfitChi2P;
   TH1F** APVfitChi2R;

   TH1F** APVfitAmpP;
   TH1F** APVfitAmpR;

   TH1F** APVfitT0P;
   TH1F** APVfitT0R;

   TH1F** APVfirstTbSigCloseClusterP;
   TH1F** APVfirstTbSigCloseClusterR;
   TH1I** APVmaxAdcCloseClusterP;
   TH1I** APVmaxAdcCloseClusterR;
   TH1I** APVmaxTbCloseClusterP;
   TH1I** APVmaxTbCloseClusterR;
   TH1I** APVnumFSigCloseClusterP;
   TH1I** APVnumFSigCloseClusterR;
   TH1I** APVnumFirstHighCloseClusterP;
   TH1I** APVnumFirstHighCloseClusterR;
   TH1F** APVmaxSigCloseClusterP;
   TH1F** APVmaxSigCloseClusterR;
   TH1F** APVsecondToLastRatioCloseClusterP;
   TH1F** APVsecondToLastRatioCloseClusterR;

   TH1F* exPulseMaxAdcNormR;
   TH1F* exPulseSigR;
   TH1F* exPulseMaxAdcNormP;
   TH1F* exPulseSigP;

   TH1F* exPulseMaxAdcNormTrackR;
   TH1F* exPulseSigTrackR;
   TH1F* exPulseMaxAdcNormTrackP;
   TH1F* exPulseSigTrackP;

   //joe's histograms added
   TH1I** clusterGeoId;
   TH1I** clustersR;
   TH1I** clustersP;
   TH1I*disk1QuadA[22];
   
   int pulseCounterP;
   int pulseCounterR;

   int pulseCounterTP;
   int pulseCounterTR;


   TH1D** rPhiRatioPlots;
   TH1D** rEff;
   TH1D* hChargeAsym;
   TH1D* hChargeRatio;
   TH1D** rNonEff;
   TH2D* chargeRatioInEffDisk;
   TH2D* chargeAsymInEffDisk;
   TH2D* chargeCorrInEffDisk;
   TH2D* tpcFgtZVertexCorr;
   TH2D* tpcFgtZVertexCorr2;
   TH2D* tpcFgtZVertexCorr3;

   TH2D** chargeCorr;
   TH1D** clusterSizeR;
   TH1D** clusterSizeP;
   TH1D** h_clusterSizeR;
   TH1D** h_clusterSizePhi;
   TH1D** h_clusterChargeR;
   TH1D** h_clusterChargePhi;

   TH2D* hIp;
   TH1D* hIpZ;
   TH1D* hIpDca;


   TH1D* hTrkZ;
   TH1D* hChi2;
   TH1D* hBx;
   TH1D* hBy;
   TH1D* hResidua;
   TH2D* hResiduaX;
   TH2D* hResiduaY;
   TH2D* hResiduaR;
   TH2D* hResiduaP;


   TH1D* hMx;
   TH1D* hMy;

   TFile* pulsePictureFile;
   TFile* myRootFile;
   int runningEvtNr;
   int hitCounter;
   int hitCounterR;
   //THD2** 


 private:   
      ClassDef(StFgtGenAVEMaker,1);

};
inline void StFgtGenAVEMaker::setUseChargeMatch(Bool_t use){useChargeMatch=use;};
#endif

