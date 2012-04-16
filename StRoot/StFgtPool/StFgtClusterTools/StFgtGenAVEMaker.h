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
#include "StFgtGeneralBase.h"
//#include "StRoot/StEvent/StFgtCollection.h"


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

  Int_t dID;
  Int_t quadID;
  AVPoint(){};
  AVPoint(Double_t mx, Double_t my, Double_t mz, Double_t mr, Double_t mPhi, Int_t mdID,Int_t mQuad,  Double_t mRCharge, Double_t mPhiCharge, Double_t mRSize, Double_t mPhiSize)
  {
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
  }
};

struct AVTrack
{
  Double_t mx;
  Double_t my;
  Double_t ax;
  Double_t ay;
  Double_t ipZ;
  Double_t chi2;
  AVTrack(){};
  AVTrack(Double_t m_mx, Double_t m_my, Double_t m_ax, Double_t m_ay, Double_t m_ipZ=0.0,Double_t m_chi2=0.0)
  {
    mx=m_mx;
    my=m_my;
    ax=m_ax;
    ay=m_ay;
    ipZ=m_ipZ;
    chi2=m_chi2;
  }
};

class StFgtCollection;

class StFgtGenAVEMaker : public StFgtGeneralBase {
 public:
  StFgtGenAVEMaker(const Char_t* name="FgtGenAVEMaker");
  virtual ~StFgtGenAVEMaker();
   Int_t Init();
   Int_t Make();
   Int_t Finish();
   //   Bool_t checkPulse(StFgtHit* pClus);
   virtual const char *GetCVS() const
   {static const char cvs[]="Tag $Name:  $ $Id: StFgtGenAVEMaker.h,v 1.1 2012/04/16 19:37:37 avossen Exp $ built "__DATE__" "__TIME__ ; return cvs;}
 protected:
   Bool_t getTrack(vector<AVPoint>& points, Double_t ipZ);
   vector<AVTrack> m_tracks;
   // for accessing the data
   StFgtCollection *mFgtCollectionPtr;
   Double_t getRPhiRatio(vector<generalCluster>::iterator hitIterBegin, vector<generalCluster>::iterator hitIterEnd);

   // for knowing what & how to plot


   // threshold, in units of # sigma above average
   Float_t mPedThres;
   //   Double_t getRPhiRatio(StSPtrVecFgtHitConstIterator hitIterBegin, StSPtrVecFgtHitConstIterator hitIterEnd);
   //   Double_t getRPhiRatio();
   TH2D** radioPlotsEff;
   TH2D** radioPlotsNonEff;
   TH1D** rPhiRatioPlots;
   TH1D** rEff;
   TH1D** rNonEff;
   TH2D* tpcFgtZVertexCorr;

   TH2D** chargeCorr;
   TH1D** h_clusterSizeR;
   TH1D** h_clusterSizePhi;
   TH1D** h_clusterChargeR;
   TH1D** h_clusterChargePhi;

   TH2D* hIp;
   TH1D* hIpZAtX0;
   TH1D* hIpZAtY0;

   TH1D* hTrkZ;
   TH1D* hChi2;

   TFile* myRootFile;
   int runningEvtNr;
   int hitCounter;
   int hitCounterR;


   //THD2** 


 private:   
      ClassDef(StFgtGenAVEMaker,1);

};

#endif

