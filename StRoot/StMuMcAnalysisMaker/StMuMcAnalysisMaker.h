// $Id: StMuMcAnalysisMaker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $
/*!
 * \class  StMuMcAnalysisMaker
 * \author fisyak
 * \date   2016/02/02
 * \brief  virtual base class for analysis MuDst information with respect Mc
 */                                                                      
#ifndef STAR_StMuMcAnalysisMaker
#define STAR_StMuMcAnalysisMaker
//#define __DEVT__
#include <assert.h>
#include <map>
#include <utility>
#include "Riostream.h"
#include "Rtypes.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TH2.h"
#include "TH3.h"
#include "TF1.h"
#include "TProfile.h"
#include "TProfile3D.h"
#include "TTree.h"
#include "TChain.h"
#include "TCanvas.h"
#include "TLegend.h"
#include "TClassTable.h"
#include "TFile.h"
#include "TChain.h"
#include "TString.h"
#include "SystemOfUnits.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryTrackCovariance.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StarRoot/TPolynomial.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFParticle.h"
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#include "StBichsel/Bichsel.h"
#ifndef StMaker_H
#include "StMaker.h"
#endif
enum TrackType {
  kGlobal = 0, kPrimary, kTotalT  // switch between global and primary tracks
};
enum TrackMatchType {
  kNotDefined = -1, kMcTk = 0, kMcTpcTk, kRecoTk, kCloneTk, kGhostTk, kLostTk, kMcHftTk, kRecoHftTk, kCloneHftTk, kGhostHftTk, kLostHftTk, kTotalMatchType // match type extended
};
enum EParticleType {
  kallP = 0, kPion, kPartypeT                                             // switch between All and pion
};
enum EChargeType {
  kPositive = 0, kNegative, kTotalSigns                                 // switch between charges
};
enum EPlotType {
  kTotalQA = 16, kTotalQAll,                                             // no. of plots for Global and Primary tracks
#ifndef __DEVT__
  noFit =  46,
#else
  noFit = 100,
#endif
  NHYPS = 18, NHypTypes = NHYPS/2, 
  kVariables = 2                                                         // x = 0 vs No. fit and No. bad points, x = 1 vs Eta and pT 
};

class StMuMcAnalysisMaker : public StMaker {
 private:
  struct Var_t {
    Double_t ChiSqXY;
    Double_t ChiSqZ;
    Double_t dDcaXY; 
    Double_t dDcaZ;  
    Double_t dPsi;   
    Double_t dPti;  
    Double_t dPtiR; 
    Double_t dTanL;  
    Double_t deta;  
    Double_t pDcaXY; 
    Double_t pDcaZ;  
    Double_t pPsi;   
    Double_t pPti;  
    Double_t pPtiR; 
    Double_t pTanL;  
    Double_t peta;  
    Double_t Phi; // degree
  };
  struct PlotName_t {
    TrackMatchType    k;
    const Char_t *Name;
    const Char_t *Title;
  };
  struct VarName_t {
    const Char_t *Name;
    const Char_t *Title;
    Int_t nx;
    Double_t xmin, xmax;
    Int_t ny;
    Double_t ymin, ymax;
    Int_t nz;
    Double_t zmin, zmax;
    Double_t  min,  max; // min and max for plots
    Int_t    GlobalOnly; // = 1: only global, -1: only primary, 0: both
  };
  
  Char_t                mBeg[1];        //!
  StMuDst              *muDst;          //!
  Char_t                mEnd[1];        //!
 protected:
 public: 
  StMuMcAnalysisMaker(const char *name="MuMcAnalysis") : StMaker(name) {memset(mBeg,0,mEnd-mBeg+1);}
  virtual       ~StMuMcAnalysisMaker() {}
  virtual Int_t  Init();
  void           BookTrackPlots();
  void           BookVertexPlots();
  virtual Int_t  Make();
  void           FillTrackPlots();
  void           FillQAGl(TrackMatchType type,const StMuTrack *gTrack = 0, const StMuMcTrack *mcTrack = 0, const StDcaGeometry *dcaG = 0, const StMuMcVertex *mcVertex = 0);
  void           FillQAPr(TrackMatchType type,const StMuTrack *pTrack = 0, const StMuMcTrack *mcTrack = 0, const StMuPrimaryTrackCovariance *cov = 0); 
  void           FillQAPr(TrackMatchType type,const StMuTrack *pTrack, const StMuMcTrack *mcTrack, const KFParticle *particle);
  Bool_t         Accept(const StMuTrack *gTrack = 0);
  Bool_t         AcceptGhost(const StMuTrack *gTrack = 0);
  Bool_t         AcceptVX(const StMuPrimaryVertex *Vtx = 0);
  TrackMatchType TrackType(const StMuMcTrack *mcTrack, multimap<Int_t,Int_t> &Mc2RcTracks, Bool_t CheckHft = kFALSE);
  void           ForceAnimate(unsigned int times=0, int msecDelay=0); 
  void           FillVertexPlots();
  virtual Int_t  Finish();  
  Bool_t         Check();
  void           DrawQA(Int_t gp = -1, Int_t pp = -1, Int_t xx = -1, Int_t ii = -1);
  void           DrawEff(Double_t ymax=1.0, Double_t pTmin = -1, Int_t animate=0);
  void           DrawdEdx();
  void           DrawPng(TCanvas *c);
  void           DrawH3s(TH3F *h3s[2], Int_t animate = 0, Double_t min = 1e9, Double_t max = -1e9, Int_t np = 2);
  void           MinMax(TH1 *h, Double_t &min, Double_t &max, Double_t amax = 1000);
  TString        DirPath(const TH1* hist);
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StMuMcAnalysisMaker.h,v 1.16 2014/08/06 11:43:53 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StMuMcAnalysisMaker,0)   //
};
#endif
// $Log: StMuMcAnalysisMaker.h,v $
