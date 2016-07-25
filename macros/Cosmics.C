/* to Run: root.exe -q -b lMuDst.C Cosmics.C+

   to get momentum Resolution:
//   CosmicT->Draw("(K.mPti+L.mPti)*TMath::Sqrt(2.)/TMath::Abs(K.mPti-L.mPti):-TMath::Log10(TMath::Abs(K.mPti-L.mPti)/2.)>>DpT(15,-1,2,100,-0.2,0.2)","chi2<200","colz");
   TLegend *leg = new TLegend(0.4,0.4,0.7,0.7);
   TF1 *f = new TF1("f","TMath::Sqrt([0]*[0]/TMath::Power(10.,2*x)+[1]*[1]*TMath::Power(10.,2*x))",-1,2);
   Int_t color = 2; 
   CosmicT->Draw("(K.mPti+L.mPti)*TMath::Sqrt(2.)/TMath::Abs(K.mPti-L.mPti):-TMath::Log10(TMath::Abs(K.mPti-L.mPti)/2.)>>DpT(30,-1,2,100,-0.2,0.2)","noFitpK>30&&noFitpL>30&&chi2<200","colz");
   TH2 *DpT = (TH2 *) gDirectory->Get("DpT");
   DpT->FitSlicesY();
   TH1 *DpT_2 = (TH1 *) gDirectory->Get("DpT_2");
   DpT_2->SetTitle("Relative p_{T} resolution from cosmics muons");
   DpT_2->SetXTitle("Log_{10}(p_{TAv})");
   DpT_2->SetYTitle("#sigma (#Delta p_{T}/p_{TAv})/ #sqrt{2}");
   DpT_2->SetMarkerStyle(20);
   DpT_2->SetMarkerColor(color);
   DpT_2->SetLineColor(color);
   DpT_2->SetStats(0);
   f->SetParameters(0,1);
   f->SetLineColor(color);
   DpT_2->Fit(f);
   if (color == 1) leg->AddEntry(DpT_2,"without correction (No. fit points > 30)");
   if (color == 2) leg->AddEntry(DpT_2,"with correction (No. fit points > 30)");
   
   Pulls:
   TLegend *legP = new TLegend(0.4,0.4,0.7,0.7);
   Int_t color = 2; 
    CosmicT->Draw("(K.mPti+L.mPti)/TMath::Sqrt(K.mPtiPti+L.mPtiPti):-TMath::Log10(TMath::Abs(K.mPti-L.mPti)/2.)>>PullDpT(30,-1,2,100,-5,5)","noFitpK>30&&noFitpL>30&&chi2<200","colz");
   TH2 *PullDpT = (TH2 *) gDirectory->Get("PullDpT");
   PullDpT->FitSlicesY();
   TH1 *PullDpT_2 = (TH1 *) gDirectory->Get("PullDpT_2");
   PullDpT_2->SetTitle("#sigma for p_{T} pull from cosmics muons");
   PullDpT_2->SetXTitle("Log_{10}(p_{TAv})");
   PullDpT_2->SetYTitle("#sigma (#Delta p_{T}/#sigma p_{T})");
   PullDpT_2->SetMarkerStyle(20);
   PullDpT_2->SetMarkerColor(color);
   PullDpT_2->SetLineColor(color);
   PullDpT_2->SetStats(0);
   if (color == 1) legP->AddEntry(PullDpT_2,"without correction (No. fit points > 30)");
   if (color == 2) legP->AddEntry(PullDpT_2,"with correction (No. fit points > 30)");
   
   dEdx:
   CosmicT->Draw("zK:-TMath::Log10(TMath::Abs(K.mPti-L.mPti)/2.)>>dEdx(30,-1,2,100,-0.25,0.25)","noFitpK>15&&noFitpL>15&&zK>-1","colz")
   CosmicT->Draw("zK:TMath::Log10(bgK)>>dEdxbg(50,0,5.,100,-.5,0.5)","noFitpK>15&&noFitpL>15&&zK>-1&&chi2<200","colz")
*/
//#define DEBUG
//#define __PrimaryVertices__
//#define __PrimaryTracks__
#define __Y2015__
#if !defined(__CINT__) || defined(__MAKECINT__)
#include <assert.h>
#include "Riostream.h"
#include <stdio.h>
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TMinuit.h"
#include "TSpectrum.h"
#include "TString.h"
#include "TLine.h"
#include "TText.h"
#include "TROOT.h"
#include "TList.h"
#include "TPolyMarker.h"
#include "StBichsel/Bichsel.h"
#include "TDirIter.h"
#include "TTree.h"
#include "TTreeIter.h"
#include "TRandom.h"
#include "TFractionFitter.h"
#include "TLegend.h"
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "StDcaGeometry.h"
#include "TROOT.h"
#include "TSystem.h"
//#include "StDedxPidTraits.h"
#include "StEnumerations.h"
#else
class TSystem;
class TMath;
class TH1;
class TH2;
class TH3;
class TProfile;
class TStyle;
class TF1;
class TTree;
class TChain;
class TFile;
class TNtuple;
class TCanvas;
class TMinuit;
class TSpectrum;
class TString;
class TLine;
class TText;
class TROOT;
class TList;
class TPolyMarker;
class Bichsel;
class TDirIter;
class TTreeIter;
#endif
Bichsel *m_Bichsel = 0;
#include "Names.h"
#if 0
#ifndef StTrackPidTraits_hh
#define StTrackPidTraits_hh
class StDedxPidTraits : public TObject {
public:
  StDedxPidTraits(StDetectorId det=kUnknownId, short meth=0,
		  unsigned short n=0, float dedx=0, float sig=0) : 
    mDetectorId(det), mNumberOfPoints(n), mMethod(meth), mDedx(dedx), mSigma(sig) {}
protected:
  Short_t  mDetectorId;
  UShort_t mNumberOfPoints;
  Short_t  mMethod;
  Float_t  mDedx;
  Float_t  mSigma;
  ClassDef(StDedxPidTraits,9)
};
#endif
#else
#include "StTrackPidTraits.h"
#include "StDedxPidTraits.h"
#endif
class CosmicTracks : public TNamed {
public:
  CosmicTracks() : TNamed("","") {}
  Int_t noFitpK;
  Int_t noFitpL;
  Double_t chi2;
  Int_t   sectorK;
  Int_t   sectorL;
  StDcaGeometry K;
  StDcaGeometry L;
  StDedxPidTraits K70;
  StDedxPidTraits L70;
  StDedxPidTraits Kfit;
  StDedxPidTraits Lfit;
  Double_t pK, pL, bgK, bgL;
  Double_t bichK, bichL;
  Double_t zK, zL;
  Double_t zFitK, zFitL;
  ClassDef(CosmicTracks,3)
};
ClassImp(StDedxPidTraits);
ClassImp(CosmicTracks);
//______________________________________________________________________
Int_t IndexH(const Char_t *name) {
  Int_t index = -1;
  TString Name(name);
  for (Int_t l = 0; l < KPidParticles; l++) {
    if (Name.BeginsWith(PidNames[l])) {index = l; break;}
  }
  return index;
}
//________________________________________________________________________________
Double_t difPsi(Double_t psi1, Double_t psi2) {
  Double_t psi = psi1 - psi2;
  if (psi >  TMath::PiOver2()) psi -= TMath::Pi();
  if (psi < -TMath::PiOver2()) psi += TMath::Pi(); 
  return psi;
}
//________________________________________________________________________________
Int_t sector(Float_t x, Float_t y, Float_t z) {
  Int_t phi = TMath::RadToDeg()*TMath::ATan2(y, x);
  if (phi <   0) phi += 360;
  if (phi > 360) phi -= 360;
  Int_t sec = -1;
  Int_t s = phi/30;
  if (z > 0) {
    sec =  15 - s; if (sec >  12) sec -= 12;
  } else {
    sec =   9 + s; if (sec <= 12) sec += 12;
  }
  return sec;
}
//________________________________________________________________________________
void Cosmics(const Char_t *files ="./*.MuDst.root",
	     const Char_t *Out = "Cosmics.root"
	    ){
  //  static const Double_t sigmaB[2] = {6.26273e-01, -5.80915e-01}; // Global Tracks, wrt Bichsel
  if (!m_Bichsel) {
    gSystem->Load("StBichsel"); 
    gSystem->Load("StarClassLibrary");  
    m_Bichsel = Bichsel::Instance();
  }
  TDirIter Dir(files);
  Char_t *file = 0;
  Int_t NFiles = 0;
  TTreeIter iter;
  while ((file = (Char_t *) Dir.NextFile())) {iter.AddFile(file); NFiles++;}
  cout << files << "\twith " << NFiles << " files" << endl; 
  // init of user variables
#define __GlobalTracks__
#define __CovGlobTrack__
#define __FirstLastPoint__
#define __dEdx__
  //#include "MuDstIter.h"
#include "MuDstIterMerged.h"
  // Book TTree
  TFile *fOut = new TFile(Out,"recreate");
  TTree *ftree = new TTree("CosmicT","Cosmic tree");
  ftree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  Int_t bufsize = 64000;
  Int_t split = 99;
  if (split)  bufsize /= 4;
  CosmicTracks tracks;
  CosmicTracks *ftrack = &tracks;
  TTree::SetBranchStyle(1); //new style by default
  ftree->Branch("CosmicTracksB", "CosmicTracks", &ftrack, bufsize, split);
   //         Now iterations
  StDcaGeometry tK, tL;
  Int_t netries = 0;
  while (iter.Next()) {
    /*
        Dca: imp   16.44 +/-   0.11,Z   45.00 +/-   0.09,psi    0.76 +/-   0.00, q/pT   -1.63 +/-   1.7%,TanL    0.373 +/-   0.001 L  139.912   0.000 NF   43 chi2    0.939 NP   45
        Dca: imp  -16.55 +/-   0.11,Z   45.07 +/-   0.09,psi   -2.38 +/-   0.00, q/pT    1.61 +/-   1.5%,TanL   -0.369 +/-   0.001 L  138.818   0.000 NF   45 chi2    0.986 NP   45
    */
    Int_t k = -1; 
    Int_t l = -1;
    Double_t Chi2Old = 1.e3;
    for (Int_t kg = 0; kg < NoGlobalTracks - 1; kg++) {
      if (GlobalTracks_mNHitsFitTpc[kg] < 15) continue;
      Int_t kgc = GlobalTracks_mIndex2Cov[kg];
      if (kgc < 0 || kgc > NoCovGlobTrack) continue;
      Double_t parK[6] = {CovGlobTrack_mImp[kgc], CovGlobTrack_mZ[kgc], CovGlobTrack_mPsi[kgc], CovGlobTrack_mPti[kgc], CovGlobTrack_mTan[kgc], 0};
      Double_t covK[15] = {
	CovGlobTrack_mImpImp[kgc], 
	CovGlobTrack_mZImp[kgc]  , CovGlobTrack_mZZ[kgc]    ,
	CovGlobTrack_mPsiImp[kgc], CovGlobTrack_mPsiZ[kgc]  , CovGlobTrack_mPsiPsi[kgc], 
	CovGlobTrack_mPtiImp[kgc], CovGlobTrack_mPtiZ[kgc]  , CovGlobTrack_mPtiPsi[kgc], CovGlobTrack_mPtiPti[kgc], 
	CovGlobTrack_mTanImp[kgc], CovGlobTrack_mTanZ[kgc]  , CovGlobTrack_mTanPsi[kgc], CovGlobTrack_mTanPti[kgc], CovGlobTrack_mTanTan[kgc]};
      TRVector ParK(5,parK);
      TRSymMatrix CovK(5,covK);
      tK.set(parK,covK);
      for (Int_t lg = kg+1; lg < NoGlobalTracks - 1; lg++) {
	Int_t lgc = GlobalTracks_mIndex2Cov[lg];
      if (GlobalTracks_mNHitsFitTpc[lg] < 15) continue;
	if (lgc < 0 || lgc > NoCovGlobTrack) continue;
	Double_t parL[6] = {CovGlobTrack_mImp[lgc], -CovGlobTrack_mZ[lgc], -CovGlobTrack_mPsi[lgc], CovGlobTrack_mPti[lgc], CovGlobTrack_mTan[lgc], 0};
	Double_t covL[15] = {
	  CovGlobTrack_mImpImp[lgc], 
	  CovGlobTrack_mZImp[lgc]  , CovGlobTrack_mZZ[lgc]    ,
	  CovGlobTrack_mPsiImp[lgc], CovGlobTrack_mPsiZ[lgc]  , CovGlobTrack_mPsiPsi[lgc], 
	  CovGlobTrack_mPtiImp[lgc], CovGlobTrack_mPtiZ[lgc]  , CovGlobTrack_mPtiPsi[lgc], CovGlobTrack_mPtiPti[lgc], 
	  CovGlobTrack_mTanImp[lgc], CovGlobTrack_mTanZ[lgc]  , CovGlobTrack_mTanPsi[lgc], CovGlobTrack_mTanPti[lgc], CovGlobTrack_mTanTan[lgc]};
	tL.set(parL,covL);
	TRVector ParL(5,parL);
	TRSymMatrix CovL(5,covL);
	TRVector Par(ParL);
	Par += ParK;
	if (Par[2] >  TMath::PiOver2()) Par[2] -= TMath::Pi();
	if (Par[2] < -TMath::PiOver2()) Par[2] += TMath::Pi(); 
	Par[4] = 0; // ignore difference in q/pT
	TRSymMatrix Cov(CovL);
	Cov += CovK;
	TRSymMatrix G(Cov,TRArray::kInverted);
        Double_t chi2 = G.Product(Par,TRArray::kATxSxA);
#if 0
	cout << "K\t" << kg << "\t" << tK << endl;
	cout << "L\t" << lg << "\t" << tL << "\tchi2 = " << chi2 << endl;
#endif
	if (chi2 < Chi2Old) {
	  Chi2Old = chi2;
	  k = kg;
	  l = lg;
	}
      }
    }
    if (k >= 0 && l >=0) {
      Int_t kg = k;
      Int_t lg = l;
      Int_t kgc = GlobalTracks_mIndex2Cov[kg];
      Double_t parK[6] = {CovGlobTrack_mImp[kgc], CovGlobTrack_mZ[kgc], CovGlobTrack_mPsi[kgc], CovGlobTrack_mPti[kgc], CovGlobTrack_mTan[kgc], 0};
      Double_t covK[15] = {
	CovGlobTrack_mImpImp[kgc], 
	CovGlobTrack_mZImp[kgc]  , CovGlobTrack_mZZ[kgc]    ,
	CovGlobTrack_mPsiImp[kgc], CovGlobTrack_mPsiZ[kgc]  , CovGlobTrack_mPsiPsi[kgc], 
	CovGlobTrack_mPtiImp[kgc], CovGlobTrack_mPtiZ[kgc]  , CovGlobTrack_mPtiPsi[kgc], CovGlobTrack_mPtiPti[kgc], 
	CovGlobTrack_mTanImp[kgc], CovGlobTrack_mTanZ[kgc]  , CovGlobTrack_mTanPsi[kgc], CovGlobTrack_mTanPti[kgc], CovGlobTrack_mTanTan[kgc]};
      tK.set(parK,covK);
      Int_t lgc = GlobalTracks_mIndex2Cov[lg];
      Double_t parL[6] = {CovGlobTrack_mImp[lgc], CovGlobTrack_mZ[lgc], CovGlobTrack_mPsi[lgc], CovGlobTrack_mPti[lgc], CovGlobTrack_mTan[lgc], 0};
      Double_t covL[15] = {
	CovGlobTrack_mImpImp[lgc], 
	CovGlobTrack_mZImp[lgc]  , CovGlobTrack_mZZ[lgc]    ,
	CovGlobTrack_mPsiImp[lgc], CovGlobTrack_mPsiZ[lgc]  , CovGlobTrack_mPsiPsi[lgc], 
	CovGlobTrack_mPtiImp[lgc], CovGlobTrack_mPtiZ[lgc]  , CovGlobTrack_mPtiPsi[lgc], CovGlobTrack_mPtiPti[lgc], 
	CovGlobTrack_mTanImp[lgc], CovGlobTrack_mTanZ[lgc]  , CovGlobTrack_mTanPsi[lgc], CovGlobTrack_mTanPti[lgc], CovGlobTrack_mTanTan[lgc]};
      tL.set(parL,covL);
      tracks.noFitpK = GlobalTracks_mNHits[kg];
      tracks.noFitpL = GlobalTracks_mNHits[lg];
      tracks.chi2 = Chi2Old;
      tracks.K = tK;
      tracks.L = tL;
      // Add sector
      tracks.sectorK = sector(GlobalTracks_mFirstPoint_mX1[kg], GlobalTracks_mFirstPoint_mX2[kg], GlobalTracks_mFirstPoint_mX3[kg]);
      if (tracks.sectorK != sector(GlobalTracks_mLastPoint_mX1[kg], GlobalTracks_mLastPoint_mX2[kg], GlobalTracks_mLastPoint_mX3[kg])) tracks.sectorK = - tracks.sectorK;
      tracks.sectorL = sector(GlobalTracks_mFirstPoint_mX1[lg], GlobalTracks_mFirstPoint_mX2[lg], GlobalTracks_mFirstPoint_mX3[lg]);
      if (tracks.sectorL != sector(GlobalTracks_mLastPoint_mX1[lg], GlobalTracks_mLastPoint_mX2[lg], GlobalTracks_mLastPoint_mX3[lg])) tracks.sectorL = - tracks.sectorL;
      if (netries%100 == 0) {
	cout << "K\t" << kg << "\t" << tracks.sectorK << "\t" << tracks.K << endl;
	cout << "L\t" << lg << "\t" << tracks.sectorL << "\t" << tracks.L << "\tchi2 = " << tracks.chi2 << endl;
      }
      tracks.K70 = StDedxPidTraits(kTpcId, kTruncatedMeanId, 
				   100*((Int_t) GlobalTracks_mProbPidTraits_mdEdxTrackLength[kg])+GlobalTracks_mNHitsDedx[kg],
				   GlobalTracks_mProbPidTraits_mdEdxTruncated[kg],GlobalTracks_mProbPidTraits_mdEdxErrorTruncated[kg]);
      tracks.L70 = StDedxPidTraits(kTpcId, kTruncatedMeanId, 
				   100*((Int_t) GlobalTracks_mProbPidTraits_mdEdxTrackLength[lg])+GlobalTracks_mNHitsDedx[lg],
				   GlobalTracks_mProbPidTraits_mdEdxTruncated[lg],GlobalTracks_mProbPidTraits_mdEdxErrorTruncated[lg]);
      tracks.Kfit = StDedxPidTraits(kTpcId, kLikelihoodFitIdentifier, 
				    100*((Int_t) GlobalTracks_mProbPidTraits_mdEdxTrackLength[kg])+GlobalTracks_mNHitsDedx[kg],
				    GlobalTracks_mProbPidTraits_mdEdxFit[kg],GlobalTracks_mProbPidTraits_mdEdxErrorFit[kg]);
      tracks.Lfit = StDedxPidTraits(kTpcId, kLikelihoodFitIdentifier, 
				    100*((Int_t) GlobalTracks_mProbPidTraits_mdEdxTrackLength[lg])+GlobalTracks_mNHitsDedx[lg],
				    GlobalTracks_mProbPidTraits_mdEdxFit[lg],GlobalTracks_mProbPidTraits_mdEdxErrorFit[lg]);
      Double_t pT = tracks.K.pt();
      tracks.pK  = pT*TMath::Sqrt(1. + tracks.K.tanDip()*tracks.K.tanDip());
      tracks.bgK = tracks.pK/0.1056584; // for muon
      tracks.bichK = 1e-6*m_Bichsel->GetI70M(TMath::Log10(tracks.bgK),1.);
      tracks.zK = -999;
      if (tracks.K70.mean() > 0) tracks.zK = TMath::Log(tracks.K70.mean()/tracks.bichK);
      pT = tracks.L.pt();
      tracks.pL  = pT*TMath::Sqrt(1. + tracks.L.tanDip()*tracks.L.tanDip());
      tracks.bgL = tracks.pL/0.1056584; // for muon
      tracks.bichL = 1e-6*m_Bichsel->GetI70M(TMath::Log10(tracks.bgL),1.);
      tracks.zL = -999;
      if (tracks.L70.mean() > 0) tracks.zL = TMath::Log(tracks.L70.mean()/tracks.bichL);
      tracks.zFitK = -999;
      if (tracks.Kfit.mean() > 0) tracks.zFitK = TMath::Log(tracks.Kfit.mean()/tracks.bichK);
      tracks.zFitL = -999;
      if (tracks.Lfit.mean() > 0) tracks.zFitL = TMath::Log(tracks.Lfit.mean()/tracks.bichL);
      ftree->Fill();
      netries++;
    }
  }
  if (fOut) fOut->Write();
}
//________________________________________________________________________________
void Plot(Int_t nevents = 1e9) {
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (!nn) return;
  TH2D  ****H2 = new TH2D***[nn]; memset(H2, 0, nn*sizeof(TH2D***));
  TIter next(files);
  struct Plot_t {
    const Char_t *Name;
    const Char_t *Title;
    const Char_t *xTitle;
    const Char_t *yTitle;
    Int_t nx;
    Double_t xmin, xmax;
    Int_t ny;
    Double_t ymin, ymax;
  };
  struct VarPr_t {
    Double_t DeltapTI;
    Double_t pullDeltapTI;
    Double_t DeltapTR;
    Double_t pullDeltapTR;
    Double_t DeltaDCAxy;
    Double_t pullDeltaDCAxy;
    Double_t DeltaDCAz;
    Double_t pullDeltaDCAz;
    Double_t DeltaTan;
    Double_t pullDeltaTan;
    Double_t DeltaPsi;
    Double_t pullDeltaPsi;
  };
  VarPr_t V;
  enum {kTotal=12, kAll, kCharge = 5};
  const Plot_t plot[kAll] = {// Name, Title,                     xTitle,              yTitle,                nx, xmin,xmax,  ny,ymin,ymax         
    {"DelpTI"  ,   "#Delta 1/p_{T} from cosmics muons"         , "Log_{10}(p_{TAv})", "#Delta 1/p_{T}"       , 30,   -2,   1, 500, -0.05, 0.05},
    {"PullpTI" ,   "pull of #Delta 1/p_{T} from cosmics muons" , "Log_{10}(p_{TAv})", "pull (#Delta 1/p_{T})", 30,   -2,   1, 500, -5.0, 5.0},
    {"DelpTR"  ,   "#Delta p_{T}/p_{T} from cosmics muons"   , "Log_{10}(p_{TAv})", "#Delta p_{T}/p_{T}"     , 30,   -2,   1, 500, -0.5, 0.5},
    {"PullpTR" ,   "pull of #Delta p_{T}/p_{T} from cosmics muons" , "Log_{10}(p_{TAv})", "pull (#Delta p_{T}/p_{T})", 30,   -2,  1, 500, -5.0, 5.0},
    {"DelImp"  ,   "#Delta DCA_{xy} from cosmics muons"        , "Log_{10}(p_{TAv})", "DCA_{xy}"             , 30,   -2,   1, 500, -1.0, 1.0},
    {"PullImp" ,   "pull of #Delta DCA_{xy} from cosmics muons", "Log_{10}(p_{TAv})", "pull DCA_{xy} "       , 30,   -2,   1, 500, -5.0, 5.0},
    {"DelZ"    ,   "#Delta DCA_{z} from cosmics muons"         , "Log_{10}(p_{TAv})", "DCA_{z}"              , 30,   -2,   1, 500, -1.0, 1.0},
    {"PullZ"   ,   "pull of #Delta DCA_{z} from cosmics muons" , "Log_{10}(p_{TAv})", "pull DCA_{z} "        , 30,   -2,   1, 500, -5.0, 5.0},
    {"DelTan"  ,   "#Delta #lambda from cosmics muons"         , "Log_{10}(p_{TAv})", "#lambda"              , 30,   -2,   1, 500, -0.02, 0.02},
    {"PullTan" ,   "pull of #Delta Tan(#lambda) from cosmics muons" , "Log_{10}(p_{TAv})", "pull Tan(#lambda)",30,   -2,   1, 500, -5.0, 5.0}, // ?
    {"DelPsi"  ,   "#Delta #psi from cosmics muons"            , "Log_{10}(p_{TAv})", "#psi"                 , 30,   -2,   1, 500, -0.02, 0.02},
    {"PullPsi" ,   "pull of #Delta #psi from cosmics muons"    , "Log_{10}(p_{TAv})", "pull #psi "           , 30,   -2,   1, 500, -5.0, 5.0},
    {"sKsL"    ,   "sector L versus sector K"                  , "secotr K",          "Sector L"             , 24,  0.5, 24.5, 24,  0.5, 24.5}
  };
  
  const Char_t *Charge[kCharge] = {"All","Pos","Neg","PosR","NegR"};
  Int_t Npads = kCharge*2*((kAll+1)/2);
  TLegend *leg[Npads]; memset(leg, 0, Npads*sizeof(TLegend *)); 
  Int_t color = 0;
  TF1 *res = new TF1("Res","TMath::Sqrt([0]*[0]*TMath::Power(10.,x) + [1]*[1]*(([2]*[2]+TMath::Power(10.,x))))",-1,1);
  res->SetParameters(1e-2,1e-2,0.1056584);
  res->FixParameter(2,0.1056584);
  CosmicTracks tracks;
  CosmicTracks *ftrack = &tracks;
  TFile *f = 0;
  TCanvas *c1 = new TCanvas("c1","c1",2,10,1200,1200);
  c1->Divide(2*kCharge,Npads/(2*kCharge));
  TH1F *frame = 0;
  Int_t NF = -1;
  Double_t RS[24] = {103.576,   59.800,   -0.000,  -59.800, -103.577, -119.600, -103.577,  -59.801,    0.000,   59.801,  103.576,  119.600,  
		     103.576,   59.798,   -0.001,  -59.801, -103.577, -119.600, -103.576,  -59.800,   -0.001,   59.800,  103.577,  119.600};
  while ( (f = (TFile *) next()) ) {
    f->cd();
    cout << "File: " << f->GetName() << endl;
    NF++;
    H2[NF] = new TH2D**[kAll]; 
    for (Int_t k = 0; k < kAll; k++) {
      H2[NF][k] = new TH2D*[kCharge];
      for (Int_t c = 0; c < kCharge; c++) {
	TString Title(plot[k].Title);
	Title += " "; Title += Charge[c];
	if (c == 0) {}
	else if (c < 3) Title += " DCA_{xy}  <= 50 cm";
	else            Title += " DCA_{xy}  >  50 cm";
	H2[NF][k][c] = new TH2D(Form("%s%s%i",plot[k].Name,Charge[c],NF),Title,plot[k].nx,plot[k].xmin,plot[k].xmax,plot[k].ny,plot[k].ymin,plot[k].ymax);
	H2[NF][k][c]->SetXTitle(plot[k].xTitle);
	H2[NF][k][c]->SetYTitle(plot[k].yTitle);
      }
    }
    TTree *tree = (TTree *) f->Get("CosmicT");
    if (! tree) continue;
    TBranch *branch = tree->GetBranch("CosmicTracksB");
    enum kIntex {kImp = 0, kZ, kPsi, kPti, kTan,
		 kImpImp = 0, 
		 kZImp, kZZ,
		 kPsiImp, kPsiZ, kPsiPsi,
		 kPtiImp, kPtiZ, kPtiPsi, kPtiPti,
		 kTanImp, kTanZ, kTanPsi, kTanPti, kTanTan};
    if (! branch) continue;
    branch->SetAddress(&ftrack);
    Int_t nentries = (Int_t)tree->GetEntries();
    if (nentries < nevents) nevents = nentries;
    for (Int_t ev = 0; ev < nevents; ev++) {
      tree->LoadTree(ev);  //this call is required when using the cache
      tree->GetEntry(ev);   
      if (ev > 0 && ! (ev%10000)) cout << "Read event " << ev << endl;
      if (tracks.noFitpK < 30 || tracks.noFitpL < 30) continue;
      Double_t pTAV     = TMath::Abs(tracks.K.params()[kPti] - tracks.L.params()[kPti])/2;
      Double_t pTAVL10 = 1.95;
      Double_t sigmapTI = TMath::Sqrt((tracks.K.errMatrix()[kPtiPti]+tracks.L.errMatrix()[kPtiPti])/2.);
      if (sigmapTI < 1e-7) continue;
      if (pTAV < 1e-7) continue;
      Int_t sK = TMath::Abs(tracks.sectorK);
      Int_t sL = TMath::Abs(tracks.sectorL);
      Int_t sectorK = sK;
      Int_t sectorL = sL;
      // track K is coming from top 
      StDcaGeometry &K = *&tracks.K;
      StDcaGeometry &L = *&tracks.L;
      if (RS[sK-1] < RS[sL-1]) {
        sectorK = sL;
	sectorL = sK;
	K       = *&tracks.L;
	L       = *&tracks.K;
      }
      Int_t charge = (K.charge() + 3)/2;
      if (TMath::Abs(K.params()[kImp] - L.params()[kImp])/2 > 50.0) charge += 2;
      pTAVL10 = TMath::Log10(pTAV);
      V.DeltapTI = (K.params()[kPti] + L.params()[kPti])/2;
      V.pullDeltapTI = V.DeltapTI/sigmapTI;
      V.DeltapTR = V.DeltapTI*pTAV;
      Double_t sigmapTR = pTAV*sigmapTI;
      V.pullDeltapTR = V.DeltapTR/sigmapTR;
      V.DeltaDCAxy = (K.params()[kImp] + L.params()[kImp])/2;
      Double_t sigmaDCAxy = TMath::Sqrt((K.errMatrix()[kImpImp]+L.errMatrix()[kImpImp])/2.);
      V.pullDeltaDCAxy = V.DeltaDCAxy/sigmaDCAxy;
      
      V.DeltaDCAz = (K.params()[kZ] - L.params()[kZ])/2;
      Double_t sigmaDCAz = TMath::Sqrt((K.errMatrix()[kZZ]+L.errMatrix()[kZZ])/2.);
      V.pullDeltaDCAz = V.DeltaDCAz/sigmaDCAz;
      
      
      V.DeltaTan = (K.params()[kTan] + L.params()[kTan])/2;
      Double_t sigmaTan = TMath::Sqrt((K.errMatrix()[kTanTan]+L.errMatrix()[kTanTan])/2.);
      V.pullDeltaTan = V.DeltaTan/sigmaTan;
      
      V.DeltaPsi = difPsi(K.params()[kPsi],L.params()[kPsi])/2;
      Double_t sigmaPsi = TMath::Sqrt((K.errMatrix()[kPsiPsi]+L.errMatrix()[kPsiPsi])/2.);
      V.pullDeltaPsi = V.DeltaPsi/sigmaPsi;
      
      Double_t *Y = &V.DeltapTI;
      for (Int_t k = 0; k < kTotal; k++) {
	H2[NF][k][0]->Fill(pTAVL10, Y[k]);
	H2[NF][k][charge]->Fill(pTAVL10, Y[k]);
      }
      H2[NF][kTotal][0]->Fill(sectorK,sectorL);
      H2[NF][kTotal][charge]->Fill(sectorK,sectorL);
    }
    TString File(f->GetName());
    File.ReplaceAll("Cosmics","");
    File.ReplaceAll(".root","");
    color++;
    if (color == 2) color = 4;
    for (Int_t k = 0; k < kAll; k++) {
      for (Int_t c = 0; c < kCharge; c++) {
	Int_t pad = kCharge*k + c + 1;
	if (NF == 1 && k == kAll -1) pad += kCharge;
	c1->cd(pad)->SetLogz(1);
	if (! H2[NF][k][c]) continue;
	TH2D *h2 = (TH2D *) f->Get(H2[NF][k][c]->GetName());
	if (! h2) continue;
	if (! leg[pad-1]) {
	  leg[pad-1] = new TLegend(0.1,0.8,0.3,0.9);
	  h2->SetStats(0);
	  h2->Draw("colz");
	  leg[pad-1]->SetFillColor(gStyle->GetLegendFillColor());
	  leg[pad-1]->Draw();
	}
	if (k != kAll -1) {
	  h2->FitSlicesY();
	  TH1D *h_1 = (TH1D *) gDirectory->Get(Form("%s_1",H2[NF][k][c]->GetName()));
	  TH1D *h_2 = (TH1D *) gDirectory->Get(Form("%s_2",H2[NF][k][c]->GetName()));
	  if (h_1) {
	    h_1->SetMarkerStyle(20); h_1->SetMarkerColor(color);
	    h_1->Fit("pol0","er","same",-1.5,0.5);
	    TF1 *pol0 = (TF1 *) h_1->GetListOfFunctions()->FindObject("pol0");
	    Double_t mu = pol0->GetParameter(0);
	    h_1->Draw("same");
	    h_2->SetMarkerStyle(21); h_2->SetMarkerColor(color);
	    h_2->Fit("pol0","er","same",-1.5,0.5);
	    pol0 = (TF1 *) h_2->GetListOfFunctions()->FindObject("pol0");
	    Double_t sigma = pol0->GetParameter(0);
	    //	  h_2->Draw("same");
	    leg[pad-1]->AddEntry(h_1,Form("%s %6.4g #pm %6.4g",File.Data(),mu,sigma));
	    leg[pad-1]->SetFillColor(gStyle->GetLegendFillColor());
	  }
	} else {
	  leg[pad-1]->AddEntry(h2,File.Data());
	  leg[pad-1]->SetFillColor(gStyle->GetLegendFillColor());
	}
	c1->Update();
      }
    }
  }
  //  leg->Draw();
}
//________________________________________________________________________________
