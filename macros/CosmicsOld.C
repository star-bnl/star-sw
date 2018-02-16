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
void Cosmics(Char_t *files ="./*.MuDst.root",
	    Char_t *Out = "Cosmics.root"
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
      if (tracks.K70.mDedx > 0) tracks.zK = TMath::Log(tracks.K70.mDedx/tracks.bichK);
      pT = tracks.L.pt();
      tracks.pL  = pT*TMath::Sqrt(1. + tracks.L.tanDip()*tracks.L.tanDip());
      tracks.bgL = tracks.pL/0.1056584; // for muon
      tracks.bichL = 1e-6*m_Bichsel->GetI70M(TMath::Log10(tracks.bgL),1.);
      tracks.zL = -999;
      if (tracks.L70.mDedx > 0) tracks.zL = TMath::Log(tracks.L70.mDedx/tracks.bichL);
      tracks.zFitK = -999;
      if (tracks.Kfit.mDedx > 0) tracks.zFitK = TMath::Log(tracks.Kfit.mDedx/tracks.bichK);
      tracks.zFitL = -999;
      if (tracks.Lfit.mDedx > 0) tracks.zFitL = TMath::Log(tracks.Lfit.mDedx/tracks.bichL);
      ftree->Fill();
      netries++;
    }
  }
  if (fOut) fOut->Write();
}
//________________________________________________________________________________
void Plot(const Char_t *opt="DPti") {
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  Int_t nn = files->GetSize();
  if (!nn) return;
  TH2 **h2 = new TH2*[nn]; memset(h2, 0, nn*sizeof(TH2*));
  TH1 **h1 = new TH1*[nn]; memset(h1, 0, nn*sizeof(TH1*));
  TIter next(files);
  TFile *f = 0;
  TString cn("c"); cn += opt;
  TCanvas *c1 = new TCanvas(cn,cn);
  TString xTitle("Log_{10}(p_{TAv})");
  TString Cut("noFitpK>30&&noFitpL>30&&chi2<200&&TMath::Abs(K.mPti-L.mPti)>0");
  TString Title, yTitle;
  TString draw;
  TString Log10pTi("-TMath::Log10(TMath::Abs(K.mPti-L.mPti)/2.)");
  TString Opt(opt);
  TH1F *frame = 0;
  if      (Opt.Contains("PullPti")) {
    draw = Form("(K.mPti+L.mPti)/TMath::Sqrt(K.mPtiPti+L.mPtiPti):%s>>%s(30,-1,2,100,-5,5)",Log10pTi.Data(),Opt.Data());
    Title = "#sigma for 1/p_{T} pull from cosmics muons";
    yTitle = "#sigma (#Delta 1/p_{T}/#sigma 1/p_{T})";
  } else if (Opt.Contains("PullImp")) {
    draw = Form("(K.mImp+L.mImp)/TMath::Sqrt(K.mImpImp+L.mImpImp):%s>>%s(30,-1,2,100,-5,5)",Log10pTi.Data(),Opt.Data());
    Title = "#sigma for #rho pull from cosmics muons";
    yTitle = "#sigma (#Delta #rho/#sigma #rho)";
  } else if (Opt.Contains("PullZ")) {
    draw = Form("(K.mZ-L.mZ)/TMath::Sqrt(K.mZZ+L.mZZ):%s>>%s(30,-1,2,100,-5,5)",Log10pTi.Data(),Opt.Data());
    Title = "#sigma for Z pull from cosmics muons";
    yTitle = "#sigma (#Delta Z/#sigma Z)";
  } else if (Opt.Contains("PullPsi")) {
    draw = Form("difPsi(K.mPsi,L.mPsi)/TMath::Sqrt(K.mPsiPsi+L.mPsiPsi):%s>>%s(30,-1,2,100,-5,5)",Log10pTi.Data(),Opt.Data());
    Title = "#sigma for #psi pull from cosmics muons";
    yTitle = "#sigma (#Delta #psi/#sigma #psi)";
  } else if (Opt.Contains("PullTan")) {
    draw = Form("(K.mTan+L.mTan)/TMath::Sqrt(K.mTanTan+L.mTanTan):%s>>%s(30,-1,2,100,-5,5)",Log10pTi.Data(),Opt.Data());
    Title = "#sigma for tan(#lambda) pull from cosmics muons";
    yTitle = "#sigma (#Delta tan(#lambda)/#sigma tan(#lambda))";
  } else if (Opt.Contains("DPti")) {
    draw = Form("(K.mPti+L.mPti)*TMath::Sqrt(2.)/TMath::Abs(K.mPti-L.mPti):%s>>%s(30,-1,2,100,-0.2,0.2)",Log10pTi.Data(),Opt.Data());
    Title = "Relative p_{T} resolution from cosmics muons  (No. fit points > 30)";
    yTitle = "#sigma (#Delta p_{T}/p_{TAv})/ #sqrt{2}";
    frame = c1->DrawFrame(-1,5e-3,1,0.5, Title);
    frame->SetXTitle(xTitle);
    frame->SetYTitle(yTitle);
    c1->SetLogy(1);
  } else if (Opt.Contains("dEdxbg")) {
    draw = Form("zK:TMath::Log10(bgK)>>%s(50,0,5.,100,-.5,0.5)",Opt.Data());
    Title = "Z = log((dE/dx)/(dEdx_{Bichsel})) versus Log_{10}(#beta #gamma) for cosmics muons";
    yTitle = "Z";
    xTitle = "Log_{10} (#beta #gamma)";
  } else if (Opt.Contains("dEdx")) {
    draw = Form("zK:%s>>%s(30,-1,2,100,-0.25,0.25)",Log10pTi.Data(),Opt.Data());
    Title = "Z = log((dE/dx)/(dEdx_{Bichsel})) versus Log_{10}(p_{TAv}) for cosmics muons";
    yTitle = "Z";
  }
  TLegend *leg = new TLegend(0.4,0.6,0.8,0.8);
  Int_t color = 0;
  TF1 *res = new TF1("Res","TMath::Sqrt([0]*[0]*TMath::Power(10.,2*x) + [1]*[1]*(([2]*[2]+TMath::Power(10.,2*x))/TMath::Power(10.,2*x)))",-1,4);
  res->SetParameters(1e-2,1e-2,0.1056584);
  res->FixParameter(2,0.1056584);
  while ( (f = (TFile *) next()) ) {
    f->cd();
    cout << "File: " << f->GetName() << endl;
    TTree *CosmicT = (TTree *) f->Get("CosmicT");
    if (! CosmicT) continue;
    TString File(f->GetName());
    File.ReplaceAll("Cosmics.","");
    File.ReplaceAll(".root","");
    color++;
    xTitle = "Log_{10}(p_{TAv})";
    cout << "draw: " << draw.Data() << "\tCut :" << Cut.Data() << endl;
    CosmicT->Draw(draw,Cut,"goff");
    h2[color-1] = (TH2 *) gDirectory->Get(Opt);
    if (! h2[color-1]) {cout << "Could not find hist: " << Opt.Data() << endl; continue;}
    h2[color-1]->FitSlicesY();
    h1[color-1] = (TH1 *) gDirectory->Get(Form("%s_2",h2[color-1]->GetName()));
    h1[color-1]->SetTitle(Title);
    h1[color-1]->SetXTitle(xTitle);
    h1[color-1]->SetYTitle(yTitle);
    h1[color-1]->SetMarkerStyle(20);
    h1[color-1]->SetMarkerColor(color);
    h1[color-1]->SetLineColor(color);
    h1[color-1]->SetStats(0);
    res->SetParameters(1e-2,1e-2,0.1056584);
    res->FixParameter(2,0.1056584);
    h1[color-1]->Fit(res,"er0","",-1.,1.);
    TString legT(Form("a = %8.2g +/- %8.2g, b = %8.2g +/- %8.2g,",
		      res->GetParameter(0),res->GetParError(0),
		      res->GetParameter(1),res->GetParError(1)));
    legT += File;
    leg->AddEntry(h1[color-1],legT);
#if 0
    if (color == 1) leg->AddEntry(h1[color-1],"without correction (No. fit points > 30)");
    if (color == 2) leg->AddEntry(h1[color-1],"with correction (No. fit points > 30)");
#endif
  }
  if (h1[0]) h1[0]->Draw();
  TString same("");
  if (frame) same = "same";
  for (Int_t i = 0; i < nn; i++) {
    if (h1[i])  {h1[i]->Draw(same); same = "same";}
  }
  leg->Draw();
}
//________________________________________________________________________________
