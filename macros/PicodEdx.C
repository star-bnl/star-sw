/*
  root.exe lMuDst.C 'PicodEdx.C+("/gpfs01/star/pwg_tasks/hf05/Pico/BES-I/AuAu27_production_2011/2011/172/12172013/*.picoDst.root","PicoOut.root")'
  root.exe 'lMuDst.C(0,"/gpfs01/star/pwg_tasks/hf05/Pico/BES-I/AuAu62_productionP10ik/2010/078/11078018/*picoDst.root","PicoOut.root")' PicodEdx.C+
  root.exe 'lMuDst.C(0,"/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root","ldEdxY2,RpicoDst","PicoOut.root")' PicodEdx.C+
  root.exe lMuDst.C 'PicodEdx.C+("/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/130/15130037/st_physics_15130037_raw_3000030_5368_5369.MuDst.root")'
  root.exe PicodEdx1_Sparse_pT100_eta24.NewdX.root doFractionFit.C
  
  
  gStyle->SetOptStat(0)
  FitP->Draw("mu:y>>P(720,-360,360,100,-0.1,0.1)","i&&j&&abs(x)<3&&x>0.3","colz")
  FitP->Draw("mu:y>>N(720,-360,360,100,-0.1,0.1)","i&&j&&abs(x)<3&&x<-0.3","colz")
  P->ProfileX()->Draw()
  N->SetMarkerColor(2)
  N->ProfileX()->Draw("same")
  TLegend *l = new TLegend(0.3,0.6,0.5,0.8)
  P_pfx->SetXTitle("#Phi(degrees)");
  l->AddEntry(P_pfx,"Positive")
  l->Draw()
  l->AddEntry(N_pfx,"Negative")
*/
//#define DEBUG
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
#include "StBichsel/StdEdxModel.h"
#include "BetheBloch.h"
#include "TDirIter.h"
#include "TTreeIter.h"
#include "TRandom.h"
#include "TFractionFitter.h"
#include "TLegend.h"
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#if 0
#include "StProbPidTraits.h"
#endif
#include "StdEdxY2Maker/dEdxHist.h"
#include "StdEdxY2Maker/StPidStatus.h"
#undef  StMessMgr
#undef ClassStMessMgr
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
class BetheBloch;
class TDirIter;
class TTreeIter;
#endif
#include "Ask.h"
StPicoDstMaker* maker = 0;
#ifndef DEBUG
static Int_t _debug = 0;
#else
static Int_t _debug = 1;
#endif
Bichsel *m_Bichsel = 0;
#include "Names.h"
//#include "FitP_t.h"
#ifndef __FitP_t_h__
#define __FitP_t_h__
#include "TObject.h"
#include "Names.h"
#include <string.h>
class FitP_t : public TObject {
public: 
  void Reset() {memset(beg, 0, end-beg);}
  void ResetParams() {memset(beg2, 0, end-beg2);}
  FitP_t() {Reset();}
  virtual ~FitP_t() {}
  Char_t beg[1];
  Int_t c, ipT, jeta;
  Double_t RefMult, RefMult_Min, RefMult_Max;
  Double_t pT, pT_Min, pT_Max;
  Double_t eta, eta_Min, eta_Max;
  Double_t KolmoD[KPidParticles*(KPidParticles+1)/2];
  Char_t beg2[1];
  Int_t iter;
  Double_t Frac[KPidParticles];
  Double_t ErFrac[KPidParticles];
  Double_t CovFrac[KPidParticles*(KPidParticles+1)/2];
  Double_t Chisq;
  Double_t Prob;
  Double_t mu, dmu;
  Double_t sigma, dsigma;
  Double_t ProbMu;
  Char_t end[1];
  ClassDef(FitP_t,1)
};
#endif

TFile *fOut = 0;
TTree *FitP = 0;
TCanvas *c1 = 0;
struct Var_t {
  Var_t(Double_t r=0, Double_t c=0, Double_t e=0, Double_t h=-99, Double_t zz=-999) : refMult(r), cpT(c), eta(e), hyp(h), z(zz) {}
  Double_t refMult; // == GoodPrimTracks
  Double_t cpT;     // charge*pT
  Double_t eta;
  Double_t hyp;     // hyp = -1 => data else hyp =  [0, KPidParticles]
  Double_t z;       // log(dE/dx) - log(dE/dx)_predicted_for pion
  //  Double_t x(Int_t i = 0) const {return *(&refMult+i);}
};
enum EBinning {NRefMult = 10, NpT = 101, Neta = 24, Nphi = 720, Nz = 200, NL = 75};
const static Double_t refmultBins[NRefMult+1] = {0,  180,  290,  390,  475,  555,  630,  705,  780,   850,  3000};
//const static Double_t refmultBins[NRefMult+1] = { 0, 51, 83 , 111, 137, 161, 185, 207, 230, 254, 495}; //

const static Float_t cpTBins[NpT+1] = {
  -50.00,-1.79,-1.52,-1.35,-1.24,-1.15,-1.08,-1.01,-0.96,-0.92,
  -0.88,-0.84,-0.80,-0.77,-0.74,-0.72,-0.69,-0.67,-0.64,-0.61,
  -0.59,-0.58,-0.56,-0.54,-0.53,-0.51,-0.49,-0.47,-0.46,-0.44,
  -0.42,-0.41,-0.39,-0.38,-0.36,-0.35,-0.34,-0.32,-0.31,-0.30,
  -0.28,-0.27,-0.25,-0.24,-0.23,-0.21,-0.20,-0.15,-0.10,-0.05,
  0.00, 0.05, 0.11, 0.16, 0.20, 0.22, 0.23, 0.24, 0.26, 0.27,
  0.29, 0.30, 0.31, 0.33, 0.34, 0.36, 0.37, 0.38, 0.40, 0.41,
  0.43, 0.45, 0.46, 0.48, 0.50, 0.51, 0.53, 0.55, 0.56, 0.58,
  0.60, 0.62, 0.64, 0.67, 0.69, 0.72, 0.75, 0.77, 0.80, 0.83,
  0.87, 0.92, 0.96, 1.00, 1.06, 1.13, 1.19, 1.29, 1.40, 1.57,
  1.86,50.00};

enum EVar {
  //  kCharge,
  kRefMult,
  kcpT,
  kEta,
  kHyp,
  kZ,
  NoDim
};
static TH1   *fFitResults = 0;
TLegend *leg = 0;
#ifndef __APPLE__
Int_t nPng = 0;
#endif
const static  Double_t pTMax = 50.;
const static  Double_t pTMaxL = TMath::Log(pTMax/0.1);
const static  Double_t etaMax = 1.2;
const static  Char_t *NameV[NoDim] = {"refMult",  "charge*pT", "#eta",               "hyp", "z"};
const static  Int_t  nBins[NoDim]  = {       10,          NpT,   Neta,     KPidParticles+1,1000};
const static  Var_t  xMin          = {       0.,           0.,-etaMax,                -1.5, -3.};
const static  Var_t  xMax          = {    3000.,       pTMaxL, etaMax, KPidParticles - 0.5,  7.};
FitP_t FitParams;

//ClassImp(FitP_t);
//________________________________________________________________________________
Int_t Debug() {return _debug;}
//________________________________________________________________________________
Double_t log2dX70(Double_t pT, Double_t eta) {
  Double_t par[3] = {0, 1.92719e-01, 3.57573e-01};
  Double_t B = 0.5; // T => 5 kG
  Double_t R  = 100 * pT/(0.3 * 0.1 * B); // cm
  Double_t L  = 2 + par[1]; // cm
  Double_t alpha = 0;
  if (2*R > L) alpha = TMath::ASin(L/(2*R));
  Double_t dxPhi = 2*R*alpha;
  if (dxPhi < 1) dxPhi = 1;
  Double_t dX = dxPhi*TMath::CosH(par[2]*eta);
  return TMath::Log2(dX);
  //  Double_t p = pT*TMath::CosH(eta);
  //  Double_t poverm = p/Masses[kPidPion];
  //  return par[0]+TMath::Log(m_Bichsel->GetI70(TMath::Log10(poverm),TMath::Log2(dX)))
  
}
//______________________________________________________________________
Int_t IndexH(const Char_t *name) {
  Int_t index = -1;
  TString Name(name);
  for (Int_t l = 0; l < KPidParticles; l++) {
    //    cout << Name << "\t" << PidNames[l] << endl;
    if (Name.BeginsWith(PidNames[l])) {index = l; break;}
  }
  //  if (index >= 0) cout << "Found " << Name << "\t" << PidNames[index] << "\tindex\t" << index << endl;      
  return index;
}
//________________________________________________________________________________
void PicodEdx(const Char_t *files ="./*.MuDst.root",
	      const Char_t *Out = "PicodEdx1.root"){
  //  static const Double_t sigmaB[2] = {6.26273e-01, -5.80915e-01}; // Global Tracks, wrt Bichsel
  if (!m_Bichsel) {
    gSystem->Load("libStBichsel"); 
    gSystem->Load("libStarClassLibrary");  
    m_Bichsel = Bichsel::Instance();
    StdEdxModel::instance();
  }
  if (! gRandom) new TRandom();
  TString OutFName(Out);
  if (fOut) fOut->cd();
  else      fOut = new TFile(OutFName,"RECREATE");
  Int_t      nZBins = 200;
  Double_t ZdEdxMin = -5;
  Double_t ZdEdxMax =  5;
  TH1F *PrimMult = new TH1F("PrimMult","No. of total Primary tracks in event",100,0,5000);
  TH1F *RefMultPos = new TH1F("RefMultPos","Ref. multiplicity the first vertex",500,0,500);
  TH1F *RefMultNeg = new TH1F("RefMultNeg","Ref. multiplicity the first vertex",500,0,500);
  TH1F *GoodPrimTracks = new TH1F("GoodPrimTracks","No. of good Primary track at the first vertex",100,0,2000);
  TH2F *TPoints70   = new TH2F("TPoints70","dEdx(I70)/Pion versus Length in Tpc  for All",
			       210,10,220., 500,-1.,4.);
  TH2F *TPointsF   = new TH2F("TPointsF","dEdx(fit)/Pion versus Length in Tpc  for All",
			      210,10,220., 500,-1.,4.);
  TH2F *TPointsN   = new TH2F("TPointsN","dNdx(fit)/Pion versus Length in Tpc  for All",
			      210,10,220., 500,-1.,4.);
  TH2F *TdEdxP70    = new TH2F("TdEdxP70","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c))", 
			       150,-1.,2., 500,-1.,4.);
  TH2F *TdEdxP7040cm    = new TH2F("TdEdxP7040cm","log10(dE/dx(I70)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
				   150,-1.,2., 500,0.,2.5);
  TH2F *TdEdxPF    = new TH2F("TdEdxPF","log10(dE/dx(fit)(keV/cm)) versus log10(p(GeV/c))", 
			      150,-1.,2., 500,0.,2.5);
  TH2F *TdEdxPF40cm    = new TH2F("TdEdxPF40cm","log10(dE/dx(fit)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
				  150,-1.,2., 500,0.,2.5);
  TH2F *TdEdxPN    = new TH2F("TdEdxPN","log10(dN/dx(fit)(1/cm) versus log10(p(GeV/c))", 
			      150,-1.,2., 500,0.5,3.0);
  TH2F *TdEdxPN40cm    = new TH2F("TdEdxPN40cm","log10(dN/dx(fit)(1/cm) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm", 
				  150,-1.,2., 500,0.5,3.0);
  TH2F *Pull70 = new TH2F("Pull70","log(I70/I(pi)))/D70  versus track length", 
			  150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
  TH2F *FitPull= new TH2F("FitPull","(zFit - log(I(pi)))/dzFit  versus track length", 
			  150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
  TH2F *PullN = new TH2F("PullN","log(dN/dx/I(pi)))/DN  versus track length", 
			 150,10.,160,nZBins,ZdEdxMin,ZdEdxMax);
  TString TitleX("z - z_{#pi} versus ");
  for (Int_t i = 0; i < NoDim; i++) {
    if (i) {TitleX += " ";}
    TitleX += NameV[i];
  }
  Float_t *etaBins = new Float_t[Neta+1];
  Float_t *phiBins = new Float_t[Nphi+1];
  Float_t *zBins   = new Float_t[Nz+1];
  Float_t *LBins   = new Float_t[NL+1];
  for (Int_t i = 0; i <= Neta; i++) etaBins[i] = -etaMax + 2*etaMax/Neta*i;
  for (Int_t i = 0; i <= Nphi; i++) phiBins[i] = -360.5 + (720./Nphi)*i;
  for (Int_t i = 0; i <= Nz;   i++) zBins[i] = -5 + 10./Nz*i;
  for (Int_t i = 0; i <= NL;   i++) LBins[i] = 2*i;
  TH2F *TPs[3]        = {TPoints70, TPointsF, TPointsN};
  TH2F *Pulls[3]      = {Pull70, FitPull, PullN};
  TH2F *TdEdxs[3]     = {TdEdxP70, TdEdxPF, TdEdxPN};
  TH2F *TdEdxs40cm[3] = {TdEdxP7040cm, TdEdxPF40cm, TdEdxPN40cm};
  TH1F *cpTh     = new TH1F("cpTh","q*pT distribution",500,-pTMax,pTMax); 
  TH1F *Etah     = new TH1F("Etah","Eta distribution",100,-5.,5.);
  TH2F *zZ       = new TH2F("zZ","zTpc - zVpd versus zTpc for highest rank vertex", 200, -200, 200, 100, -50, 50); 
  delete [] etaBins;
  delete [] phiBins;
  delete [] zBins;
  delete [] LBins;
  Hists2D I70("I70");
  Hists2D fitZ("fitZ");
  Hists2D fitN("fitN");
#if 1
  maker = new StPicoDstMaker(StPicoDstMaker::IoRead,files);
  maker->Init();
#else
  maker = (StPicoDstMaker *) StMaker::GetTopChain()->Maker("PicoDst");
  if (! maker) return;
#endif
  maker->SetStatus("*",1);
#if 0
  maker->SetStatus("*",0);
  const Char_t *ActiveBranches[] = {
    "MuEvent"
    ,"PrimaryVertices"
    ,"PrimaryTracks"
    ,"GlobalTracks"
    ,"CovPrimTrack"
    ,"CovGlobTrack"
    ,"BTofHeader"
#if 0
    ,"StStMuMcVertex"
    ,"StStMuMcTrack"
#endif
    ,"KFTracks"
    ,"KFVertices"
  };
  Int_t Nb = sizeof(ActiveBranches)/sizeof(Char_t *);
  for (Int_t i = 0; i < Nb; i++) maker->SetStatus(ActiveBranches[i],1); // Set Active braches
#endif
  //         Now iterations
  Var_t Var;
  TChain *tree = maker->chain();
  Long64_t nentries = tree->GetEntries();
  if (nentries <= 0) return;
#ifdef DEBUG
  Long64_t nevent = 9;
#else
  Long64_t nevent = 9999999;
#endif
  nevent = TMath::Min(nevent,nentries);
  cout << nentries << " events in chain " << nevent << " will be read." << endl;
  tree->SetCacheSize(-1);        //by setting the read cache to -1 we set it to the AutoFlush value when writing
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,34,0)
  tree->SetCacheLearnEntries(1); //one entry is sufficient to learn
  tree->SetCacheEntryRange(0,nevent);
#endif
  
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    // for Maxim ============================================================
    StPicoDst* pico = maker->picoDst();   // get a pointer to the StPicoDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StPicoEvent* picoEvent = pico->event(); // get a pointer to the class holding event-wise information
    if (Debug()) {cout << " #" << ev;}
    Int_t NoGlobalTracks = pico->numberOfTracks( );  if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    if (Debug())                                                               {cout << endl;}
    TVector3 pRcVx = picoEvent->primaryVertex();
    Double_t zTpc = pRcVx.z();
    PrimMult->Fill(NoGlobalTracks);
    RefMultPos->Fill(picoEvent->refMultPos());
    RefMultNeg->Fill(picoEvent->refMultNeg());
    Int_t noGoodPrimTracks = 0;
    Bool_t HFTon = kFALSE;
    for (Int_t k = 0; k < NoGlobalTracks; k++) {
      StPicoTrack *gTrack = pico->track(k);
      if (!gTrack) continue;
      StPicoTrack *pTrack = gTrack;
      if (!gTrack->isPrimary()) continue;
      StPicoTrackCovMatrix *gCov = pico->trackCovMatrix(k);
      if (! gCov) continue;
      StDcaGeometry dcaG  = gCov->dcaGeometry();
      KFParticle particle = dcaG.Particle(k);
#ifdef DEBUG
      cout << k << "\t" << particle <<endl;
#endif
      StPidStatus PiD(gTrack); 
      if (PiD.PiDStatus < 0) continue;
      memset (&Var.refMult, 0, sizeof(Var_t));
      Var.refMult = NoGlobalTracks;
      Int_t charge = gTrack->charge();
      Int_t sCharge = (charge + 1)%2;
      Double_t p = gTrack->gMom().Mag();
      //      Double_t Zs[3] = {dEdxL[0] - zPred[0][kPidPion], dEdxL[1] - zPred[1][kPidPion], dEdxL[2] - zPred[2][kPidPion]};
      //      Double_t cpT = gTrack->charge()*pOut.perp();
      Double_t cpT = charge*pTrack->pPt();
      Var.cpT = cpT;
      Var.eta = pTrack->pMom().Eta();
      Double_t phi = TMath::RadToDeg()*pTrack->pMom().Phi();
      if (phi < 0) phi += 360;
      if (Var.eta < 0) phi -= 360;
      Double_t zPred[3][KPidParticles];
      Double_t sPred[3][KPidParticles]; // errors versus bg10
      Double_t dEdx[3] = {PiD.fI70.I(), PiD.fFit.I(), PiD.fdNdx.I()};
      if (dEdx[0] <= 0 || dEdx[1] <= 0) continue;
      Double_t dEdxL[3]   = {TMath::Log(dEdx[0]), TMath::Log(dEdx[1]), dEdx[2] > 0 ? TMath::Log(dEdx[2]):0};
      Double_t dEdxL10[3] = {TMath::Log10(dEdx[0]), TMath::Log10(dEdx[1]), dEdx[2] > 0 ? TMath::Log10(dEdx[2]):0};
      Double_t sigmas[3] = {pTrack->dEdxError(), pTrack->dEdxError(), 0};
      Double_t nSigmasPi[3] = {PiD.fI70.D(), PiD.fFit.D(), PiD.fdNdx.D()};
      Double_t Zs[3] = {PiD.fI70.dev[kPidPion], PiD.fFit.dev[kPidPion], PiD.fdNdx.dev[kPidPion]};
      for (Int_t m = 0; m < 3; m++) {// I70 && Fit && dNdx
	if (sigmas[m] > 0) {
	  Var.hyp = -1;
	  Var.z = Zs[m];
	  //	  TPs[m]->Fill(pTrack->probPidTraits().dEdxTrackLength(), Zs[m]);
	  //	  Pulls[m]->Fill(pTrack->probPidTraits().dEdxTrackLength(), Zs[m]/sigmas[m]);
	  TdEdxs[m]->Fill(TMath::Log10(p), dEdxL10[m]);
	}
      }
#if 0
      for (Int_t l = kPidElectron; l < KPidParticles; l++) {
	Int_t k = PiD.PiDkeyU3;
	if (PiD.fI70.fPiD) {
	  I70.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
	  I70.dev[l][      2]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
	  if (Debug()) cout << "I70.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.fI70.dev[l] << endl;
	  if (k >= 0) {
	    I70.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
	    I70.devT[l][      2]->Fill(PiD.bghyp[l],PiD.fI70.dev[l]);
	    if (Debug()) cout << "I70.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.fI70.dev[l] << endl;
	  }
	}
	if (PiD.fFit.fPiD) {
	  fitZ.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
	  fitZ.dev[l][      2]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
	  if (Debug()) cout << "fitZ.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.fFit.dev[l] << endl;
	  if (k >= 0) {
	    fitZ.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
	    fitZ.devT[l][      2]->Fill(PiD.bghyp[l],PiD.fFit.dev[l]);
	    if (Debug()) cout << "fitZ.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.fFit.dev[l] << endl;
	  }
	}
	if (PiD.fdNdx.fPiD) {
	  fitN.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	  fitN.dev[l][      2]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	  if (Debug()) cout << "fitN.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.fdNdx.dev[l] << endl;
	  if (k >= 0) {
	    fitN.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	    fitN.devT[l][      2]->Fill(PiD.bghyp[l],PiD.fdNdx.dev[l]);
	    if (Debug()) cout << "fitN.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.fdNdx.dev[l] << endl;
	  }
	}
      }
#endif
      noGoodPrimTracks++;
    } // track loop
    GoodPrimTracks->Fill(noGoodPrimTracks);
  } // event loop
  if (fOut) fOut->Write();
}
