/*
  root.exe 'lMuDst.C(-2,"/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/130/15130037/*.MuDst.root","RMuDst","MuOut.root")' MudEdx.C+
  root.exe lMuDst.C 'MudEdx.C+("/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/130/15130037/st_physics_15130037_raw_3000030_5368_5369.MuDst.root")'
  root.exe MudEdx1_Sparse_pT100_eta24.NewdX.root doFractionFit.C


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
//#define __SPARSE__
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
#include "THnSparse.h"
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
#include "StBTofHeader.h"
#include "StMuDSTMaker/COMMON/StMuTimer.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
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
StMuDstMaker* maker = 0;
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
void MudEdx(const Char_t *files ="./*.MuDst.root",
	    const Char_t *Out = "MudEdx1.root"){
  //  static const Double_t sigmaB[2] = {6.26273e-01, -5.80915e-01}; // Global Tracks, wrt Bichsel
  if (!m_Bichsel) {
    gSystem->Load("StBichsel"); 
    gSystem->Load("StarClassLibrary");  
    m_Bichsel = Bichsel::Instance();
    StdEdxModel::instance();
  }
  if (! gRandom) new TRandom();
  TString OutFName(Out);
#ifdef __SPARSE__
  OutFName.ReplaceAll(".",Form("_Sparse_pT%i_eta%i.dX.Fit.",NpT,Neta));
#endif /* __SPARSE__ */
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
#ifdef __SPARSE__
  THnSparseF  *sZs[3] = {0};
  for (Int_t m = 0; m < 3; m++) {
    if (m == 0) sZs[m] = new THnSparseF("sZdEdx", TitleX, NoDim, nBins, 0, 0);
    if (m == 1) sZs[m] = new THnSparseF("sZdEdxFit", TitleX, NoDim, nBins, 0, 0);
    if (m == 2) sZs[m] = new THnSparseF("sZdNdx", TitleX, NoDim, nBins, 0, 0);
    fOut->Append(sZs[m]);
    for (Int_t i = 0; i < NoDim; i++) { 
      sZs[m]->GetAxis(i)->SetName(NameV[i]);
      if      (i == 0)  sZs[m]->SetBinEdges(i,refmultBins);
      else if (i == 1)  sZs[m]->SetBinEdges(i,cpTBins);
      else {
	Double_t*bins = new Double_t[nBins[i]+1];
	Double_t x = xMin.x(i);
	Double_t dx = (xMax.x(i) - xMin.x(i))/nBins[i];
	for (Int_t j = 0; j <= nBins[i]; j++) {bins[j] = x; x += dx;}
	sZs[m]->SetBinEdges(i,bins);
	delete [] bins;
      }
    }
  }
#endif /* __SPARSE__ */
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
#ifdef __dEdxPhi__
  TH2F *pTPhiT   = new TH2F("pTPhiT","Phi of Track (-East +West) q*pT for all tracks"
			    ,NpT,cpTBins,Nphi,phiBins);
  TH2F *pTPhiD   = new TH2F("pTPhiD","Phi of 1-st hit (-East +West) q*pT for all tracks"
			    ,NpT,cpTBins,Nphi,phiBins);
  TH2F *pTEta    = new TH2F("pTEta","Eta q*pT for all tracks"
			    ,NpT,cpTBins,Neta,etaBins);
  TH2F *pTEtaPiD = new TH2F("pTEtaPiD","Eta versus q*pT"
			    ,NpT,cpTBins,Neta,etaBins);
  TH2F *pTEtaPiD40cm = new TH2F("pTEtaPiD40cm","Eta versus q*pT for tracks with TpcLength > 40 cm"
			    ,NpT,cpTBins,Neta,etaBins);
  TH3F *pTEtaPiDz= new TH3F("pTEtaPiDz","Eta versus q*pT and z (Pion)"
			    ,NpT,cpTBins,Neta,etaBins, Nz, zBins);
  TH3F *pTEtaPiD40cmz= new TH3F("pTEtaPiD40cmz","Eta versus q*pT and z (Pion) for tracks with TpcLength > 40 cm"
			    ,NpT,cpTBins,Neta,etaBins, Nz, zBins);
  TH2F *pTPhiPiD = new TH2F("pTPhiPiD","Phi of 1-st hit (-East +West) q*pT"
			    ,NpT,cpTBins,Nphi,phiBins);
  TH2F *pTPhiPiD40cm = new TH2F("pTPhiPiD40cm","Phi of 1-st hit (-East +West) q*pT for tracks with TpcLength > 40 cm"
			    ,NpT,cpTBins,Nphi,phiBins);
  TH3F *pTPhiPiDz= new TH3F("pTPhiPiDz","z vs Phi of 1-st hit (-East +West) q*pT and z (Pion)"
			    ,NpT,cpTBins,Nphi,phiBins, Nz, zBins);
  TH3F *pTPhiPiD40cmz= new TH3F("pTPhiPiD40cmz","z vs Phi of 1-st hit (-East +West) q*pT and z (Pion) for tracks with TpcLength > 40 cm"
			    ,NpT,cpTBins,Nphi,phiBins, Nz, zBins);
  TH3F *pTPhiPiDsigma= new TH3F("pTPhiPiDsigma","sigma vs Phi of 1-st hit (-East +West) q*pT and sigma (Pion)"
			    ,NpT,cpTBins,Nphi,phiBins, Nz, zBins);
  TH3F *pTPhiPiD40cmsigma= new TH3F("pTPhiPiD40cmsigma","sigma vs Phi of 1-st hit (-East +West) q*pT and sigma (Pion) for tracks with TpcLength > 40 cm"
			    ,NpT,cpTBins,Nphi,phiBins, Nz, zBins);
  TH3F *pTPhiPiDL= new TH3F("pTPhiPiDL","L vs Phi of 1-st hit (-East +West) q*pT and z (Pion)"
			    ,NpT,cpTBins,Nphi,phiBins, NL, LBins);
  TH3F *pTPhiPiD40cmL= new TH3F("pTPhiPiD40cmL","L vs Phi of 1-st hit (-East +West) q*pT and z (Pion) for tracks with TpcLength > 40 cm"
			    ,NpT,cpTBins,Nphi,phiBins, NL, LBins);
  TH2F *pTEtaPiDHFToff = new TH2F("pTEtaPiDHFToff","Eta versus q*pT HFToff"
			    ,NpT,cpTBins,Neta,etaBins);
  TH3F *pTEtaPiDzHFToff= new TH3F("pTEtaPiDzHFToff","Eta versus q*pT and z (Pion) HFToff"
			    ,NpT,cpTBins,Neta,etaBins, Nz, zBins);
  TH2F *pTPhiPiDHFToff = new TH2F("pTPhiPiDHFToff","Phi of 1-st hit (-East +West) q*pT HFToff"
			    ,NpT,cpTBins,Nphi,phiBins);
  TH3F *pTPhiPiDzHFToff= new TH3F("pTPhiPiDzHFToff","z vs Phi of 1-st hit (-East +West) q*pT and z (Pion) HFToff"
			    ,NpT,cpTBins,Nphi,phiBins, Nz, zBins);
  TH3F *pTPhiPiDLHFToff= new TH3F("pTPhiPiDLHFToff","L vs Phi of 1-st hit (-East +West) q*pT and z (Pion) HFToff"
			    ,NpT,cpTBins,Nphi,phiBins, NL, LBins);
#endif /* __dEdxPhi__ */
  TH2F *zZ       = new TH2F("zZ","zTpc - zVpd versus zTpc for highest rank vertex", 200, -200, 200, 100, -50, 50); 
  delete [] etaBins;
  delete [] phiBins;
  delete [] zBins;
  delete [] LBins;
  Hists2D I70("I70");
  Hists2D fitZ("fitZ");
  Hists2D fitN("fitN");
  StMuDebug::setLevel(0);  
  maker = new StMuDstMaker(0,0,"",files,"st:MuDst.root",1e9);   // set up maker in read mode
  //                       0,0                        this mean read mode
  //                           dir                    read all files in this directory
  //                               file               bla.lis real all file in this list, if (file!="") dir is ignored
  //                                    filter        apply filter to filenames, multiple filters are separated by ':'
  //                                          10      maximum number of file to read
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
    StMuDst* mu = maker->muDst();   // get a pointer to the StMuDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
    if (Debug()) {cout << " #" << ev;}
    TClonesArray *PrimaryVertices   = mu->primaryVertices(); 
    Int_t NoPrimaryVertices = PrimaryVertices->GetEntriesFast();  if (Debug()) {cout << "\tPrimaryVertices " << NoPrimaryVertices;}
    TClonesArray *PrimaryTracks    = mu->array(muPrimary);  
    Int_t NoPrimaryTracks = PrimaryTracks->GetEntriesFast();      if (Debug()) {cout << "\tPrimaryTracks " << NoPrimaryTracks;}
    TClonesArray *GlobalTracks     = mu->array(muGlobal);  
    Int_t NoGlobalTracks = GlobalTracks->GetEntriesFast();        if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    TClonesArray *CovGlobTrack     = mu->covGlobTrack();          if (Debug()) {cout << "\tCovGlobTrack " << CovGlobTrack->GetEntriesFast();}
    TClonesArray *KFVertices       = mu->KFVertices(); Int_t NoKFVertices = KFVertices->GetEntriesFast();if (Debug()) {cout << "\tNoKFVertices " << NoKFVertices;}
    TClonesArray *KFTracks         = mu->KFTracks();   Int_t NoKFTracks = KFTracks->GetEntriesFast(); if (Debug()) {cout << "\tNoKFTracks " << NoKFTracks;}
    if (Debug())                                                               {cout << endl;}
    StMuPrimaryVertex *RcVx = (StMuPrimaryVertex *) PrimaryVertices->UncheckedAt(0);
    if (! RcVx) continue; 
    Double_t zTpc = RcVx->position().z();
    Double_t VpdZ = -9999;
    StBTofHeader* BTofHeader = mu->btofHeader();
    if ( BTofHeader) {
      UInt_t NoWestHits = BTofHeader->numberOfVpdHits(west);
      UInt_t NoEastHits = BTofHeader->numberOfVpdHits(east);
      if ( NoWestHits > 0 &&  NoEastHits > 0) {
	VpdZ = BTofHeader->vpdVz();
	zZ->Fill(zTpc,zTpc-VpdZ);
      }
    }
    PrimMult->Fill(NoPrimaryTracks);
    RefMultPos->Fill(RcVx->refMultPos());
    RefMultNeg->Fill(RcVx->refMultNeg());
    Int_t noGoodPrimTracks = 0;
    Bool_t HFTon = kFALSE;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
      if (  pTrack->vertexIndex() != 0) continue;
      Int_t kg = pTrack->index2Global();
      if (kg < 0) continue;
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (! gTrack) continue;
      const StThreeVectorF &firstPoint = gTrack->firstPoint();
      Double_t R = firstPoint.perp();
      if (R < 40.0) HFTon = kTRUE;
    }    
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      StMuTrack *pTrack = (StMuTrack *) PrimaryTracks->UncheckedAt(k);
      if (  pTrack->vertexIndex() != 0) continue;
      Int_t kg = pTrack->index2Global();
      if (kg < 0) continue;
      StMuTrack *gTrack = (StMuTrack *) GlobalTracks->UncheckedAt(kg);
      if (! gTrack) continue;
#ifdef DEBUG
      cout << k << "\tNHits " << (int) pTrack->nHits()
	   << "\tNHitsDedx " << (int) pTrack->nHitsDedx()
	   << "\tpT " << pTrack->pt() << "\tEta " << pTrack->eta() 
	   << "\tpx " << pTrack->p().x()
	   << "\tpy " << pTrack->p().y()
	   << "\tpz " << pTrack->p().z()
	   << "\tdEdx " << 1e6*pTrack->probPidTraits().dEdxTruncated() << "\tQ " << (int) pTrack->muHelix().q() << endl;
#endif
      //#if 0
      StPidStatus PiD(gTrack); 
      if (PiD.PiDStatus < 0) continue;
      memset (&Var.refMult, 0, sizeof(Var_t));
      Var.refMult = NoPrimaryTracks;
      Int_t charge = 1;
      Int_t sCharge = 0;
      if (pTrack->muHelix().q() < 0) {charge = -1; sCharge = 1;}
#if 0
      //      Double_t Zs[3] = {dEdxL[0] - zPred[0][kPidPion], dEdxL[1] - zPred[1][kPidPion], dEdxL[2] - zPred[2][kPidPion]};
      const StMuHelix &OuterHelix = gTrack->muOuterHelix();
      StPhysicalHelixD helix = OuterHelix.helix();
      const StThreeVectorF &pOut = OuterHelix.p();
      //      Double_t cpT = gTrack->charge()*pOut.perp();
      Double_t cpT = charge*pTrack->pt();
      Var.cpT = cpT;
      Var.eta = pTrack->eta();
      Double_t phi = TMath::RadToDeg()*pTrack->phi();
      if (phi < 0) phi += 360;
      if (Var.eta < 0) phi -= 360;
      const StThreeVectorF &lastPoint = gTrack->lastPoint();
      Double_t phiD = TMath::RadToDeg()*lastPoint.phi();
      if (phiD < 0) phiD += 360;
      if (Var.eta < 0) phiD -= 360;
      Double_t bg[KPidParticles];
      Double_t bgL10[KPidParticles];
      Double_t zPred[3][KPidParticles];
      Double_t sPred[3][KPidParticles]; // errors versus bg10
      Double_t dEdx[3] = {1e6*pTrack->probPidTraits().dEdxTruncated(), 1e6*pTrack->probPidTraits().dEdxFit(), pTrack->probPidTraits().dNdxFit()};
      if (dEdx[0] <= 0 || dEdx[1] <= 0) continue;
      Double_t dEdxL[3]   = {TMath::Log(dEdx[0]), TMath::Log(dEdx[1]), dEdx[2] > 0 ? TMath::Log(dEdx[2]):0};
      Double_t dEdxL10[3] = {TMath::Log10(dEdx[0]), TMath::Log10(dEdx[1]), dEdx[2] > 0 ? TMath::Log10(dEdx[2]):0};
      Double_t p = pOut.mag(); // gTrack->pt()*TMath::CosH(gTrack->eta());
      Double_t sigmas[3] = {pTrack->probPidTraits().dEdxErrorTruncated(), pTrack->probPidTraits().dEdxErrorFit(), pTrack->probPidTraits().dNdxErrorFit()};
      Double_t nSigmasPi[3] = {0, pTrack->nSigmaPion(), 0};
      Double_t Zs[3] = {dEdxL[0] - zPred[0][kPidPion], sigmas[1]*pTrack->nSigmaPion(), dEdxL[2] - zPred[2][kPidPion]};
#ifdef __dEdxPhi__
      pTPhiPiDL->Fill(Var.cpT,phiD, pTrack->probPidTraits().dEdxTrackLength());
      if (! HFTon) {
	pTPhiPiDLHFToff->Fill(Var.cpT,phiD, pTrack->probPidTraits().dEdxTrackLength());
      }
      cpTh->Fill(Var.cpT);
      Etah->Fill(Var.eta);
      pTEta->Fill(Var.cpT,Var.eta);
      pTPhiT->Fill(Var.cpT,phi);
      pTPhiD->Fill(Var.cpT,phiD);
      for (Int_t l = 0; l < KPidParticles; l++) {
	charge = 1;
	if (l >= kPidHe3) charge = 2;
	bg[l]      = charge*p/Masses[l]; 
	bgL10[l]   = TMath::Log10(bg[l]);
	//	zPred[0][l]   = m_Bichsel->I70Trs  (l,bgL10[l]);
	zPred[0][l]   = TMath::Log(m_Bichsel->GetI70M(bgL10[l]));
	sPred[0][l]   = m_Bichsel->I70TrsS (l,bgL10[l]);
	zPred[1][l]   = m_Bichsel->GetMostProbableZ(bgL10[l]);
	//	zPred[1][l]   = m_Bichsel->IfitTrs (l,bgL10[l]);
	sPred[1][l]   = m_Bichsel->IfitTrsS(l,bgL10[l]);
	zPred[2][l]   = TMath::Log(StdEdxModel::instance()->dNdx(bg[l]));
	sPred[2][l]   = 1;
      }
      pTEtaPiD->Fill(Var.cpT,Var.eta);
      pTPhiPiD->Fill(Var.cpT,phiD);
      pTEtaPiDz->Fill(Var.cpT,Var.eta, Zs[1]);
      pTPhiPiDz->Fill(Var.cpT,phiD, Zs[1]);
      pTPhiPiDsigma->Fill(Var.cpT,phiD, nSigmasPi[1]);
      if (! HFTon) {
	pTEtaPiDHFToff->Fill(Var.cpT,Var.eta);
	pTPhiPiDHFToff->Fill(Var.cpT,phiD);
	pTEtaPiDzHFToff->Fill(Var.cpT,Var.eta, Zs[1]);
	pTPhiPiDzHFToff->Fill(Var.cpT,phiD, Zs[1]);
      }
#endif /* __dEdxPhi__ */
      for (Int_t m = 0; m < 3; m++) {// I70 && Fit && dNdx
	if (sigmas[m] > 0) {
	  Var.hyp = -1;
	  Var.z = Zs[m];
	  TPs[m]->Fill(pTrack->probPidTraits().dEdxTrackLength(), Zs[m]);
	  Pulls[m]->Fill(pTrack->probPidTraits().dEdxTrackLength(), Zs[m]/sigmas[m]);
	  TdEdxs[m]->Fill(TMath::Log10(p), dEdxL10[m]);
#ifdef __dEdxPhi__
	  if (pTrack->probPidTraits().dEdxTrackLength() > 40) {
	    TdEdxs40cm[m]->Fill(TMath::Log10(p), dEdxL10[m]);
	    pTEtaPiD40cm->Fill(Var.cpT,Var.eta);
	    pTPhiPiD40cm->Fill(Var.cpT,phiD);
	    pTEtaPiD40cmz->Fill(Var.cpT,Var.eta, Zs[1]);
	    pTPhiPiD40cmz->Fill(Var.cpT,phiD, Zs[1]);
	    pTPhiPiD40cmsigma->Fill(Var.cpT,phiD, nSigmasPi[1]);
	  }
#endif /* __dEdxPhi__ */
#ifdef __SPARSE__
	  sZs[m]->Fill(&Var.refMult);
	  for (Int_t l = 0; l < KPidParticles; l++) {
	    Var.hyp = l;
	    Double_t zDif = zPred[m][l]-zPred[m][kPidPion];
	    Double_t sigma = sigmas[m]*sPred[m][l];
	    for (Int_t i = 0; i < 100; i++) {
	      Double_t zMC = gRandom->Gaus(zDif,sigma); Var.z = zMC;
	      sZs[m]->Fill(&Var.refMult);
	    }
	  }
#endif /* __SPARSE__ */
	}
      }
#endif
#if 1
      for (Int_t l = kPidElectron; l < KPidParticles; l++) {
	Int_t k = PiD.PiDkeyU3;
	if (PiD.fI70.fPiD) {
	  I70.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.devZ[l]);
	  I70.dev[l][      2]->Fill(PiD.bghyp[l],PiD.devZ[l]);
	  if (Debug) cout << "I70.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.devZ[l] << endl;
	  if (k >= 0) {
	    I70.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.devZ[l]);
	    I70.devT[l][      2]->Fill(PiD.bghyp[l],PiD.devZ[l]);
	    if (Debug) cout << "I70.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.devZ[l] << endl;
	  }
	}
	if (PiD.fFit.fPiD) {
	  fitZ.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.devF[l]);
	  fitZ.dev[l][      2]->Fill(PiD.bghyp[l],PiD.devF[l]);
	  if (Debug) cout << "fitZ.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.devZ[l] << endl;
	  if (k >= 0) {
	    fitZ.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.devF[l]);
	    fitZ.devT[l][      2]->Fill(PiD.bghyp[l],PiD.devF[l]);
	    if (Debug) cout << "fitZ.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.devZ[l] << endl;
	  }
	}
	if (PiD.fdNdx.fPiD) {
	  fitN.dev[l][sCharge]->Fill(PiD.bghyp[l],PiD.devN[l]);
	  fitN.dev[l][      2]->Fill(PiD.bghyp[l],PiD.devN[l]);
	  if (Debug) cout << "fitN.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.devZ[l] << endl;
	  if (k >= 0) {
	    fitN.devT[l][sCharge]->Fill(PiD.bghyp[l],PiD.devN[l]);
	    fitN.devT[l][      2]->Fill(PiD.bghyp[l],PiD.devN[l]);
	    if (Debug) cout << "fitN.dev l = " << l << "\t bg = " << PiD.bghyp[l] << "\tdevZ = " << PiD.devZ[l] << endl;
	  }
	}
      }
#endif
      noGoodPrimTracks++;
    }
    GoodPrimTracks->Fill(noGoodPrimTracks);
  }
  //    iter.Reset(); //ready for next loop                                 
  if (fOut) fOut->Write();
}
#if 0
//________________________________________________________________________________
Double_t MuFcn(Double_t *x, Double_t *p) {
  if (! fFitResults) return 0;
  return fFitResults->Interpolate((x[0] - p[0])/p[1]);
}
//________________________________________________________________________________
Int_t doFit(TH1D *data, TObjArray *mc, 
	    Int_t bin1 = 0, Int_t bin2 = 0, Bool_t doMakePng = kFALSE, Int_t NIter = 3) {
  Int_t ok = -1;
  //	FitParams.ResetParams();
  
  Double_t defaultFraction;
  Double_t defaultStep = 0.01;
  const Int_t      NHypH = mc->GetEntriesFast();
  TFractionFitter *fit = 0;
  for (Int_t iter = 0; iter < NIter; iter++) {// iter = 0 : fit e, p, k, pi; iter = 1 : + fit mu; iter = 2 fit all
    TVirtualFitter *currentFitter  = TVirtualFitter::GetFitter();
    if (currentFitter) delete currentFitter;
    //    TVirtualFitter::SetDefaultFitter("Fumili");
    TVirtualFitter::SetDefaultFitter("Minuit");
    if (fit) delete fit;
    TObjArray mcH;
    Int_t kindex[KPidParticles];
    for (Int_t l = 0; l < KPidParticles; l++) kindex[l] = -1;
    Int_t NL = KPidParticles;
    if (iter == 0) NL = 4;
    if (iter == 1) NL = 5;
    for (Int_t h = 0; h < NHypH; h++) {
      TH1D *mh = (TH1D *) mc->At(h);
      Int_t k = IndexH(mh->GetName());
      assert(k > -1);
      for (Int_t l = 0; l < NL; l++) {
	if (k != l) continue;
	kindex[l] = k;
	mcH.Add(mh);
	break;
      }
    } 
    fit = new TFractionFitter(data, &mcH, "V");     // initialise
    fit->SetRangeX(bin1,bin2);
    TVirtualFitter *fitter = fit->GetFitter();
    fitter->SetMaxIterations(10000);
    Double_t fzero = 1e-20;
    Int_t NH = mcH.GetEntriesFast();
    fit->Constrain(0,fzero,1.0);               // constrain fraction 1 to be between fzero and 1
    for (Int_t l = 0; l < NH; l++) {
      Int_t k = kindex[l];
      assert(k >= 0);
      if (iter == 0) { // fit e, p, k, pi
	if      (k == kPidPion)  defaultFraction = 0.70;
	else if (k <  kPidPion)  defaultFraction = 0.09;
	else                     defaultFraction = 1e-7;
	fitter->SetParameter(l, PidNames[k], defaultFraction, defaultStep, fzero, 1);
	if (defaultFraction < 0.01)  fitter->FixParameter(l);
      } else if (iter == 1) { // + fit mu
	Double_t step = FitParams.ErFrac[k]; // TMath::Sqrt(TMath::Abs(FitParams.CovFrac[k*(k+1)/2 + k]));
	if (step < 1e-7) step = defaultStep;
	Double_t frac =  FitParams.Frac[k];
	if (k == kPidMuon)  frac = 0.03;
	if (frac < 1e-7) frac = 1e-7;
	fitter->SetParameter(l, PidNames[k], frac, step, fzero, 1);
	if (frac <= step && FitParams.Frac[k] > fzero) fitter->FixParameter(l);
	else {
	  if (fitter->IsFixed(l)) fitter->ReleaseParameter(l); 
	}
      } else {// iter = 2: fit all
	Double_t step = FitParams.ErFrac[k]; //TMath::Sqrt(TMath::Abs(FitParams.CovFrac[l*(l+1)/2 + l]));
	if (step < 1e-7) step = defaultStep;
	Double_t frac =  FitParams.Frac[k];
	if (frac < 1e-7) frac = 1e-7;
	fitter->SetParameter(l, PidNames[k], frac, step, fzero, 1);
	if (frac <= step && FitParams.Frac[k] > fzero) fitter->FixParameter(l);
	else {
	  if (fitter->IsFixed(l)) fitter->ReleaseParameter(l); 
	}
      }
    }
    Int_t status = fit->Fit();               // perform the fit
    if (status) {
      // try once more
      status = fit->Fit();
      if (status) {
	// Fix parameters if they are below 1 std
	Int_t nhypfit = fitter->GetNumberTotalParameters();
	for (Int_t l =  0; l < nhypfit; l++) {
	  if (! fitter->IsFixed(l)) {
	    Double_t frac = fitter->GetParameter(l);
	    Double_t erfrac = fitter->GetParError(l);
	    if (frac < erfrac) {
	      Int_t k = kindex[l];
	      fitter->SetParameter(l, PidNames[k], fzero, 0, fzero, 1);
	      //	      fitter->FixParameter(l);
	    }
	  }
	}
	status = fit->Fit();
      }
    }
    if (status) {delete fit; fit = 0; continue;}
    ok = iter;
    FitParams.ResetParams();
    FitParams.Prob =  fit->GetProb();
    cout << "fit status: " << status << " Prob: " << FitParams.Prob << endl;
    if (leg) delete leg;
    Int_t npar = fitter->GetNumberFreeParameters();
    leg = new TLegend(0.6,0.85-0.05*(npar+2),0.9,0.85);
    TH1* result = (TH1*) fit->GetPlot();
    if (! result) continue;
    leg->AddEntry(data,"Data");
    data->Draw("Ep");
    result->SetName(Form("Fit_%s",data->GetName()));
    result->SetMarkerStyle(1);
    result->Draw("samehistl");
    leg->AddEntry(result,Form("Fit prob = %5.3f",FitParams.Prob));
    Double_t dataEntries = data->GetEntries();
    Int_t icolor = 0;
    Int_t ip = -1;
    Int_t nhypfit = fitter->GetNumberTotalParameters();
    for (Int_t l =  0; l < nhypfit; l++) {
      if (fitter->IsFixed(l)) continue;
      TH1 *pred = fit->GetMCPrediction(l);
      ip++;
      if (pred) {
	pred->SetName(((TH1 *)mc->At(l))->GetName());
	FitParams.iter = iter;
	Int_t k = kindex[l]; // IndexH(pred->GetName());
	if (k < 0) continue;
	icolor = k+2;
	if (icolor >= 5) icolor++;
	pred->SetMarkerColor(icolor);
	pred->SetMarkerStyle(1);
	pred->SetLineColor(icolor);
	pred->SetLineStyle(1);
	Double_t frac, erfrac;
	fit->GetResult(l,frac,erfrac);
	Double_t tot = pred->GetEntries();
	Double_t scale = dataEntries/tot*frac;
	pred->Scale(scale);
	pred->Draw("samehistl");
	leg->AddEntry(pred,Form("%5.2f +/- %5.2f %s",100*frac, 100*erfrac,PidNames[k]));
	pred->Write();
	FitParams.Frac[k] = frac;
	FitParams.ErFrac[k] = erfrac;
	Int_t mp = -1;
	for (Int_t m = 0; m <= l; m++) {
	  if (fitter->IsFixed(m)) continue;
	  mp++;
	  Int_t km = kindex[m]; 
	  if (km >= 0) 
	    FitParams.CovFrac[k*(k+1)/2 + km] = fitter->GetCovarianceMatrixElement(ip,mp);
	}
      }
    }
    leg->Draw();
    //	  data->GetListOfFunctions()->Add(leg);
    if (iter == NIter-1) {
      data->Write();
      result->Write();
    }
    if (fFitResults) delete fFitResults;
    fFitResults = (TH1 *) result->Clone();
    delete fit; fit = 0;
#if 1
    TF1   *fmu = new TF1("fmu", MuFcn, -1, 1, 2);
    fmu->SetParName(0,"mu");
    fmu->SetParameter(0,0);
    fmu->SetParName(1,"sigma");
    fmu->SetParameter(1,1);
    fmu->SetParLimits(1,0.5,1.5);
    currentFitter  = TVirtualFitter::GetFitter();
    if (currentFitter) delete currentFitter;
    TVirtualFitter::SetDefaultFitter("Fumili");
    data->Fit(fmu,"0");
    fmu->Draw("samehistl");
    FitParams.mu = fmu->GetParameter(0);
    FitParams.dmu = fmu->GetParError(0);
    FitParams.sigma = fmu->GetParameter(1);
    FitParams.dsigma = fmu->GetParError(1);
    FitParams.ProbMu = fmu->GetProb();
    delete fmu;
#endif
    c1->Modified();
    c1->Update();
#ifndef __APPLE__
    if (doMakePng) {
      TString PngName(data->GetName());
      PngName += ".png";
      TVirtualX::Instance()->WritePixmap(c1->GetCanvasID(),-1,-1,(Char_t *) PngName.Data());
      nPng++;
      cout << "Draw #\t" << nPng << "\t" << PngName.Data() << endl;
      if (! gROOT->IsBatch()) {
	if (Ask()) return ok;
      }
      //	while(!gSystem->ProcessEvents()){}; 
    }
#endif
    FitP->Fill();
  } // iterations
  if (fit) delete fit;
  return ok;
}
//________________________________________________________________________________
void Fraction(const Char_t *name = "sZdEdx", // "sZdEdxFit"
	      Int_t r1 =-1, Int_t r2 = -1, // RefMult, r1 < 0 -> for all, r2 = -1 => NrefMult
	      Int_t i1 = 1, Int_t i2 = -1, // pT
	      Int_t j1 = 1, Int_t j2 = -1, // eta
	      Int_t slide = 1,             // sum over [bin-slide,bin+slide] for pT and eta bins
	      Bool_t doStop = kTRUE,
	      Bool_t doMakePng = kTRUE, Int_t Niter = 3)
{
  TDirectory *fIn = gDirectory;
  THnSparseF *sZdEdx = (THnSparseF *) fIn->Get(name);
  if (! sZdEdx) {
    cout << "sZdEdx historam has not been found. Stop." << endl;
    return;
  }
  Int_t NrefMult = sZdEdx->GetAxis(kRefMult)->GetNbins();
  Int_t NpT  = sZdEdx->GetAxis(kcpT)->GetNbins();
  Int_t Neta = sZdEdx->GetAxis(kEta)->GetNbins();
  if (fOut) fOut->cd();
  else      {
    TString FName(gSystem->BaseName(fIn->GetName()));
    FName.ReplaceAll(".root",name);
    if (r2 < 0) r2 = NrefMult;
    if (r1 < 0) {r1 = r2 = 0;}
    else {
    if (r1 !=        1) FName += Form("R%i",r1);
    if (r2 != NrefMult) FName += Form("R%i",r2);
    }
    if (i2 < 0) i2 = NpT;
    if (i1 < 0) {i1 = i2 = 0;}
    if (i1 !=        1) FName += Form("P%i",i1);
    if (i2 !=      NpT) FName += Form("P%i",i2);
    if (j2 < 0) j2 = Neta;
    if (j1 < 0) {j1 = j2 = 0;}
    if (j1 !=        1) FName += Form("E%i",j1);
    if (j2 !=     Neta) FName += Form("E%i",j2);
    if (slide < 0) slide = 0;
    if (slide != 1) FName += Form("S%i",slide);   
    FName += ".Frac.root";
    fOut = new TFile(FName,"recreate");
    if (! FitP) {
      FitP = new TTree("FitP","Fit results");
      FitP->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
      Int_t bufsize = 64000;
      Int_t split = 99;
      if (split)  bufsize /= 4;
      TTree::SetBranchStyle(1); //new style by default
      FitP_t *FitParamsP = &FitParams;
      TBranch *branch = FitP->Branch("FitPar", "FitP_t", &FitParamsP, bufsize,split);
      branch->SetAutoDelete(kFALSE);
    }
  }
  if (! c1) c1 = new TCanvas();
  else      c1->Clear();
  c1->cd();
  //
  TObjArray       *mc  = 0;
  //  Int_t nPng = 0;
  for (Int_t c = r1; c <= r2; c++) {
    FitParams.c = c;
    TAxis *aXrM = sZdEdx->GetAxis(0);
    if (FitParams.c > 0)  aXrM->SetRange(FitParams.c,FitParams.c);
    FitParams.RefMult_Min = aXrM->GetBinLowEdge(aXrM->GetFirst());
    FitParams.RefMult_Max = aXrM->GetBinUpEdge(aXrM->GetLast());
    FitParams.RefMult     = 0.5*(FitParams.RefMult_Min + FitParams.RefMult_Max);
    for (Int_t ipT = i1; ipT <= i2; ipT++) {
      FitParams.ipT = ipT;
      TAxis *aXpTL = sZdEdx->GetAxis(kcpT);
      if (FitParams.ipT > 0)  aXpTL->SetRange(TMath::Max(1,FitParams.ipT-slide),TMath::Min(NpT,FitParams.ipT+slide));
      FitParams.pT_Min = aXpTL->GetBinLowEdge(aXpTL->GetFirst());
      FitParams.pT_Max = aXpTL->GetBinUpEdge(aXpTL->GetLast());
      FitParams.pT = 0.5*(FitParams.pT_Min + FitParams.pT_Max);
      for (Int_t jeta = j1; jeta <= j2; jeta++) {
	FitParams.jeta = jeta;
	TAxis *aXeta = sZdEdx->GetAxis(kEta);
	//	aXeta->SetRange(TMath::Max(1,FitParams.jeta-1),TMath::Min(Neta,FitParams.jeta+1));
	if (FitParams.jeta > 0) aXeta->SetRange(TMath::Max(1,FitParams.jeta-slide),TMath::Min(Neta,FitParams.jeta+slide));
	FitParams.eta_Min = aXeta->GetBinLowEdge(aXeta->GetFirst());
	FitParams.eta_Max = aXeta->GetBinUpEdge(aXeta->GetLast());
	FitParams.eta = 0.5*(FitParams.eta_Min + FitParams.eta_Max);
	const Char_t *cs = FitParams.pT > 0 ? "+" : "-";
	TString Name(Form("R%i_pT%i_eta%i",FitParams.c,FitParams.ipT,FitParams.jeta));
	TString Title(Form("Data(%s) in range refmult [%4.0f,%4.0f] eta [%4.1f,%4.1f] and pT [%7.3f,%7.3f]", cs,
			   FitParams.RefMult_Min,FitParams.RefMult_Max,
			   FitParams.eta_Min,FitParams.eta_Max,
			   FitParams.pT_Min,FitParams.pT_Max));
	sZdEdx->GetAxis(kHyp)->SetRange(1,1);
	TH1D *data = sZdEdx->Projection(kZ,"E"); 
	if (! data) continue;
	data->SetName(Name);
	data->SetTitle(Title);
	cout << "got Data\t" << data->GetName() << "\t" << data->GetTitle() 
	     << " with " << data->GetEntries() << " entries" 
	     << " Ref: " << FitParams.c << " pT: " << FitParams.ipT << " eta: " << FitParams.jeta
	     << endl;
	Double_t dataEntries = data->GetEntries();
	if (dataEntries < 100) continue;
	Int_t nx = data->GetNbinsX();
	Int_t bin1 = nx;
	Int_t bin2 = 0;
	dataEntries = 0;
	for (Int_t i = 1; i <= nx; i++) {
	  if (data->GetBinContent(i) <= 0) continue;
	  dataEntries += data->GetBinContent(i);
	  if (i < bin1) bin1 = i;
	  if (i > bin2) bin2 = i;
	}
	if (bin1 >= bin2) continue;
	data->GetXaxis()->SetRange(bin1,bin2);
	if (mc) delete mc;
	mc = new TObjArray();        // MC histograms are put in this array
	mc->SetOwner();
	for (Int_t l = 0; l < KPidParticles; l++) {
	//	for (Int_t l = 0; l <= kPidMuon; l++) {
	//	for (Int_t l = 0; l < kPidMuon; l++) {
	  TString NameMc(Form("%s_%s",PidNames[l],Name.Data()));
	  sZdEdx->GetAxis(kHyp)->SetRange(l+2,l+2);
	  TH1D *mch = sZdEdx->Projection(kZ,"E"); 
	  if (! mch) continue;
	  mch->GetXaxis()->SetRange(bin1,bin2);
	  mch->SetName(NameMc);
	  Double_t tot = 0;
	  for (Int_t bin = bin1; bin <= bin2; bin++) {
	    tot += mch->GetBinContent(bin);
	  }
	  if (tot < 100) continue;
	  // check if Mc hitograms are different enough
	  TObjArrayIter next(mc);
	  //	  static const Double_t probCut = 0.01;
	  Double_t prob = 0;
	  TH1D *h = 0;
	  Int_t lh = IndexH(mch->GetName());
	  for (Int_t k = 0; k < l; k++) {
	    //	  while ((h = (TH1D *) next())) {
	    h = (TH1D*) (*mc)[k];
	    if (! h) continue;
	    prob = mch->KolmogorovTest(h,"DM");
	    Int_t kh = IndexH(h->GetName());
	    Int_t ij = kh + lh*(lh+1)/2;
	    FitParams.KolmoD[ij] = prob;
	    //	    if (prob > probCut) break;
	  }
	  //	  if (prob > probCut) continue;
	  cout << "\tgot Mc\t" << mch->GetName() << "\t" << mch->GetTitle() << " with " << tot << " entries" << endl;
	  //	if (l == kPidElectron || l == kPidProton || l == kPidKaon || l == kPidPion) 
	  mc->Add(mch);
	}
	doFit(data, mc, bin1, bin2, doMakePng, Niter);
	if (doStop) {
	  if (! gROOT->IsBatch()) {
	    if (Ask()) return;
	  }
	}
      } // jeta - loop 
    } // ipT - loop 
  } // c - loop
  FitP->Write();
}
#endif
