/*
  root.exe lMuDst.C 'Pico.C+("/gpfs01/star/pwg_tasks/hf05/Pico/BES-I/AuAu27_production_2011/2011/172/12172013/*.picoDst.root","PicoOut.root")'
  root.exe lMuDst.C 'Pico.C+("/gpfs01/star/pwg/iraklic/Run2018/IB/SL19e/Ru/0-500/*.picoDst.root","PicoOut.root")'
  root.exe  lMuDst.C Pico.C+
  
  PicoDst->Draw("Track->gMom().Phi():Track->gMom().Eta()","Track->isPrimary()&&Track->nHits()>15&&Track->charge()<0","colz")
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
#define __TFG__VERSION__
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
#include "StDcaGeometry.h"
#include "TRSymMatrix.h"
#include "THelixTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoBTofPidTraits.h"
#include "StPicoEvent/StPicoETofPidTraits.h"
#include "StPicoEvent/StPicoTrackCovMatrix.h"
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
void Pico(const Char_t *files ="./*.picoDst.root",
	      const Char_t *Out = "Pico.root"){
#ifndef __CINT__
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
  TH1F *VxZ     = new TH1F("VxZ","Vertex Z",210,-210.,210.);
  TH2F *Vxy     = new TH2F("Vxy","Vertex XY",200,-2.,2.,200,-2.,2.);
  TH2F *VxErrXYmultL = new TH2F("VxErrXYmultL","VxErr_{XY} versus log_{10}(Mult)", 100,0,3, 250, 0, 10.0);
  TH2F *VxErrZmultL = new TH2F("VxErrZmultL","VxErr_{Z} versus log_{10}(Mult)",    100,0,3, 250, 0, 10.0);
  TH2F *EtapT   = new TH2F("EtapT","tracks versus  #eta pT/q",100,-2.5,2.5,100,-5,5.);
  TH1F *PrimMult = new TH1F("PrimMult","No. of total Primary tracks in event",5000,0,5000);
  TH1F *RefMultPos = new TH1F("RefMultPos","Ref. multiplicity the first vertex",500,0,500);
  TH1F *RefMultNeg = new TH1F("RefMultNeg","Ref. multiplicity the first vertex",500,0,500);
  TH1F *GoodPrimTracks = new TH1F("GoodPrimTracks","No. of good Primary track at the first vertex",2000,0,2000);
  TH2F *dcaXYInvpT = new TH2F("dcaXYInvpT","dca_{XY} versus 1/pT", 100,0,10, 500, -2.5, 2.5);
  TH2F *dcaZInvpT = new TH2F("dcaZInvpT","dca_{Z} versus 1/pT", 100,0,10, 500, -2.5, 2.5);
  TH2F *zZ       = new TH2F("zZ","zTpc - zVpd versus zTpc for highest rank vertex", 210, -210, 210, 100, -50, 50);
  TH2F *dEdxP  = new TH2F("dEdxP","dEdx vesus regidity",250,-2.5,2.5,500,0,100);
  TH2F *betaToF  = new TH2F("beta","BToF 1/beta -1 versus regity",350,-3.5,3.5,500,-0.6,4.4);
  TH2F *betaEToF  = new TH2F("Ebeta","EToF 1/beta -1 versus regity",350,-3.5,3.5,500,-0.6,4.4);
#define  __Use_dNdx__
#ifdef     __Use_dNdx__
  enum  {kTotalMethods = 3};
#else
  enum  {kTotalMethods = 1};
#endif
  static StDedxMethod kTPoints[kTotalMethods] = {// {"F","70","FU","70U","N", "NU"};
    kLikelihoodFitId         // F
#ifdef     __Use_dNdx__
    ,kTruncatedMeanId         // 70
    //    ,kWeightedTruncatedMeanId // FU
    //    ,kEnsembleTruncatedMeanId  // 70U
    ,kOtherMethodId           // N
    //    ,kOtherMethodId2          // NU
#endif
  };
  static TH2F *fTdEdx[3][5];
  for (Int_t k = 0; k < kTotalMethods; k++) {
    const Char_t *parN[5] = {"","pi","e","K","P"};
    const Char_t *parT[5] = {"All","|nSigmaPion| < 1","|nSigmaElectron| < 1","|nSigmaKaon| < 1","|nSigmaProton| < 1"};
    const Char_t *FitName[3] = {"F","I70","N"};
    Double_t ymin = 0, ymax = 2.5;
    if (k == 2) {ymin = 1.2; ymax = 4.2;}
    for (Int_t t = 0; t < 1; t++) {//5; t++) {
      TString Title(Form("log10(dE/dx(%s)(keV/cm)) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm %s",FitName[k],parT[t]));
      if (k == 2) Title = Form("log10(dN/dx) versus log10(p(GeV/c)) for Tpc TrackLength > 40 cm %s",parT[t]);
      fTdEdx[k][t] = new TH2F(Form("TdEdx%s%s",FitName[k],parN[t]),Title,
			      350,-1.5,2., 500, ymin, ymax);
      fTdEdx[k][t]->SetMarkerStyle(1);
      fTdEdx[k][t]->SetMarkerColor(t+1);
    }
  } 
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
  TH1F *EtaP     = new TH1F("EtaP","Eta(+) distribution",100,-5.,5.);
  TH1F *EtaN     = new TH1F("EtaN","Eta(-) distribution",100,-5.,5.);
  delete [] etaBins;
  delete [] phiBins;
  delete [] zBins;
  delete [] LBins;
  //#define __fit__
#ifdef __fit__
  Hists2D I70("I70");
  Hists2D fitZ("fitZ");
  Hists2D fitN("fitN");
  static TH2F *Eta[3] = {0};     // 0 -> F, 1 -> 70, 2 -> N
  // TPoints block
  for (Int_t t = 0; t < 3; t++) {
    const Char_t *N[6] = {"F","70","N", "FU","70U","NU"};
    const Char_t *T[6] = {"dEdx(fit)/Pion",
			  "dEdx(I70)/Pion",
			  "dNdx/Pion",
			  "dEdx(fit_uncorrected)/Pion ",
			  "dEdx(I70_uncorrected)/Pion",
			  "dNdx(uncorrected)/Pion"};
    Eta[t] = new TH2F(Form("Eta%s",N[t]),
		      Form("%s for primary tracks versus Eta for |zPV| < 10cm and TpcLength > 40cm, TPC - iTPC",T[t]),
		      100,-2.5,2.5,500,-1.,4.);
  }
#endif
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
    TVector3 pRcVxError = picoEvent->primaryVertexError();
    Double_t xyzV[3] = {pRcVx.x(), pRcVx.y(), pRcVx.z()};
    Double_t zTpc = xyzV[2];
    Double_t vzVpd = picoEvent->vzVpd();
    zZ->Fill(zTpc, zTpc - vzVpd);
    if (vzVpd > -998.9 && TMath::Abs(zTpc - vzVpd) > 10.0) continue;
    VxZ->Fill(pRcVx.z());
    Vxy->Fill(pRcVx.x(),pRcVx.y());
    PrimMult->Fill(NoGlobalTracks);
    RefMultPos->Fill(picoEvent->refMultPos());
    RefMultNeg->Fill(picoEvent->refMultNeg());
    
    // Count no. of good primary tracks
    Int_t noGoodPrimTracks = 0;
    for (Int_t k = 0; k < NoGlobalTracks; k++) {
      StPicoTrack *gTrack = pico->track(k);
      if (!gTrack) continue;
      StPicoTrack *pTrack = gTrack;
      if (!gTrack->isPrimary()) continue;
      StPicoTrackCovMatrix *gCov = pico->trackCovMatrix(k);
      if (! gCov) continue;
      noGoodPrimTracks++;
    }
    GoodPrimTracks->Fill(noGoodPrimTracks);
    VxErrXYmultL->Fill(TMath::Log10(noGoodPrimTracks), pRcVxError.Pt());
    VxErrZmultL->Fill(TMath::Log10(noGoodPrimTracks), pRcVxError.z());
    Bool_t HFTon = kFALSE;
    for (Int_t k = 0; k < NoGlobalTracks; k++) {
      StPicoTrack *gTrack = pico->track(k);
      if (!gTrack) continue;
      StPicoTrack *pTrack = gTrack;
      if (!gTrack->isPrimary()) continue;
      StPicoTrackCovMatrix *gCov = pico->trackCovMatrix(k);
      if (! gCov) continue;
      Int_t charge = gTrack->charge();
      Int_t sCharge = (charge + 1)%2;
      EtapT->Fill(pTrack->pMom().Eta(), charge*pTrack->pMom().Pt());
      StDcaGeometry dcaG  = gCov->dcaGeometry();
      Double_t xyzp[6], CovXyzp[21];
      dcaG.GetXYZ(xyzp,CovXyzp);
      THelixTrack     thelixK =  dcaG.thelix();
      Double_t dca[2], ermx[3];
      thelixK.Dca(xyzV,dca[0],dca[1],ermx,2);
      Double_t pMom = pTrack->pMom().Mag();
      Double_t pTinv = 1./pTrack->pMom().Pt();
      dcaXYInvpT->Fill(pTinv, dca[0]);
      dcaZInvpT->Fill(pTinv, dca[1]);
      KFParticle particle = dcaG.Particle(k);
#ifdef DEBUG
      cout << k << "\t" << particle <<endl;
#endif
      StPidStatus PiD(gTrack); 
      if (PiD.PiDStatus < 0) continue;
      memset (&Var.refMult, 0, sizeof(Var_t));
      Var.refMult = NoGlobalTracks;
      Double_t p = gTrack->gMom().Mag();
      //      
      //      Double_t cpT = gTrack->charge()*pOut.perp();
      Double_t cpT = charge*pTrack->pPt();
      Double_t rigity = charge*p;
      Var.cpT = cpT;
      Var.eta = pTrack->pMom().Eta();
      if (charge > 0) EtaP->Fill(Var.eta);
      else            EtaN->Fill(Var.eta);
      Double_t phi = TMath::RadToDeg()*pTrack->pMom().Phi();
      if (phi < 0) phi += 360;
      if (Var.eta < 0) phi -= 360;
      Double_t zPred[3][KPidParticles];
      Double_t sPred[3][KPidParticles]; // errors versus bg10
      Double_t dEdx[3] = {PiD.fI70.I(), PiD.fFit.I(), PiD.fdNdx.I()};
      if (dEdx[0] <= 0 || dEdx[1] <= 0) continue;
      Double_t dEdxL[3]   = {TMath::Log(dEdx[0]), TMath::Log(dEdx[1]), dEdx[2] > 0 ? TMath::Log(dEdx[2]):0};
      Double_t dEdxL10[3] = {TMath::Log10(dEdx[0]), TMath::Log10(dEdx[1]), dEdx[2] > 0 ? TMath::Log10(dEdx[2]):0};
      Double_t sigmas[3] = {pTrack->dEdxError(), pTrack->dEdxError(), pTrack->dEdxError()};
      Double_t nSigmasPi[3] = {PiD.fI70.D(), PiD.fFit.D(), PiD.fdNdx.D()};
      Double_t Zs[3] = {PiD.fI70.dev[kPidPion], PiD.fFit.dev[kPidPion], PiD.fdNdx.dev[kPidPion]};
      for (Int_t k = 0; k < kTotalMethods; k++) {// I70 && Fit && dNdx
	StDedxMethod m = kTPoints[k];
	if (! PiD.Status(m)) continue;
	if (sigmas[k] > 0) {
	  Var.hyp = -1;
	  Var.z = Zs[k]; //	  if (TPs[m])	  TPs[m]->Fill(pTrack->probPidTraits().dEdxTrackLength(), Zs[m]);
	  //	  if (Pulls[k])	  Pulls[k]->Fill(pTrack->probPidTraits().dEdxTrackLength(), Zs[k]/sigmas[k]);
	  if (k < 2) fTdEdx[k][0]->Fill(TMath::Log10(p), dEdxL10[k]+6);
	  else       fTdEdx[k][0]->Fill(TMath::Log10(p), dEdxL10[k]);
	  if (k == 1 && (
			 (TMath::Abs(p-0.5) < 0.1 && dEdxL10[k]+6 > 1.0 && dEdxL10[k]+6 < 1.5) ||
			 (TMath::Abs(p-1.0) < 0.1 && dEdxL10[k]+6 > 0.5 && dEdxL10[k]+6 < 0.8)
			 )
	      ) {
	    TChain *tc = maker->chain();
	    if (tc) {
	      cout << tc->GetCurrentFile()->GetName() << "\thas abnormal dEdx : p = " << p << "\tdEdxL10 = " << dEdxL10[k]+6 << endl;
	    }
	  }
	  dEdxP->Fill(rigity, 1e6*PiD.fFit.I());
#ifdef __fit__
	  if (pMom >= 0.4 && pMom <= 0.5) {
	    Eta[k]->Fill(pTrack->pMom().Eta(),Zs[k]);
	  }
#endif
	}
      }
#ifdef __fit__
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
      // ToF
      if (gTrack->isTofTrack()) {
	Int_t iTofTrait = gTrack->bTofPidTraitsIndex();
	if (iTofTrait , 0) continue;
	StPicoBTofPidTraits *btofT = pico->btofPidTraits(iTofTrait);
	if (btofT->btofMatchFlag() != 1) continue;
	Float_t beta = btofT->btofBeta();
	if (beta < 0.1) continue;
	betaToF->Fill(rigity, 1./beta - 1); 
      }
      // Etof
      if (gTrack->isETofTrack()) {
	Int_t iEtofTrait = gTrack->eTofPidTraitsIndex();
	if (iEtofTrait , 0) continue;
	StPicoETofPidTraits *etofT = pico->etofPidTraits(iEtofTrait);
	if (etofT->matchFlag() != 1) continue;
	Float_t beta = etofT->beta();
	if (beta < 0.1) continue;
	betaEToF->Fill(rigity, 1./beta - 1); 
      }
    } // track loop
  } // event loop
  if (fOut) fOut->Write();
#endif /* !__CINT__ */
}
