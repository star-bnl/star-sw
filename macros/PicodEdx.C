/*
  root.exe lMuDst.C 'PicodEdx.C+("/net/l401/data/scratch2/kehw/reco/2019/069/20069033/hlt_20069033_10_01_002.picoDst.root","PicoOut.root")'
  root.exe lMuDst.C 'PicodEdx.C+("/gpfs01/star/pwg_tasks/hf05/Pico/BES-I/AuAu27_production_2011/2011/172/12172013/*.picoDst.root","PicoOut.root")'
  root.exe 'lMuDst.C(0,"/gpfs01/star/pwg_tasks/hf05/Pico/BES-I/AuAu62_productionP10ik/2010/078/11078018/*picoDst.root","PicoOut.root")' PicodEdx.C+
  root.exe 'lMuDst.C(0,"/net/l404/data/fisyak/Pico/2016/125/17125034/st_physics_17125034_raw_5500079.picoDst.root","ldEdxY2,RpicoDst","PicoOut.root")' PicodEdx.C+
  root.exe lMuDst.C 'PicodEdx.C+("/star/subsys/tpc/fisyak/reco/2014/50M/SL15StiCAKFV/130/15130037/st_physics_15130037_raw_3000030_5368_5369.MuDst.root")'
  root.exe PicodEdx_Sparse_pT100_eta24.NewdX.root doFractionFit.C
  
  
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
#define DEBUG
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
#include "StPicoEvent/StPicoEvent.h"
#include "StPicoEvent/StPicoTrack.h"
#define ClassStMessMgr
#define StMessMgr Int_t
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
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
void PicodEdx(const Char_t *files ="./*.picot.root",
	      const Char_t *Out = "PicodEdx.root"){
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
  //         Now iterations
  Long64_t nevent = 9999999;
  for (Long64_t ev = 0; ev < nevent; ev++) {
    if (maker->Make()) break;
    // for Maxim ============================================================
    StPicoDst* pico = maker->picoDst();   // get a pointer to the StPicoDst class, the class that points to all the data
    if (ev%1000 == 0) cout << "Read event\t" << ev << endl;
    StPicoEvent* picoEvent = pico->event(); // get a pointer to the class holding event-wise information
    if (Debug()) {cout << " #" << ev;}
    Int_t NoGlobalTracks = pico->numberOfTracks( );  if (Debug()) {cout << "\tGlobalTracks " << NoGlobalTracks;}
    if (Debug())                                                               {cout << endl;}
    StThreeVectorF pRcVx = picoEvent->primaryVertex();
    Double_t zTpc = pRcVx.z();
    cout << "zTpc = " << zTpc << "\tzVpd = " << picoEvent->vzVpd() << endl;
    Int_t noGoodPrimTracks = 0;
    Bool_t HFTon = kFALSE;
    for (Int_t k = 0; k < NoGlobalTracks; k++) {
      StPicoTrack *gTrack = pico->track(k);
      if (!gTrack) continue;
      //      if (!gTrack->isPrimary()) continue;
      StDcaGeometry dcaG  = gTrack->dcaGeometry();
      KFParticle particle = dcaG.Particle(k);
#ifdef DEBUG
      cout << k << "\t" << particle <<endl;
      cout << "has Pxl1Hit:" <<  gTrack->hasPxl1Hit() << "\tPxl2Hit:" << gTrack->hasPxl2Hit() << "\tIstHit:" <<  gTrack->hasIstHit() << "\tSstHit:" << gTrack->hasSstHit() << endl;
#endif
    }
  }
}


