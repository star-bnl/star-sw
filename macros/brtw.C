/*
  root.exe  11p5GeV.I.dEdx.root brtw.C+
 */
//________________________________________________________________________________
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TFile.h"
#include "TStyle.h"
#include "TF1.h"
#include "TCanvas.h"
#include "TSystem.h"
#include "TNtuple.h"
#include "TArrayF.h"
#include "TFitResultPtr.h"
#include "TPaveLabel.h"
#endif
static const Double_t mK  = 0.493677;
static const Double_t mpi = 0.13956995;
static const Double_t mP  = 0.93827231;
static TString        nameP("K_S0");
static       Double_t M1 = mK;
static       Double_t M2 = mpi;
static       Int_t    NoSignals = 1; // 0 -> Scalar, 1 -> Vector, 2 => Tensor;
static       Int_t NoParameters = 3*NoSignals + 4;
static Bool_t Baryon = kFALSE; 
static Bool_t reject = kFALSE;
static const Char_t  *SignalNames[3] = {"S","V","T"};
static Double_t Masses[3] = {0.497611, -1, -1}; // Initail parameters
static Double_t Widths[3] = {0.0107, -1, -1};
static const Char_t  *FuncNames[3] = {"Total","Background","Signal"};
static Bool_t NoBackground = kFALSE;
//________________________________________________________________________________
void SetReject(Bool_t r = kFALSE) {reject = r;}
//________________________________________________________________________________
void SetNoBackground(Bool_t r = kTRUE) {NoBackground = r;}
//________________________________________________________________________________
Double_t pcmax(Double_t m,Double_t m1, Double_t m2){
  Double_t res = 0.0;
  if (m > m1 + m2) {
    res = TMath::Sqrt ((m - m1 + m2)*
		       (m + m1 - m2)*
		       (m - m1 - m2)*
		       (m + m1 + m2))/(2.*m);
  }
  return res;
} 
//________________________________________________________________________________
Double_t BreitWignerF(Double_t Mass,Double_t rstar, Double_t gamma, Int_t L){
  Double_t resol = 0;
  Double_t S1, S2, R, G, GF, brtw = 0;
  S1 = pcmax (Mass,M1,M2);
  if (S1 > 0) { 
    S2 = pcmax (rstar,M1,M2);
    if (S2 > 0) {
      if (Baryon) {
	R = rstar/Mass;
      } else {
      R = (TMath::Power(Mass  + M2,2) - TMath::Power(M1,2))/TMath::Power(Mass,2)/
         ((TMath::Power(rstar + M2,2) - TMath::Power(M1,2))/TMath::Power(rstar,2));
      }
      if (L < 0) {// Gauss
	brtw = TMath::Gaus(Mass ,rstar,gamma, 1);
      } else {
	if (L > 0) {// P- or D- wave
	  G = gamma*R*TMath::Power(S1/S2,2*L + 1);
	  GF = G + resol;
	  brtw = (Mass/S1)*GF/
	    (TMath::Power((Mass - rstar)*(Mass + rstar),2) + TMath::Power(rstar*GF,2));
	}  else { // S-wave
	  GF = gamma   + resol;
	  brtw = (2.*Mass*rstar*GF/TMath::Pi())/
	    (TMath::Power((Mass - rstar)*(Mass + rstar),2) + TMath::Power(rstar*GF,2));
	}
      }
    }
  }
  return brtw;
}
//________________________________________________________________________________
Double_t total(Double_t *x, Double_t *par) {
  Double_t res = 0;
  if (reject) {
    for (Int_t s = 0; s < NoSignals; s++) {
      if (x[0] > par[3*s+1] - 3*par[3*s+2] && x[0] < par[3*s+1] + 3*par[2*s+2]) {TF1::RejectPoint(kTRUE); return res;}
    }
  }
  Double_t signal = 0;
  Double_t background = 0;
  Double_t pq = pcmax(x[0],M1,M2);
  Int_t    L = par[ NoParameters];
  if (pq > 0) {
    for (Int_t s = 0; s < NoSignals; s++) {
      if (par[3*s] > -90) {
	Double_t sL     = par[3*s];// + bglog;
	if (sL > 70) sL = 70;
	if (L > -1) {
	  //	  L = s;
	  signal += TMath::Exp(sL)*BreitWignerF(x[0],par[3*s+1],par[3*s+2],L);
	} else {
	  signal += TMath::Exp(sL)*TMath::Gaus(x[0],par[3*s+1],par[3*s+2],kTRUE);
	}
      } 
    }
    if (par[3*NoSignals] > -90) {
      Double_t sum = par[NoParameters-1];
      for (Int_t i = NoParameters-2; i >= 3*NoSignals + 2; i--) sum += pq*sum + par[i];
      Double_t bglog = par[3*NoSignals+1]*TMath::Log(pq) + pq*sum;
      Double_t s = par[3*NoSignals] + bglog;
      if (s > 700) s = 700;
      background = TMath::Exp(s);
    }
  }
  res = signal +  background;
  return res;
}
//________________________________________________________________________________
TH1F *SubstracF(TH1F *hist, TF1* total, const Option_t *opt="b") {
  if (! hist || ! total) return 0;
  Int_t nx = hist->GetNbinsX();
  TH1F *h = new TH1F(*hist);
  h->SetName(Form("%s%s",hist->GetName(),opt));
  for (Int_t bin = 1; bin <= nx; bin++) {
    Double_t x = hist->GetBinCenter(bin);
    Double_t y = hist->GetBinContent(bin) - total->Eval(x);
    Double_t e = hist->GetBinError(bin);
    h->SetBinContent(bin,y);
    h->SetBinError(bin,e);
  }
  return h;
}
//________________________________________________________________________________
TF1 *brtw(TH1 *hist, Double_t MMin=0.3, Double_t MMax = 1.3, Double_t m1 = mpi, Double_t m2 = mpi, Int_t l = 0, Bool_t baryon = kFALSE) {
  if (! hist) return 0;
  M1 = m1;
  M2 = m2;
  Baryon = baryon;
  static TF1 *f[3] = {0};
  for (Int_t k = 0; k < 3; k++) {// total, signal, background
    TString fName(nameP);
    fName += FuncNames[k];
    if (l >= 0)  fName += l;// cout << "fname = " << fName << endl;
    f[k] = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
    if (! f[k]) f[k] = new TF1(fName,total,MMin,MMax, NoParameters+1);
    f[k]->SetParName(NoParameters,"L");
    f[k]->FixParameter(NoParameters, l);
    f[k]->SetNpx(400);
    if (k == 0) { // Total
      for (Int_t s = 0; s < NoSignals; s++) {
	f[k]->SetParName(3*s  ,Form("lSig%s",SignalNames[s])); 
	f[k]->SetParameter(3*s,0.);
	f[k]->SetParLimits(3*s,-90,90);
	if (l < 0) {
	  f[k]->SetParName(3*s+1,"#mu");   
	  f[k]->FixParameter(3*s+1,Masses[s]);
	  f[k]->SetParName(3*s+2,"#sigma");   
	  f[k]->FixParameter(3*s+2, Widths[s]);
	} else if (l + s < 3) {
	  f[k]->SetParName(3*s+1,Form("M_{%s}",SignalNames[l+s]));   
	  f[k]->FixParameter(3*s+1,Masses[s]);
	  f[k]->SetParName(3*s+2,Form("#Gamma_{%s}",SignalNames[l+s]));   
	  f[k]->FixParameter(3*s+2, Widths[s]);
	} else {
	  f[k]->SetParName(3*s+1,"M");   
	  f[k]->FixParameter(3*s+1,Masses[s]);
	  f[k]->SetParName(3*s+2,"#Gamma");
	  f[k]->FixParameter(3*s+2, Widths[s]);
	}
      }
      f[k]->SetParName(3*NoSignals+0,"lBack");     f[k]->SetParameter(3*NoSignals+0,0);
      f[k]->SetParName(3*NoSignals+1,"#alpha");    f[k]->SetParameter(3*NoSignals+1,0);
      if (MMin - M1 - M2 > 0.010)                  f[k]->FixParameter(3*NoSignals+1,0);
      f[k]->SetParName(3*NoSignals+2,"#beta_{0}");     f[k]->SetParameter(3*NoSignals+2,0);
      
      for (Int_t i = 3*NoSignals+3, j = 1; i < NoParameters; i++, j++) {
	f[k]->SetParName(i,Form("#beta_{%i}",j));
	f[k]->SetParameter(i,0.);}
    } else {
      f[k]->SetLineColor(k+1);
      for (Int_t i = 0; i < NoParameters; i++) {
	f[k]->SetParName(i,f[0]->GetParName(i));
	f[k]->SetParameter(i, f[0]->GetParameter(i));
      }
      if (k == 1) {// Background
	for (Int_t s = 0; s < NoSignals; s++) {
	  f[k]->FixParameter(3*s,-99.);
	  f[k]->FixParameter(3*s+1, f[0]->GetParameter(3*s+1));
	  f[k]->FixParameter(3*s+2, f[0]->GetParameter(3*s+2));
	}
      } else { // Signal
	f[k]->SetParameter(3*NoSignals+1,-99);
	for (Int_t i = 3*NoSignals+2; i < NoParameters; i++) {
	  f[k]->FixParameter(i,0.);}
      }
    }
  }
  TF1 *Total = f[0];
  TF1 *Background = f[1];
  TF1 *Signal = f[2];
  hist->SetAxisRange(MMin, MMax, "X");
  for (Int_t s = 0; s < NoSignals; s++) {
    Total->FixParameter(3*s,-99.);
  }
  TFitResultPtr res;
  if (! NoBackground) {
    SetReject(kTRUE);
    res = hist->Fit(Total,"r","",MMin,MMax);
    SetReject(kFALSE);
  } else {
    for (Int_t i = 3*NoSignals; i < NoParameters; i++) {
      Total->FixParameter(i, -99.);
    }
  }
  for (Int_t s = 0; s < NoSignals; s++) {
    Total->SetParameter(3*s,0.);
    Total->SetParLimits(3*s,-90,90);
    Total->ReleaseParameter(3*s+1);
    Total->ReleaseParameter(3*s+2);
  }
  res = hist->Fit(Total,"r","same",MMin,MMax);
  res = hist->Fit(Total,"rim","same",MMin,MMax);
  Double_t params[20];
  Total->GetParameters(params);
  Signal->SetParameters(params);
  Signal->FixParameter(3*NoSignals,-99);
  Signal->Draw("same");
  Background->SetParameters(params);
  for (Int_t s = 0; s < NoSignals; s++) {
    Background->FixParameter(3*s,-99.);
  }
  Background->Draw("same");
  Double_t binWidth = hist->GetBinWidth(1);
  Double_t S = Signal->Integral(params[1]-3*params[2],params[1]+3*params[2])/binWidth;
  Double_t B = Background->Integral(params[1]-2*params[2],params[1]+2*params[2])/binWidth;
  Double_t T = Total->Integral(params[1]-2*params[2],params[1]+2*params[2])/binWidth;
  
  cout << gSystem->BaseName(gDirectory->GetName()) << "\t";
  cout << hist->GetName() << "\t S = " << S << "\tB = " << B;
  if (B > 0) cout<< "\tS/B = " << S/B;
  cout << "\tS/sqrt(T) = " << S/TMath::Sqrt(T);
  cout << "\tSignificance = " << 1./Total->GetParError(0);
  TH1F *z = (TH1F *) gDirectory->Get("/Particles/KFParticlesFinder/PrimaryVertexQA/z");
  if (z) {
    Double_t nevents = z->GetEntries();
    if (nevents > 0) {
      Double_t SperE = S/nevents;
      cout << "\tSignal per Event(";
      if (nevents < 1000) 
	cout << Form("%7.0f",nevents);
      else if (nevents < 1e6)
	cout << Form("%7.3fK",nevents/1e3);
      else 
	cout << Form("%7.3fM",nevents/1e6);
      cout << ")  = " << SperE;
    }
  }
  cout << Form("\tM = %7.2f +/- %5.2f",1e3*Total->GetParameter(1),1e3*Total->GetParError(1))
       << Form("\tW = %7.2f +/- %5.2f",1e3*Total->GetParameter(2),1e3*Total->GetParError(2)) 
       << " (MeV)" << endl;
  return Total;
}
//________________________________________________________________________________
TF1 *K0BW(TH1F *M) {
  if (! M) return 0;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_BW",M->GetName()));
  Masses[0] = 0.497611; // Initail parameters
  Widths[0] = 0.0107;
  nameP = "K_S0";
  return brtw(m,0.45,0.55,mpi, mpi, 0);
}
//________________________________________________________________________________
TF1 *K0BW(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return 0;
  return K0BW(M);
  TH1F *m = new TH1F(*M);
}
//________________________________________________________________________________
TF1 *K0G(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return 0;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_Gaus",M->GetName()));
  Masses[0] = 0.497611; // Initail parameters
  Widths[0] = 0.0107;
  nameP = "K_S0";
  return brtw(m,0.45,0.55,mpi, mpi, -1);
}
//________________________________________________________________________________
TF1 *LambdaBW(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Lambda/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return 0;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_BW",M->GetName()));
  Masses[0] = 1.115683; // Initail parameters
  Widths[0] = 0.0020;
  nameP = "Lambda";
  NoParameters = 3*NoSignals + 3;
  return brtw(m,1.1,1.2,mpi, mP, 0);
}
//________________________________________________________________________________
TF1 *Lambda(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Lambda/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return 0;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_Gaus",M->GetName()));
  Masses[0] = 1.115683; // Initail parameters
  Widths[0] = 0.0020;
  nameP = "Lambda";
  NoParameters = 3*NoSignals + 3;
  return brtw(m,1.1,1.2,mpi, mP, -1);
}
//________________________________________________________________________________
TF1 *Lambdab(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Lambdab/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return 0;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_Gaus",M->GetName()));
  Masses[0] = 1.115683; // Initail parameters
  Widths[0] = 0.0020;
  nameP = "Lambdab";
  NoParameters = 3*NoSignals + 3;
  return brtw(m,1.1,1.2,mpi, mP, -1);
}
//________________________________________________________________________________
TF1 *phiBW(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/phi_KK/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return 0;
  TH1F *m = new TH1F(*M);
  Masses[0] = 1.020; // Initail parameters
  Widths[0] = 0.004;
  m->SetName(Form("%s_BW",M->GetName()));
  nameP = "phi";
  NoParameters = 3*NoSignals + 8;
  return brtw(m,0.98,1.26,mK, mK, 1);
}
//________________________________________________________________________________
void FitH3(
	   const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/y-#phi-M"
	   //	   const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/y-p_{t}-M" 
	   ) {
  TH3F *h3 = (TH3F *) gDirectory->Get(histN);
  if (! h3) return;
  if (h3->GetDimension() != 3) return;
  TFile *fOut = new TFile("FitH3.root","recreate");
  TString Name(gSystem->BaseName(histN));
  Name += "_z";
  TAxis *xax = h3->GetXaxis();
  Int_t nx = xax->GetNbins();// printf ("nx = %i",nx);
  Axis_t xmin = xax->GetXmin();// printf (" xmin = %f",xmin);
  Axis_t xmax = xax->GetXmax();// printf (" xmax = %f\n",xmax);
  TAxis *yax = h3->GetYaxis();
  Int_t ny = yax->GetNbins();// printf ("ny = %i",ny);
  Axis_t ymin = yax->GetXmin();// printf (" ymin = %f",ymin);
  Axis_t ymax = yax->GetXmax();// printf (" ymax = %f\n",ymax);
  TH2D*    mean    = new TH2D("mean",h3->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
  TH2D*    rms     = new TH2D("rms",h3->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
  TH2D*    entries = new TH2D("entries",h3->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
  TH2D*    mu      = new TH2D("mu",h3->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
  TH2D*    sigma   = new TH2D("sigma",h3->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
  TH2D*    chisq   = new TH2D("chisq",h3->GetTitle(),nx,xmin,xmax,ny,ymin,ymax);
  TF1 *T = 0;
  Float_t *vars = 0;
  TNtuple *FitP = 0;
  TH1D *proj = 0;
  Int_t Npar = 0;
  static TArrayF varsX;
  for (int i = 0; i <= nx; i++){
    for (int j = 0; j <= ny; j++){
      if (i == 0 && j != 0 || j != 0 && j == 0) continue;
      if (i == 0 && j == 0)  proj = h3->ProjectionZ(Form("f%i_%i", i, j )); 
      else                   proj = h3->ProjectionZ(Form("f%i_%i", i, j ),i,i,j,j); 
      if (! T) {
	T =  brtw(proj,0.45,0.55,mpi, mpi, 0);
	Npar = T->GetNpar();
	varsX = TArrayF(2*Npar+6);
	vars = varsX.GetArray();
	TString Vars("i:j:x:y");
	for (Int_t p = 0; p < Npar; p++) {
	  TString V(T->GetParName(p));
	  V.ReplaceAll("{","");
	  V.ReplaceAll("}","");
	  V.ReplaceAll("#","");
	  Vars += ":";
	  Vars += V;
	  Vars += ":d";
	  Vars += V;
	}
	Vars += ":NDF:chisq:mean:rms:entries";
	FitP = new TNtuple("FitP",Form("Fit results for %s",histN),Vars);
      }
      mean->SetBinContent(i,j,proj->GetMean());
      rms->SetBinContent(i,j,proj->GetRMS());
      entries->SetBinContent(i,j,proj->GetEntries());
      if (proj->GetEntries() < 1e2) continue;
      T = brtw(proj,0.45,0.55,mpi, mpi, 0);
      if (! T) continue;
      vars[0] = i; // i
      vars[1] = j; // j
      vars[2] = xax->GetBinCenter(i); // x
      vars[3] = yax->GetBinCenter(j); // y
      Int_t Npar = T->GetNpar();
      for (Int_t p = 0; p < Npar; p++) {
	vars[4+2*p] = T->GetParameter(p)-0.497611;
	vars[5+2*p] = T->GetParError(p);
	if (p == 1) {
	  mu->SetBinContent(i,j,vars[4+2*p]);
	  mu->SetBinError(i,j,vars[5+2*p]);
	} else if (p == 2) {
	  sigma->SetBinContent(i,j,T->GetParameter(p));
	  sigma->SetBinError(i,j,T->GetParError(p));
	}
      }
      vars[4+2*Npar] = T->GetNDF();
      vars[5+2*Npar] = T->GetChisquare();
      chisq->Fill(i,j, T->GetChisquare());
      vars[6+2*Npar] = proj->GetMean();
      vars[7+2*Npar] = proj->GetRMS();
      vars[8+2*Npar] = proj->GetEntries();
      FitP->Fill(vars);
    }
  }
  fOut->Write();
}
//________________________________________________________________________________
void Drawf0_0() {
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  Int_t NF = 0;
  TIter next(files);
  TFile *f = 0;
  while ((f = (TFile *) next())) {
    TH1 *f0_0 = (TH1 *) f->Get("f0_0");
    if (! f0_0) continue;
    FitFiles[NF] = f;
    NF++;
  }
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","Reconstructed Mass");
  c1->Clear();
  c1->Divide(1,NF);
  for (Int_t i = 0; i < NF; i++) {
    f = FitFiles[i];
    c1->cd(i+1);
    TH1 *f0_0 = (TH1 *) f->Get("f0_0");
    f0_0->Draw("e");
    TString Name(f->GetName());
    Name.ReplaceAll("FitH3.root","");
    TPaveLabel *pl = new TPaveLabel(0.2,0.7,0.4,0.9,Name,"NDCbr");
    pl->Draw();
  }
}
//________________________________________________________________________________
void brtw() {
  const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/y-#phi-M";
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    f->cd();
    TString F(f->GetName());
    TH3F *h3 = (TH3F *) gDirectory->Get(histN);
    if (! h3) continue;;
    TH1D *h1 = (TH1D *) h3->Project3D("z");
    K0BW(h1->GetName());
  }
}
//________________________________________________________________________________
/*
  11p5GeV.H4.devdEdx.root:/Particles/KFParticlesFinder/Particles/Ks/Parameters
  root.exe *H3.root
  
 */



