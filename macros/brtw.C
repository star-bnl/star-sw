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
#include "TStyle.h"
#include "TF1.h"
#include "TCanvas.h"
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
static TCanvas *cFit = 0;
static Bool_t reject = kFALSE;
static const Char_t  *SignalNames[3] = {"S","V","T"};
static Double_t Masses[3] = {0.4977, -1, -1}; // Initail parameters
static Double_t Widths[3] = {0.0107, -1, -1};
static const Char_t  *FuncNames[3] = {"Total","Background","Signal"};
//________________________________________________________________________________
void SetReject(Bool_t r = kFALSE) {reject = r;}
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
void brtw() {
}
//________________________________________________________________________________
void brtw(TH1F *hist, Double_t MMin=0.3, Double_t MMax = 1.3, Double_t m1 = mpi, Double_t m2 = mpi, Int_t l = 0, Bool_t baryon = kFALSE) {
  if (! hist) return;
  M1 = m1;
  M2 = m2;
  Baryon = baryon;
  static TF1 *f[3] = {0};
  for (Int_t k = 0; k < 3; k++) {// total, signal, background
    TString fName(nameP);
    fName += FuncNames[k];
    if (l >= 0)  fName += l;// cout << "fname = " << fName << endl;
    f[k] = (TF1 *) gROOT->GetListOfFunctions()->FindObject(fName);
    if (f[k]) continue;
    f[k] = new TF1(fName,total,MMin,MMax, NoParameters+1);
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
	} else {
	  f[k]->SetParName(3*s+1,Form("Mass%s",SignalNames[s]));   
	  f[k]->FixParameter(3*s+1,Masses[s]);
	  f[k]->SetParName(3*s+2,Form("Width%s",SignalNames[s]));   
	  f[k]->FixParameter(3*s+2, Widths[s]);
	}
      }
      f[k]->SetParName(3*NoSignals+0,"lBack");     f[k]->SetParameter(3*NoSignals+0,0);
      f[k]->SetParName(3*NoSignals+1,"alpha");     f[k]->SetParameter(3*NoSignals+1,0);
      f[k]->SetParName(3*NoSignals+2,"beta0");     f[k]->SetParameter(3*NoSignals+2,0);
      
      for (Int_t i = 3*NoSignals+3, j = 1; i < NoParameters; i++, j++) {
	f[k]->SetParName(i,Form("beta%i",j));
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
  SetReject(kTRUE);
  hist->Fit(Total,"r","",MMin,MMax);
  SetReject(kFALSE);
  for (Int_t s = 0; s < NoSignals; s++) {
    Total->SetParameter(3*s,0.);
    Total->SetParLimits(3*s,-90,90);
    Total->ReleaseParameter(3*s+1);
    Total->ReleaseParameter(3*s+2);
  }
  hist->Fit(Total,"r","same",MMin,MMax);
  hist->Fit(Total,"rim","same",MMin,MMax);
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
  cout << hist->GetName() << "\t S = " << S << "\tB = " << B << "\tS/B = " << S/B << "\tS/sqrt(T) = " << S/TMath::Sqrt(T);
  TH1F *z = (TH1F *) gDirectory->Get("/Particles/KFParticlesFinder/PrimaryVertexQA/z");
  if (z) {
    Double_t nevents = z->GetEntries();
    if (nevents > 0) {
      Double_t SperE = S/nevents;
      cout << "\tSignal per Event(" << nevents << ")  = " << SperE;
    }
  }
  cout << "\tM = " << Total->GetParameter(1) << " +/- " << Total->GetParError(1) 
       << "\tW = " << Total->GetParameter(2) << " +/- " << Total->GetParError(2) 
       << endl;
}
//________________________________________________________________________________
void K0BW(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_BW",M->GetName()));
  Masses[0] = 0.4977; // Initail parameters
  Widths[0] = 0.0107;
  nameP = "K_S0";
 brtw(m,0.45,0.55,mpi, mpi, 0);
}
//________________________________________________________________________________
void K0G(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Ks/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_Gaus",M->GetName()));
  Masses[0] = 0.4977; // Initail parameters
  Widths[0] = 0.0107;
  nameP = "K_S0";
  brtw(m,0.55,0.55,mpi, mpi, -1);
}
//________________________________________________________________________________
void Lambda(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Lambda/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_Gaus",M->GetName()));
  Masses[0] = 1.115683; // Initail parameters
  Widths[0] = 0.0020;
  nameP = "Lambda";
  NoParameters = 3*NoSignals + 3;
  brtw(m,1.1,1.2,mpi, mP, -1);
}
//________________________________________________________________________________
void Lambdab(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/Lambdab/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return;
  TH1F *m = new TH1F(*M);
  m->SetName(Form("%s_Gaus",M->GetName()));
  Masses[0] = 1.115683; // Initail parameters
  Widths[0] = 0.0020;
  nameP = "Lambdab";
  NoParameters = 3*NoSignals + 3;
  brtw(m,1.1,1.2,mpi, mP, -1);
}
//________________________________________________________________________________
void phiBW(const Char_t *histN = "/Particles/KFParticlesFinder/Particles/phi_KK/Parameters/M") {
  TH1F *M = (TH1F *) gDirectory->Get(histN);
  if (! M) return;
  TH1F *m = new TH1F(*M);
  Masses[0] = 1.020; // Initail parameters
  Widths[0] = 0.004;
  m->SetName(Form("%s_BW",M->GetName()));
  nameP = "phi";
  NoParameters = 3*NoSignals + 8;
  brtw(m,0.98,1.26,mK, mK, 1);
}
//________________________________________________________________________________
/*
  11p5GeV.H4.devdEdx.root:/Particles/KFParticlesFinder/Particles/Ks/Parameters
 */
