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
static const Double_t mK = 0.493677;
static const Double_t mpi = 0.13956995;
static const Double_t M1 = mK;
static const Double_t M2 = mpi;
static const Int_t    L = -1; 
static const Int_t NoParameters = 20;
static TCanvas *cFit = 0;
static Bool_t reject;
static Bool_t draw;
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
Double_t BreitWignerF(Double_t Mass,Double_t rstar, Double_t gamma){
  Double_t resol = 0;
  Double_t S1, S2, R, G, GF, brtw = 0;
  S1 = pcmax (Mass,M1,M2);
  if (S1 > 0) { 
    S2 = pcmax (rstar,M1,M2);
    if (S2 > 0) {
#ifndef BARYON
      R = rstar/Mass;
#else
      R = (TMath::Power(Mass  + M2,2) - TMath::Power(M1,2))/TMath::Power(Mass,2)/
         ((TMath::Power(rstar + M2,2) - TMath::Power(M1,2))/TMath::Power(rstar,2));
#endif
      if (L < 0) {// Gauss
	brtw = TMath::Gaus(Mass ,rstar,gamma, 1);
      } else {
	if (L > 0) {
	  G = gamma*R*TMath::Power(S1/S2,2*L + 1);
	  GF = G + resol;
	  brtw = (Mass/S1)*GF/
	    (TMath::Power((Mass - rstar)*(Mass + rstar),2) + TMath::Power(rstar*GF,2));
	}
	else {
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
Double_t func(Double_t *x, Double_t *par) {
  Double_t res = 0;
  //  if (! draw && x[0] > 0.65 && x[0] < 0.75)  {TF1::RejectPoint(); return res;}
  if (reject && x[0] > par[1] - 3*par[2] && x[0] < par[1] + 3*par[2]) {TF1::RejectPoint(); return res;}
  Double_t signal = 0;
  Double_t background = 0;
  Double_t pq = pcmax(x[0],M1,M2);
  if (pq > 0) {
    if (L >= 0) {
      Double_t sum = par[NoParameters-1];
      for (Int_t i = NoParameters-2; i >= 8; i--) sum += pq*sum + par[i];
      Double_t bglog = par[7]*TMath::Log(pq) + pq*sum;
      if (par[0] > -90) {
	Double_t s     = par[0];// + bglog;
	if (s > 700) s = 700;
	signal += TMath::Exp(s)*BreitWignerF(x[0],par[1],par[2]);
      }
      if (par[3] > -90) {
	Double_t s     = par[3];// + bglog;
	if (s > 700) s = 700;
	signal += TMath::Exp(s)*BreitWignerF(x[3],par[4],par[5]);
      }
      if (par[6] > -90) {
	Double_t s = par[6] + bglog;
	if (s > 700) s = 700;
	background = TMath::Exp(s);
      }
    } else { // L < 0
      if (par[0] > -90) signal += TMath::Exp(par[0])*BreitWignerF(x[0],par[1],par[2]);
      if (par[3] > -90) signal += TMath::Exp(par[3])*BreitWignerF(x[0],par[4],par[5]);
      if (par[6] > -90) {
	Double_t sum = par[NoParameters-1];
	for (Int_t i = NoParameters-2; i > 6; i--) sum += pq*sum + par[i];
	background = TMath::Exp(par[6] + sum);
      }
    }
    res = signal +  background;
    //    printf ("x: %f signal: %f background: %f\n",x[0],signal,background); 
  }
  return res;
}
//________________________________________________________________________________
TH1D *SubstracF(TH1D *hist, TF1* func, const Option_t *opt="b") {
  if (! hist || ! func) return 0;
  Int_t nx = hist->GetNbinsX();
  TH1D *h = new TH1D(*hist);
  h->SetName(Form("%s%s",hist->GetName(),opt));
  for (Int_t bin = 1; bin <= nx; bin++) {
    Double_t x = hist->GetBinCenter(bin);
    Double_t y = hist->GetBinContent(bin) - func->Eval(x);
    Double_t e = hist->GetBinError(bin);
    h->SetBinContent(bin,y);
    h->SetBinError(bin,e);
  }
  return h;
}
//________________________________________________________________________________
void brtw(TH1D *hist, Double_t MMin=1.6, Double_t MMax = 2.2, Int_t Np = 10) {
  TF1 *Func = (TF1 *) gROOT->GetListOfFunctions()->FindObject("Func");
  if (! Func) Func = new TF1("Func",func,MMin,MMax, NoParameters);
  TF1 *Background = (TF1 *) gROOT->GetListOfFunctions()->FindObject("Background");
  if (! Background)  Background = new TF1("Background",func,MMin,MMax, NoParameters);
  TF1 *Signal = (TF1 *) gROOT->GetListOfFunctions()->FindObject("Signal");
  if (! Signal) Signal = new TF1("Signal",func,MMin,MMax, NoParameters);
  TF1 *f[3] = {Func, Background, Signal};
  for (Int_t k = 0; k < 3; k++) {
    f[k]->SetParName(0,"logSignal"); f[k]->SetParameter(0,0.);
    if (k == 1)                      f[k]->FixParameter(0,-99.);
    else                             f[k]->SetParLimits(0,-90,90);
    f[k]->SetParName(1,"Mass");      f[k]->FixParameter(1,0.4977);// K0s 1.86044); // D0
    f[k]->SetParName(2,"Width");     f[k]->FixParameter(2,0.01072); // D0
    f[k]->SetParName(3,"logSigRef"); f[k]->SetParameter(3,0.);
#if 0
    if (k == 1)                      f[k]->FixParameter(3,-99.);
    else                             f[k]->SetParLimits(3,-90,90);
#else
    f[k]->FixParameter(3,-99.);
#endif
    f[k]->SetParName(4,"MassRef");   f[k]->FixParameter(4,1.86044);// D0
    f[k]->SetParName(5,"WidthRef");  f[k]->FixParameter(5,0.087);  // D0
    f[k]->SetParName(6,"BG");        f[k]->SetParameter(6,1);
    if (k == 2)                      f[k]->SetParameter(6,-99.);
    else                             f[k]->SetParLimits(6,-90,90);
    f[k]->SetParName(7,"alpha");     f[k]->SetParameter(7,0);
    f[k]->SetParName(8,"beta0");     f[k]->SetParameter(8,0);
    for (Int_t i = 9; i < NoParameters; i++) {
      f[k]->SetParName(i,Form("beta%i",i-8));
      if (i > Np ) f[k]->FixParameter(i,0);
      else        {f[k]->ReleaseParameter(i); f[k]->SetParameter(i,0.);}
    }
  }
  if (! hist) return;
  TVirtualPad* savepad = gPad;
  if (! cFit) cFit = new TCanvas("Fit","Fit results");
  else      cFit->Clear();
  hist->SetAxisRange(MMin, MMax, "X");
  cFit->Divide(1,2);
  reject = kTRUE;
  draw = kFALSE;
  cFit->cd(1);
  
  Background->FixParameter(0,-99.);
  Background->FixParameter(3,-99.);
  Background->SetLineColor(2);
  hist->Fit(Background,"r","",MMin,MMax);
  reject = kFALSE;
  draw = kTRUE;
#if 0
  TH1D *h = SubstracF(hist,Background);
  h->Draw();
#endif
  Double_t params[20];
  Background->GetParameters(params);
  Func->SetParameters(params);
  Func->ReleaseParameter(0); Func->SetParameter(0,0.1); Func->SetParLimits(0,-90,90);
  Func->ReleaseParameter(3); Func->SetParameter(3,0.1); Func->SetParLimits(3,-90,90);
  //    Func->ReleaseParameter(1);
  //    Func->ReleaseParameter(2);
  //    cFit->cd(2);
  draw = kFALSE;
  hist->Fit(Func,"r","",MMin,MMax);
  cout << hist->GetName();
  if (Func->GetParameter(0) < -90 || 
      Func->GetParError(0) <= 0 || 
      Func->GetParError(0) > 1) cout << "\tNo signal" << endl;
  else         cout << "\t" << 1./Func->GetParError(0) << " std Signal" << endl;
  Background->Draw("same");
  Func->GetParameters(params);
  Func->SetParameter(0,-99);
  Func->SetParameter(3,-99);
  draw = kTRUE;
  TH1D *h = SubstracF(hist,Func,"r");
  cFit->cd(2);
  h->Draw();
  //    Func->SetParameters(params);
  Signal->SetParameter(0,params[0]);
  Signal->SetParameter(3,params[3]);
  //  Signal->SetParameter(3,-99);
  Signal->Draw("same");
  if (savepad) savepad->cd();
}
