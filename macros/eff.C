// @(#)root/main:$Name:  $:$Id: eff.C,v 1.2 2005/01/11 15:07:53 fisyak Exp $
#define ASIN
//#define ATANH
#define pTMIN 0.1
#define pTMAX 10.
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iostream.h>
#include "TROOT.h"
#include "TString.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TLeafI.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#else
class TMultiDimFit;
#endif
extern char *Form(const char *fmt, ...);     // format in circular buffer
Char_t *Names[6] = {"p", "K","pi","e","mu","d"};
const Double_t Masses[6] = {0.93827231,
			   0.493677,
			   0.13956995,
			   0.51099907e-3,
			   1.87561339};
TMultiDimFit* fit = 0;
Int_t NMult = 9;
//_______________________________________________________________________________
Double_t EvalMdF(Double_t *x, Double_t *params) {
  Double_t xx[3];
  xx[0] = x[0];
  xx[1] = TMath::Abs(x[1]);
  xx[2] = params[0];
  TVectorD &min = *fit->GetMinVariables();
  TVectorD &max = *fit->GetMaxVariables();
  for (int i=0;i<3;i++) 
    xx[i] = TMath::Max(min(i),TMath::Min(max(i),xx[i]));
#ifdef ASIN
  Double_t theta = TMath::Max(0,TMath::Min(TMath::Pi()/2,fit->Eval(xx)));
  //  Double_t theta = fit->Eval(xx);
  Double_t val = TMath::Sin(theta);
  return val*val;
#else
#ifndef ATANH
  return TMath::Max(0,TMath::Min(1.,fit->Eval(xx)));
#else
  return 0.5*(1. + TMath::TanH(fit->Eval(xx)));
#endif
#endif
  //  return TMath::Min(1., TMath::Max(0, fit->Eval(xx)));
}
//_______________________________________________________________________________
Double_t EvalMdF1(Double_t *x, Double_t *params) {
  Double_t xx[3];
  //  Double_t xm = TMath::Min(1., TMath::Max(0.1, x[0]));
  Double_t xm = TMath::Max(pTMIN,TMath::Min(pTMAX,x[0]));
  xx[0] = TMath::Log(xm);
  xx[1] = params[0];
  xx[2] = params[2];
  Double_t val = EvalMdF(xx,&xx[2]);
  //  printf("x[0]: %f xm: %f xx[2]: %f %f %f val: %f \n",x[0],xm,xx[0],xx[1],xx[2],val);
  return 100.*val;
//   Double_t val = TMath::Sin(fit->Eval(xx));
//   return 100*val*val;
  //  return 100*TMath::Min(1., TMath::Max(0, fit->Eval(xx)));
}
//________________________________________________________________________________
void eff(Char_t *set = "4_14N"){
  //                                     "16N"
  // Global data parameters 
#if 1
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
  }
#endif
  Int_t nVars       = 3;
  Double_t correlationCut = 1.25;
  // make fit object and set parameters on it. 
  //fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
  Int_t mPowers[]   = { 7 , 3, 3};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(10000);
  fit->SetMaxTerms(10);
  fit->SetPowerLimit(25);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);
  TString cname("");
  cname += set;
  cname += "1D";
  TCanvas *c2 = new TCanvas(cname.Data(),cname.Data());//,4);
  c2->SetLogx(1);
  gStyle->SetTitleW(1.0);
  gStyle->SetTitleH(0.05);
  // variables to hold the temporary input data 
  Double_t d;
  Double_t e;
  //  Double_t *x = new Double_t[nVars];
  Double_t x[3];
  // Print out the start parameters
  fit->Print("p");
  Int_t iz, iy, ix;
  Int_t NPoints = 0;
  TString fn(set);
  fn += ".root";
  TH1F *total = 0;
  for (iz = 1; iz <= NMult; iz++) {
    TString Set(Form("pT%02i_",iz)); 
    Set += set; cout << "Set:" << Set.Data() << endl;
    total2D = (TH2F *) gROOT->FindObject(Set.Data());
    if (! total2D) {
      cout << "set " << Set.Data() << " has not been found" << endl;
      continue;
    }
    TAxis *xa = total2D->GetXaxis();
    TAxis *ya = total2D->GetYaxis();
    Int_t nx = xa->GetNbins();
    Int_t ny = ya->GetNbins();
    TH2F *frac2D = new TH2F(*total2D);
    TString Name2D("");
    Name2D += total2D->GetName();
    frac2D->SetName(Name2D.Data());
    TString title("Efficiency for ");
    title += Name2D;
    title += total2D->GetTitle();
    frac2D->SetTitle(title.Data());
    for (iy = 1; iy <= ny; iy++) {
      TString name1(total2D->GetName());
      name1 += Form("_%02i",iy);
      total = (TH1F *) gROOT->FindObject(name1.Data());
      if (! total) continue;
      name1 += "Cut";
      TH1F *totalCut = (TH1F *) gROOT->FindObject(name1.Data());
      if (!totalCut) continue;
      name1.ReplaceAll("Cut","Frac");
      TH1F *frac = new TH1F(*totalCut); 
      frac->Reset();
      frac->SetName(name1.Data()); cout << "Make:" << frac->GetName() << endl;
      for (ix = 1; ix <= nx; ix++) {
	Double_t tot = total->GetBinContent(ix);
	if (tot <= 0.) continue;
	Double_t cut   = totalCut->GetBinContent(ix);
	Double_t value = cut/tot;
	Double_t error = TMath::Sqrt(value*(1.-value)/tot);
	Double_t pT    = xa->GetBinCenter(ix);
	frac->SetBinContent(ix,value);
	frac->SetBinError(ix,error);
	Double_t Eta   = ya->GetBinCenter(iy);
	x[0]           = TMath::Log(pT);
	x[1]           = TMath::Abs(Eta);
	x[2]           = iz;
	NPoints++;
	frac2D->SetCellContent(ix,iy,value);
	frac2D->SetCellError(ix,iy,error);
#ifdef ASIN
	d = TMath::ASin(TMath::Sqrt(value));
	e = 1./(2*tot);
#else
#ifndef ATANH
	d = value;
	e = error*error;
#else
	d = TMath::ATanH(2.*value - 1);
	e = TMath::CosH(d);
	e *= e;
	e *=error;
	e *=e;
#endif
#endif
	fit->AddRow(x,d,e);
      }
    }
  }
 if (NPoints == 0) return;
  cout << "Got " << NPoints << "points" << endl;
  // Print out the statistics
  fit->Print("s");
  
  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
  // We're done 
  //  delete fit;
  cout << "The END" << endl;
}
//____________________________________________________________________________







