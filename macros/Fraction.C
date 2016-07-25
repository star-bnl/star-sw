// @(#)root/main:$Name:  $:$Id: Fraction.C,v 1.2 2005/01/11 15:07:51 fisyak Exp $
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
Int_t FindHists(TH2* total2D, Int_t iy, TH1 *total, TH1 **fHists, Int_t fNHyps) {
  if (!total2D) return -1;
  Char_t name1[20];
  sprintf(name1,"%s_%02i",total2D->GetName(),iy);
  TH1 *total = (TH1 *) gROOT->FindObject(name1);
  if (! total) return -1;
  for (Int_t jh = 1; jh <= fNHyps; jh++) {
    TString Name(Names[jh-1]);
    Name += total->GetName();
    fHists[jh] = (TH1 *) gROOT->FindObject(Name.Data());
    //    if (! fHists[jh]) continue;
    for (Int_t kh = 1; kh <= jh; kh++){
      Int_t ijh = fNHyps + (jh-1)*jh/2 + kh;
      Name   = Names[jh-1];
      Name  += Names[kh-1];
      Name  += total->GetName();
      fHists[ijh] = (TH1 *) gROOT->FindObject(Name.Data());
    }
  }
  return 0;
}
//________________________________________________________________________________
void Fraction(Int_t ih=3, Char_t *set = "4_14N", Int_t Nhyps=4){
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
  Double_t correlationCut = 1.09;
  // make fit object and set parameters on it. 
  //fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
  Int_t mPowers[]   = { 3 , 3, 3};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(10000);
  fit->SetMaxTerms(10);
  fit->SetPowerLimit(25);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);
  TString cname(Names[ih-1]);
  cname += set;
//   TCanvas *c1 = new TCanvas(cname.Data(),cname.Data(),10,10,600,800);
//   c1->Divide(3,3);
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
  Int_t fNHyps = Nhyps;
  TH1 **fHists  = new TH1*[1+fNHyps+fNHyps*(fNHyps+1)/2];
  TH2F *total2D = 0;
  TH1F *total = 0, *frac = 0;
  //  TFile *output = new TFile(fn.Data(),"recreate");
  TString FSET(set);
  FSET += Names[ih-1];
  TString FW(FSET);
  FW += ".h";
  FILE *fp = fopen(FW.Data(),"w");
  fprintf(fp,"static Double_t %s[%i][][] = {\n",FSET.Data(),NMult);
  FW = "e";
  FW += FSET;
  FW += ".h";
  FILE *fe = fopen(FW.Data(),"w");
  fprintf(fe,"static Double_t e%s[%i][][] = {\n",FSET.Data(),NMult);
  for (iz = 0; iz <= NMult; iz++) {
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
    TString Name2D(Names[ih-1]);
    Name2D += total2D->GetName();
    frac2D->SetName(Name2D.Data());
    TString title("Faction for ");
    title += Name2D;
    title += total2D->GetTitle();
    frac2D->SetTitle(title.Data());
    for (iy = 1; iy <= ny; iy++) {
      if (FindHists(total2D,iy,total,fHists,fNHyps)) continue;
      frac = (TH1F*)fHists[ih]; 
      if (frac) {
	TH1F *smooth = new TH1F(*frac);
	smooth->Smooth(100);
	//	cout << "Get Fraction from " << frac->GetName() 
	//<< "  " << frac->GetTitle() << endl;
	fprintf(fp,"{");
	fprintf(fe,"{");
	for (ix = 1; ix <= nx; ix++) {
	  Double_t error = frac->GetBinError(ix)/100.;
	  Double_t value = frac->GetBinContent(ix)/100.;
	  if (ix > 1) {
	    fprintf(fp,",");
	    fprintf(fe,",");
	  }
	  Double_t pT    = xa->GetBinCenter(ix);
	  Double_t Eta   = ya->GetBinCenter(iy);
	  Double_t p = pT*TMath::CosH(Eta);
	  //	  if (pT < 0.25) continue;
	  //	  if (pT*TMath::CosH(Eta) > 1.3) continue;
	  x[0]           = TMath::Log(pT);
	  x[1]           = TMath::Abs(Eta);
	  x[2]           = iz;
#if 0
	  Double_t predI =  BetheBloch::Sirrf(p/Masses[ih-1]);
#endif
	  Int_t ok = 1;
	  Double_t corFact = 1.;
	  Double_t diffMin = 999.;
	  for (Int_t kh = 1; kh <= fNHyps; kh++){
	    if (ih == kh) continue;
	    Int_t ijh;
	    if (kh <= ih) ijh = fNHyps + (ih-1)*ih/2 + kh;
	    else          ijh = fNHyps + (kh-1)*kh/2 + ih;
	    Double_t correlation = fHists[ijh]->GetBinContent(ix);
#if 0
	    Double_t predK =  BetheBloch::Sirrf(p/Masses[kh-1]);
	    Double_t diff = TMath::Log(predI/predK);
	    if (TMath::Abs(diff) < TMath::Abs(diffMin)) diffMin = diff;
	    cout << NPoints << "\tpT :\t" << pT << "\tEta : \t" << x[1] << "\tMult \t" << x[2] 
		 << "\tFraction : \t" << value << "\t+/-\t" << error 
		 << "\tcorr \t" << correlation << "\t BB diff " << diff << endl;
#endif
 	    corFact += correlation*correlation;
	  }
	  Double_t smo   = smooth->GetBinContent(ix)/100.;
	  Double_t sdev = -999;
	  if (error > 0.) sdev = (smo - value)/error;
	  NPoints++;
	  TString mess("Accepted");
	  Int_t iok = 0;
	  if (corFact > correlationCut) {mess = "Correlation"; iok = 4;}
	  if (p > 0.6) {
	    if (pT < pTMIN || pT > pTMAX) {mess = "pT out"; iok = 2;}
	    if (error > 0.5*value) {mess = "Value"; iok = 3;}
	    if (TMath::Abs(sdev) > 10.) {mess = "Smoother"; iok = 5;}
	  }
	  if (error < 1.e-7) {mess = "Error"; iok = 1;}
	  if (iok) {
	    cout << NPoints << "\t" << mess.Data() <<"\tpT :\t" 
		 << x[0] << "\tEta : \t" << x[1] << "\tMult \t" << x[2] 
		 << "\tFraction : \t" << value << "\t+/-\t" << error << " \tcorFact \t" << corFact 
		 << "\tsdev : \t" << sdev << endl;
	    fprintf(fp,"0");
	    fprintf(fe,"0");
	    continue;
	  }
	  fprintf(fe,"%6.4f",error);
	  frac2D->SetCellContent(ix,iy,value);
	  frac2D->SetCellError(ix,iy,error);
#ifdef ASIN
	  d = TMath::ASin(TMath::Sqrt(value));
	  if (value >= 1. || value <= 1.e-7) e = 1.;
	  else e = error*error/(4.*(value*(1.-value)));//*corFact;
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
	delete smooth;
      }
      fprintf(fp,"},\n");
      fprintf(fe,"},\n");
    }
  }
  fprintf(fp,"};\n");
  fprintf(fe,"};\n");
  fclose(fp);
  fclose(fe);
  
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
  //}
  //void Print(Int_t ih=3, Char_t *set = "16N", Int_t Nhyps=4){
  //  Int_t iz, iy, ix;
  Int_t n = 0, nbad = 0;
  Double_t chisq = 0;
  for (iz = 0; iz <= NMult; iz++) {
    TString Set(Form("pT%02i_",iz)); 
    Set += set; cout << "Set:" << Set.Data() << endl;
    TH2F *total2D = (TH2F *) gROOT->FindObject(Set.Data());
    if (! total2D) {
      cout << "set " << Set.Data() << " has not been found" << endl;
      continue;
    }
    TAxis *xa = total2D->GetXaxis();
    TAxis *ya = total2D->GetYaxis();
    Int_t nx = xa->GetNbins();
    Int_t ny = ya->GetNbins();
    Int_t fNHyps = Nhyps;
    TString Eval("MDFEval");
    TString Name2D(Names[ih-1]);
    Name2D += total2D->GetName();
    TH2F *frac2D= (TH2F *) gROOT->FindObject(Name2D.Data());
    if (!frac2D) continue;
    Eval += frac2D->GetName();
    TF2 *mdfEval = new TF2(Eval.Data(),
			   EvalMdF,xa->GetXmin(),xa->GetXmax(),
			   ya->GetXmin(),ya->GetXmax(),1);
    mdfEval->SetParameter(0,iz);
//     TF2 *fnew2 = new TF2();
//     mdfEval->Copy(*fnew2);
//     frac2D->GetListOfFunctions()->Add(fnew2);
//     fnew2->SetParent(frac2D);
//     c1->cd(iz);
//     gPad->SetLogx(1);
//     frac2D->SetStats(0);
//     frac2D->Draw("colz");
//     mdfEval->Draw("same");
//     c1->Update();
//    mdfEval->Draw("same");
    for (iy = 1; iy <= ny; iy++) {
      if (FindHists(total2D,iy,total,fHists,fNHyps)) continue;
      Double_t Eta   = ya->GetBinCenter(iy);
      Double_t xx[3];
      xx[1]           = TMath::Abs(Eta);
      xx[2]           = iz;
      for (ix = 1; ix <= nx; ix++) {
	Double_t pT    = xa->GetBinCenter(ix);
	if (pT < pTMIN || pT > pTMAX) continue;
	xx[0]           = TMath::Log(pT);
	Double_t value = frac2D->GetCellContent(ix,iy);
	Double_t error = frac2D->GetCellError(ix,iy);
	if (error <= 2.e-3) continue;
	if (value <= 2.e-3) continue;
	Double_t corFact = 1.;
	for (Int_t kh = 1; kh <= fNHyps; kh++){
	  if (ih == kh) continue;
	  Int_t ijh;
	  if (kh <= ih) ijh = fNHyps + (ih-1)*ih/2 + kh;
	  else          ijh = fNHyps + (kh-1)*kh/2 + ih;
	  Double_t correlation = fHists[ijh]->GetBinContent(ix);
	  corFact += correlation*correlation;
	}
	if (corFact > correlationCut) continue;
	Double_t par[1];
	par[0] = iz;
	Double_t pred = EvalMdF(xx,par);
	Double_t dev  = (value - pred)/error;
	n++; chisq += dev*dev;
	if (TMath::Abs(dev) > 3.0) {
	  Double_t corrMax = 0;
	  Double_t kBad = 0;
	  for (Int_t kh = 1; kh <= fNHyps; kh++){
	    if (ih == kh) continue;
	    Int_t ijh;
	    if (kh <= ih) ijh = fNHyps + (ih-1)*ih/2 + kh;
	    else          ijh = fNHyps + (kh-1)*kh/2 + ih;
	    Double_t correlation = fHists[ijh]->GetBinContent(ix);
	    if (TMath::Abs(correlation) > TMath::Abs(corrMax)) {
	      corrMax = correlation;
	      kBad = kh-1;
	    }
	  }
	  //	  if (TMath::Abs(corrMax) > 0.2) continue;
	  nbad++;
	  cout << n << "/" << nbad << "\tpT = " << pT << "\tEta = "  << Eta << "\tMult" << iz 
	       << "\tval = " << value << "\t+/- " << error 
	       << "\tpred = " << pred << "\tdev = " << dev 
	       << "\tCorrMax = " << corrMax << " " << Names[kBad] 
	       << endl;
	}
      }
      frac = (TH1F*) fHists[ih]; 
      if (!frac) continue;
      TString Eval1 = Eval;
      Eval1 += "1D";
      TF1 *mdfEval1 = new TF1(Eval1.Data(),
			      EvalMdF1,xa->GetXmin(),xa->GetXmax(),2);
      mdfEval1->SetParameter(0,xx[1]);
      mdfEval1->SetParameter(1,xx[2]);
      TF1 *fnew1 = new TF1();
      mdfEval1->Copy(*fnew1);
      frac->GetListOfFunctions()->Add(fnew1);
      fnew1->SetParent(frac);
      c2->cd();
      frac->SetStats(0);
      frac->Draw();
      c2->Update();
    }
  }
  cout << n << "/" << nbad << " points chisq = " <<  chisq << endl;
  // Write code to file 
  //   TString nameC(frac2D->GetName());
  //   nameC += ".C";
  //   fit->MakeCode(nameC.Data());
  fit->MakeMethod(frac2D->GetName());
#if 0
  // Write histograms to disk, and close file 
  output->Write();
  output->Close();
  delete output;
#endif
  // We're done 
  //  delete fit;
  cout << "The END" << endl;
}
//____________________________________________________________________________



