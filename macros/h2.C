// @(#)root/main:$Name:  $:$Id: h2.C,v 1.1 2004/03/30 23:18:32 fisyak Exp $
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <iostream.h>
#include "TROOT.h"
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
Char_t *Names[6] = {"p", "K","pi","e","mu","d"};
TMultiDimFit* fit = 0;
//_______________________________________________________________________________
Double_t EvalMdF(Double_t *x, Double_t *params=0) {
  Double_t xx[2];
  xx[0] = x[0];
  xx[1] = TMath::Abs(x[1]);
  Double_t val = TMath::Sin(fit->Eval(xx));
  return val*val;
}
//________________________________________________________________________________
void h2(Int_t ih=3, Char_t *set = "pT01_16N", Int_t max=5, Int_t Nhyps=4){
  TH2F *total2D = (TH2F *) gROOT->FindObject(set);
  if (! total2D) {
    cout << "Set " << set << " has not been found " << endl;
    return;
  }
  // Global data parameters 
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");

  Int_t mPowers[]   = { 5 , 5};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(10);
  fit->SetPowerLimit(5);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t d;
  Double_t e;
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");

  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t fNHyps = Nhyps;
  TH2F *frac2D = new TH2F(*total2D);
  TString Name2D(Names[ih-1]);
  Name2D += total2D->GetName();
  frac2D->SetName(Name2D.Data());
  TString title("Faction for ");
  title += Name2D;
  title += total2D->GetTitle();
  frac2D->SetTitle(title.Data());
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    Char_t name1[20];
    sprintf(name1,"%s_%02i",total2D->GetName(),iy);
    TH1 *total = (TH1 *) gROOT->FindObject(name1);
    if (! total) continue;
    TH1 **fHists  = new TH1*[1+fNHyps+fNHyps*(fNHyps+1)/2];
    for (Int_t jh = 1; jh <= fNHyps; jh++) {
      TString Name(Names[jh-1]);
      Name += total->GetName();
      fHists[jh] = (TH1 *) gROOT->FindObject(Name.Data());
      if (! fHists[jh]) continue;
      cout << "Got at \t" << jh << "\t" 
	   << fHists[jh]->GetName() << "/\t" 
	   << fHists[jh]->GetTitle() << endl;
      for (Int_t kh = 1; kh <= jh; kh++){
	Int_t ijh = fNHyps + (jh-1)*jh/2 + kh;
	Name   = Names[jh-1];
	Name  += Names[kh-1];
	Name  += total->GetName();
	fHists[ijh] = (TH1 *) gROOT->FindObject(Name.Data());
	cout << "Got at \t" << ijh << "\t" 
	       << fHists[ijh]->GetName() << "/\t" 
	       << fHists[ijh]->GetTitle() << endl;
      }
    }
    TH1 *frac = fHists[ih]; 
    if (frac) {
      cout << "Get Fraction from " << frac->GetName() << "  " << frac->GetTitle() << endl;
      // Create training sample 
      //   Int_t i;
      //   for (i = 0; i < nData ; i++) {
      
      //     // Make some data 
      //     makeData(x,d,e);
      
      //     // Add the row to the fit object
      //     fit->AddRow(x,d,e);
      //   }
      for (ix = 1; ix <= nx; ix++) {
	Double_t error = frac->GetBinError(ix)/100.;
	Double_t value = frac->GetBinContent(ix)/100.;
	Double_t pT    = xa->GetBinCenter(ix);
	Double_t Eta   = ya->GetBinCenter(iy);
	x[0]           = pT;
	x[1]           = TMath::Abs(Eta);
	cout << "\tpT :" << pT << "\tEta : " << Eta 
	     << "\tFraction : " << value << "\t+/-" << error << endl;
	if (value <= 2.e-3) continue;
	if (error <= 2.e-3) continue;
	Int_t ok = 1;
	for (Int_t kh = 1; kh <= fNHyps; kh++){
	  if (ih == kh) continue;
	  Int_t ijh;
	  if (kh <= ih) ijh = fNHyps + (ih-1)*ih/2 + kh;
	  else          ijh = fNHyps + (kh-1)*kh/2 + ih;
          Double_t correlation = fHists[ijh]->GetBinContent(ix);
	  ok = 1;
	  if (TMath::Abs(correlation) > 0.5) {
	    cout << "Correlation from " << fHists[ijh]->GetName() << " = " << correlation << endl;
	    ok = 0;
	  }
	}
	if (ok) {
	  frac2D->SetCellContent(ix,iy,value);
	  frac2D->SetCellError(ix,iy,error);
          double yy = TMath::ASin(TMath::Sqrt(value));
	  double ee = error*error/(4.*(value*(1.-value)));
	  fit->AddRow(x,yy,ee);
	}
      }
      //      break;
    }
  }
  // Print out the statistics
  fit->Print("s");

  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
  TF2 *mdfEval = new TF2("mdfEval",EvalMdF,xa->GetXmin(),xa->GetXmax(),ya->GetXmin(),ya->GetXmax(),0);
  frac2D->Draw("colz");
  mdfEval->Draw("same");
  Int_t n = 0;
  Double_t chisq = 0;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t pT    = xa->GetBinCenter(ix);
      Double_t Eta   = ya->GetBinCenter(iy);
      Double_t xx[2];
      xx[0]           = pT;
      xx[1]           = TMath::Abs(Eta);
      Double_t value = frac2D->GetCellContent(ix,iy);
      Double_t error = frac2D->GetCellError(ix,iy);
      if (error <= 2.e-3) continue;
      if (value <= 2.e-3) continue;
      Double_t pred = EvalMdF(xx);
      Double_t dev  = (value - pred)/error;
      n++; chisq += dev*dev;
      if (TMath::Abs(dev) > 3.0) 
      cout << n << "\tpT = " << pT << "\tEta = "  << Eta
	   << "\tval = " << value << "\t+/- " << error 
	   << "\tpred = " << pred << "\tdev = " << dev << endl;
    }
  }
  cout << n << " points chisq = " <<  chisq << endl;
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
