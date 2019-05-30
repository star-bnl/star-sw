// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  rcd("TPointsBUGPRunXII19pp510P13ia_dEdx")
  .x h2mdf.C("mu",5,1,20)
  .x h2mdf.C("sigma",5,1,20)
 */
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Riostream.h"
#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TLeafI.h"
#include "TH1.h"
#include "TH2.h"
#include "TF2.h"
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#include "TCanvas.h"
#else
class TMultiDimFit;
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
#endif
//using namespace std;
TMultiDimFit* fit = 0;
//________________________________________________________________________________
Double_t funcMDF(Double_t *x, Double_t *p=0) {
  if (! fit ) return 0;
  return fit->Eval(x, p);
}
//________________________________________________________________________________
#ifndef __CINT__
void h2mdfADC(Int_t max=5, Int_t t = 0, Int_t maxTerm = 10, Double_t ymin = 0.2, Double_t ymax = 1)
#else
void h2mdfADC(Int_t max=5, TMultiDimFit::EMDFPolyType type = TMultiDimFit::kMonomials, Int_t maxTerm = 10, Double_t ymin = 0.2, Double_t ymax = 1)
#endif
  {
  TH2D *mu = (TH2D *) gDirectory->Get("mu");
  TH2D *sigma = (TH2D *) gDirectory->Get("sigma");
  TH2D *entries = (TH2D *) gDirectory->Get("entries");
  TH2D *mu = (TH2D *) gDirectory->Get("mu");
  TH2D *mu = (TH2D *) gDirectory->Get("mu");
  if (! mu) {
    cout << "Histogram  has not been found " << endl;
    return;
  }
  // Global data parameters 
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
#ifndef __CINT__
  EMDFPolyType type = (EMDFPolyType) t;
#endif
  fit = new TMultiDimFit(nVars, type,"vk");

  Int_t mPowers[]   = {max , max};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(maxTerm);
  fit->SetPowerLimit(max);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");

  TAxis *xa = mu->GetXaxis();
  TAxis *ya = mu->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = mu->GetBinError(ix,iy);
      if (error <= 0.0) continue;
      if (error >  0.1) continue;
      Double_t value = mu->GetBinContent(ix,iy);
      if (value < ymin || value > ymax) continue;
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      Double_t yy = value;
      Double_t ee = error*error;
      fit->AddRow(x,yy,ee);
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
  //
  // Now for the data
  //
  TF2 *mdfP = new TF2("mdfP", funcMDF,xa->GetXmin(), xa->GetXmax(),ya->GetXmin(), ya->GetXmax());
  new TCanvas("mdfPar","mdfPar");
  mdfP->Draw();
  Int_t i, j;
  // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    cout << endl;
  }
  cout << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) cout << endl;
  }
  if (fit->GetNCoefficients()%2) cout << endl;
}
//____________________________________________________________________________
