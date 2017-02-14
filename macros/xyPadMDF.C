// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
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
#include "TProfile.h"
#include "TGraph.h"
#include "TMath.h"
#include "TMultiDimFit.h"
#include "TString.h"
#else
class TMultiDimFit;
#endif
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
TMultiDimFit* fit = 0;
ofstream out;
//using namespace std;
//________________________________________________________________________________
void xyPadMDF1(const TH2D *total2D, Int_t max=7, Int_t maxTerm = 20){
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  if (fit) delete fit;
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
  fit = new TMultiDimFit(nVars, TMultiDimFit::kLegendre,"vk");
  fit->SetName(Form("MDF_%s",total2D->GetName()));
  gDirectory->Append(fit);
  Int_t mPowers[]   = {max , max};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(maxTerm);
#if 0
  //  fit->SetPowerLimit(max);
  fit->SetPowerLimit(1);
  fit->SetMinAngle(); //10);
  fit->SetMaxAngle(1); //10);
  fit->SetMinRelativeError(0.01);
#endif
  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");
  
  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = ya->GetFirst(); iy <= ya->GetLast(); iy++) {
    for (ix = xa->GetFirst(); ix <= xa->GetLast(); ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      //      if (error >  0.008) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      if (TMath::Abs(value) > 0.15) continue;
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
//       if (x[0] < 0.6) continue;
//       if (x[0] > 4.0) continue;
      Double_t yy = value;
      Double_t ee = error*error; // 1e-4; //
      fit->AddRow(x,yy,ee);
    }
  }
  // Print out the statistics
  fit->Print("s");
  
  // Book histograms 
  //  fit->SetBinVarX(1000);
  //  fit->SetBinVarY(1000);
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
  //
   // Now for the data
  TH2D *total2Dr = new TH2D(*total2D);
  total2Dr->SetName(Form("%s_MDFres",total2D->GetName()));
  total2Dr->Reset();
  TH2D *total2Df = new TH2D(*total2D);
  total2Df->SetName(Form("%s_MDFpar",total2D->GetName()));
  total2Df->Reset();
  Double_t chisq = 0;
  Int_t    NDF = - fit->GetNCoefficients();
  for (iy = ya->GetFirst(); iy <= ya->GetLast(); iy++) {
    for (ix = xa->GetFirst(); ix <= xa->GetLast(); ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      x[0]           = xa->GetBinCenter(ix);
      x[1]           = ya->GetBinCenter(iy);
      Double_t par = fit->Eval(x);
      total2Df->SetBinContent(ix,iy,par);
      Double_t diff = value - par;
      total2Dr->SetBinContent(ix,iy,diff);
      Double_t dev = diff/error;
#if 0
      cout << "x = " << x[0] << "\ty = " << x[1] << "\tv = " << value << " +/- " << error
	   << "\tr = " << diff << "\tdev = " << dev << endl;
#endif
      NDF++;
      chisq += dev*dev;
    }
  }  
  cout << "NDF = " << NDF << "\tchisq = " << chisq << "\tchisq/NDF = " << chisq/NDF << endl;
  //
#if 1
  Int_t i, j;
 // Assignment to coefficients vector.
  cout << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  cout << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  cout << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  out << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  out << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  out << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
    out << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  cout << endl;  out << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    cout << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
    out << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  cout << endl;  out << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      cout << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
      out << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    cout << endl;    out << endl;
  }
  cout << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  out << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    out << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) {cout << endl; out << endl;}
  }
  if (fit->GetNCoefficients()%2) {cout << endl; out << endl;}
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    cout << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    out << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) {cout << endl; out << endl;}
  }
  if (fit->GetNCoefficients()%2) {cout << endl; out << endl;}
#endif
}
//____________________________________________________________________________
void xyPadMDF(const Char_t  *total = "mu", Int_t max=7, Int_t maxTerm = 20){
  TH2D *mu = (TH2D *) gDirectory->Get(total);
  if (! mu) {
    cout << "Histogram " << total << " has not been found " << endl;
    return;
  }
  TString fOut =  "TpcPadCorrectionMDF.20160521.000311.C";
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_MDFCorrection\")) return 0;" << endl;
  out << "  MDFCorrection_st row;" << endl;
  out << "  St_MDFCorrection *tableSet = new St_MDFCorrection(\"TpcPadCorrectionMDF\",48);" << endl;
  for (Int_t sector = 1; sector <= 24; sector++) {
    for (Int_t io = 0; io < 2; io++) {// io == 0 : Outer, io = 1 : Inner
      TH2D *muP = new TH2D(*mu);
      muP->SetName(Form("%s_%i_%i",mu->GetName(), sector, io));
      if (io) muP->GetXaxis()->SetRange( 1+20*(sector-1),10+20*(sector-1)); // Inner
      else    muP->GetXaxis()->SetRange(11+20*(sector-1),20+20*(sector-1)); // Outer
      Int_t idx = io + 2*(sector-1);
      out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
      out << "  row.nrows = 48; "<< endl;
      out << "  row.idx   = " << Form("%2i", idx+1) << ";" << endl;
      xyPadMDF1(muP, max, maxTerm);
      out << "  tableSet->AddAt(&row," << idx << ");" << endl;
    }
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close();
}
//____________________________________________________________________________
