// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  root.exe -q -b xyPad3G*.root Chain.C 'MakePadCorrectionMDF.C(tChain,7,20,20190225,202320)' >& MakePadCorrectionMDF.log
 */
#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
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
using namespace std;
//________________________________________________________________________________
void MakePadCorrectionMDF1(TTree *FitP, Int_t max=7, Int_t maxTerm = 20){
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  if (fit) delete fit;
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kChebyshev,"vk");
  fit = new TMultiDimFit(nVars, TMultiDimFit::kLegendre,"vk");
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
  Int_t Nrows = FitP->GetSelectedRows();
  cout << "Select " << Nrows << " No. active rows" << endl;
  Double_t *mu = FitP->GetV1();
  Double_t *X  = FitP->GetV2();
  Double_t *Y  = FitP->GetV3();
  Double_t *dmu = FitP->GetV4();
  Double_t nentries = 0;
  for (Int_t i = 0; i < Nrows; i++) {
    Double_t error = dmu[i];
    if (error <= 0) continue;
    //      if (error >  0.008) continue;
    Double_t value = mu[i];
    if (TMath::Abs(value) > 0.25) continue;
    x[0]           = X[i];
    x[1]           = Y[i];
    //       if (x[0] < 0.6) continue;
    //       if (x[0] > 4.0) continue;
    Double_t yy = value;
    Double_t ee = error*error; // 1e-4; //
    fit->AddRow(x,yy,ee);
    nentries++;
  }
  // Print out the start parameters
  fit->Print("p");
  // Print out the statistics
  fit->Print("s");
  cout << "SampleSize = " << fit->GetSampleSize() << "\tSumSqQuantity = " << fit->GetSumSqQuantity() << "\tSumSqAvgQuantity = " << fit->GetSumSqQuantity()/fit->GetSampleSize() << endl;
  //  if (fit->GetSumSqQuantity()/fit->GetSampleSize() < 5e-5) return;
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
  Int_t NDF = 0;
  Double_t chisq = 0;
  for (Int_t i = 0; i < Nrows; i++) {
    Double_t error = dmu[i];
    if (error <= 0) continue;
    Double_t value = mu[i];
    if (TMath::Abs(value) > 0.25) continue;
    x[0]           = X[i];
    x[1]           = Y[i];
    Double_t par = fit->Eval(x);
    Double_t diff = value - par;
    Double_t dev = diff/error;
#if 0
    cout << "x = " << x[0] << "\ty = " << x[1] << "\tv = " << value << " +/- " << error
	 << "\tr = " << diff << "\tdev = " << dev << endl;
#endif
    NDF++;
    chisq += dev*dev;
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
void MakePadCorrectionMDF(TTree *FitP, Int_t max=7, Int_t maxTerm = 20, Int_t date = 20190201, Int_t time = 709){
  if (! FitP) {
    cout << "TTree FitP has not been found " << endl;
    return;
  }
  TString fOut =  Form("TpcPadCorrectionMDF.%8i.%06i.C",date,time);
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_MDFCorrection\")) return 0;" << endl;
  out << "  MDFCorrection_st row;" << endl;
  out << "  St_MDFCorrection *tableSet = new St_MDFCorrection(\"TpcPadCorrectionMDF\",48);" << endl;
  TFile *fFileOut = new TFile("MDFitResults.root","recreate");
  for (Int_t sector = 1; sector <= 24; sector++) {
    for (Int_t io = 0; io < 2; io++) {// io == 0 : Outer, io = 1 : Inner
      // xyPad: 24*20, 32,-1,1, 200, -5., 5., 0.5, 24.5)
      // CdEdx[NdEdx].xpad = 2*(CdEdx[NdEdx].pad - 0.5)/NoPadsInRow - 1.0;    => xpad = [ 1/N - 1, 2*(N - 0.5)/N - 1] = [-1 + 1/N, +1 - 1/N]
      // CdEdx[NdEdx].yrow = sector + 0.5*((row <= St_tpcPadConfigC::instance()->innerPadRows(sector)) ? 
      // (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/St_tpcPadConfigC::instance()->innerPadRows(sector) : 
      // (row - St_tpcPadConfigC::instance()->innerPadRows(sector) - 0.5)/(St_tpcPadConfigC::instance()->numberOfRows(sector) - St_tpcPadConfigC::instance()->innerPadRows(sector)));
      // yrow_I = [(1 - Ni - 0.5)/Ni, (Ni - Ni - 0.5)/Ni] =                                    sector + [-0.5/Ni - 1, -0.5/Ni] => sector + [-0.5, 0]
      // yrow_O = [(Ni + 1 - Ni - 0.5)/No, (Ni + No - Ni - 0.5)/No, (Ni + No - Ni - 0.5)/No] = sector + [0.5/No, 1 - 0.5/No]   => sector + [ 0, 0.5]
      // xyPad3 ## SIGN .Fill(FdEdx[k].yrow,FdEdx[k].xpad, Vars);	
      Double_t xmin = sector - 0.5*io;
      Double_t xmax = xmin + 0.5;
      do {
	Long64_t n = FitP->Draw("mu-muJ:x:y:dmu",Form("i&&j&&x>%f&&x<%f",xmin,xmax),"goff");
	Long64_t est = FitP->GetEstimate();
	if (n >= est) FitP->SetEstimate(2*n);
      } while (n >= est);
      cout << "sector = " << sector << " io = " << io << " xmin = " << xmin << " xmax = " << xmax << " No.entries = " << FitP->GetSelectedRows() << endl;
      TString dirname(Form("/%i_%i",sector,io));
      fFileOut->mkdir(dirname);
      fFileOut->cd(dirname);
#if 0
      TH2D *muP = new TH2D(*mu);
      muP->SetName(Form("%s_%i_%i",mu->GetName(), sector, io));
      if (io) muP->GetXaxis()->SetRange( 1+20*(sector-1),10+20*(sector-1)); // Inner
      else    muP->GetXaxis()->SetRange(11+20*(sector-1),20+20*(sector-1)); // Outer
#endif
      Int_t idx = io + 2*(sector-1);
      out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
      out << "  row.nrows = 48; //" << gDirectory->GetName() << endl;
      out << "  row.idx   = " << Form("%2i", idx+1) << ";" << endl;
      MakePadCorrectionMDF1(FitP, max, maxTerm);
      out << "  tableSet->AddAt(&row," << idx << ");" << endl;
    }
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close();
  
}
//____________________________________________________________________________
