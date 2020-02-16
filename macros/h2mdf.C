// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  rcd("TPointsBUGPRunXII19pp510P13ia_dEdx")
  .x h2mdf.C("mu",5,1,20)
  .x h2mdf.C("sigma",5,1,20)
  root.exe TPoints*UG*dEdx926.root  h2mdf.C
 */
#ifndef __CINT__
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Riostream.h"
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
#else
class TMultiDimFit;
#endif
//using namespace std;
ofstream out;
TMultiDimFit* fit = 0;
// enum EMDFPolyType {
//   kMonomials,
//   kChebyshev,
//   kLegendre
// };
//________________________________________________________________________________
void PrintRow() {
  Int_t i, j;
  // Assignment to coefficients vector.
  out << "  row.PolyType = \t"      << fit->GetPolyType() << ";" << endl;
  out << "  row.NVariables = \t"    << fit->GetNVariables() << ";" << endl;
  out << "  row.NCoefficients = \t" << fit->GetNCoefficients() << ";" << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    out << Form("  row.XMin[%2i] = %10.5g;", i,fit->GetMinVariables()->operator()(i));
  }
  out << endl;
  for (i = 0; i < fit->GetNVariables(); i++) {
    out << Form("  row.XMax[%2i] = %10.5g;", i,fit->GetMaxVariables()->operator()(i));
  }
  out << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    for (j = 0; j < fit->GetNVariables(); j++) {
      out << Form("  row.Power[%2i] = %2i;",i * fit->GetNVariables() + j,
		   fit->GetPowers()[fit->GetPowerIndex()[i] * fit->GetNVariables() + j]);
    }
    out << endl;
  }
  out << "  row.DMean = \t"          << fit->GetMeanQuantity() << ";" << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    out << Form("  row.Coefficients[%2i]    = %15.5g;", i, fit->GetCoefficients()->operator()(i));
    if ((i+1) %2 == 0) out << endl;
  }
  if (fit->GetNCoefficients()%2) out << endl;
  for (i = 0; i < fit->GetNCoefficients(); i++) {
    out << Form("  row.CoefficientsRMS[%2i] = %15.5g;", i, fit->GetCoefficientsRMS()->operator()(i));
    if ((i+1) %2 == 0) out << endl;
  }
  //  return out;
}
//________________________________________________________________________________
void h2mdf1(const Char_t  *total = "mu", Int_t max=5, TMultiDimFit::EMDFPolyType type = TMultiDimFit::kMonomials, Int_t maxTerm = 10, Double_t ymax = 1){
  TH2D *total2D = (TH2D *) gDirectory->Get(total);
  if (! total2D) {
    cout << "Histogram  has not been found " << endl;
    return;
  }
  // Global data parameters 
  Int_t nVars       = 2;
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
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
  TMultiDimFit::EMDFPolyType type = 1; //TMultiDimFit::kChebyshev;
  // Print out the start parameters
  fit->Print("p");

  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 0) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      if (TMath::Abs(value) > ymax) continue;
      Double_t Length = xa->GetBinCenter(ix);
      Double_t dxLog2 = ya->GetBinCenter(iy);
      x[0]           = TMath::Log(Length); 
      x[1]           = dxLog2;
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
  //  cout << PrintRow();
}
//____________________________________________________________________________
void  h2mdf(Int_t date = 0, Int_t time = 0){
  TFile *fIn[3] = {0};
  const Char_t *histN[2] = {"mu","sigma"};
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TFile *f = 0;
  TIter next(files);
  TString tag;
  while ( (f = (TFile *) next()) ) {
    TString F(f->GetName());
    if (! F.Contains("TPoints")) continue;
    Int_t l = -1;
    if (F.Contains("70U")) l = 0;
    else if (F.Contains("NU")) l = 1;
    else if (F.Contains("FU")) l = 2;
    if (l < 0) continue;
    TH2D *mu = (TH2D *) f->Get("mu");
    TH2D *sigma = (TH2D *) f->Get("sigma");
    if (! mu || ! sigma) continue;
    if (tag == "") {
      tag = f->GetName();
      tag.ReplaceAll("TPoints","");
      tag.ReplaceAll("70","");
      tag.ReplaceAll("F","");
      tag.ReplaceAll("N","");
      tag.ReplaceAll("UGP","");
      tag.ReplaceAll(".root","");
    }
    fIn[l] = f;
  }
  
  TString fOut;
  if (date > 0) fOut =  Form("TpcLengthCorrectionMDF.%8i.%06i.C",date,time);
  else          fOut =  Form("TpcLengthCorrectionMDF.%s.C",tag.Data());
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_MDFCorrection\")) return 0;" << endl;
  out << "  MDFCorrection_st row;" << endl;
  out << "  St_MDFCorrection *tableSet = new St_MDFCorrection(\"TpcLengthCorrectionMDF\",6);" << endl;
  out << "  Int_t nrows = 6;" << endl;
  Int_t idx = 0;
  for (Int_t l = 0; l < 3; l++) {
    for (Int_t m = 0; m < 2; m++) {
      out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
      out << "  row.nrows =  6; //" << gDirectory->GetName() << endl;
      idx++;
      out << "  row.idx   = " << Form("%2i", idx) << ";" << endl;
      if (fIn[l]) {
	fIn[l]->cd();
	TH2D *h = (TH2D *) gDirectory->Get(histN[m]);
	if (h) {
	  //	  out << "  //" << gDirectory->GetName() << "; h2mdf1(\"" << histN[m] << "\",5,1,20);" << endl;
	  //	  h2mdf1(histN[m],  5,type,20); 
	  h2mdf1(histN[m],  5, 1,20); 
	  PrintRow();
	}
      }
      out << "  tableSet->AddAt(&row);" << "// " << gDirectory->GetName() << ";\t" << idx << "\th2mdf1(\"" << histN[m] << "\",5,1,20);" << endl;
    }
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close();
}
//____________________________________________________________________________
