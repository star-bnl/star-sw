// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  rcd("NPoints2BUGPRunXII19pp510P13ia_dEdx")
  .x h2mdf.C("mu",5,1,20)
  .x h2mdf.C("sigma",5,1,20)
  root.exe NPoints2*U+*.root  MakeTpcLengthCorrectionMDN.C+

  foreach d (`ls -1d [0-9]*GeV*.root pp500GeV*.root`)
     set b = `basename ${d} .root`;
     root.exe -q -b NPoints2*U*${d}  MakeTpcLengthCorrectionMDN.C+ | tee ${b}.log
  end 
  foreach d (FF_OO_200GeV_2021  OO_200GeV_2021  ps_OO_200GeV_2021)
     set b = `basename ${d} .root`;
     root.exe -q -b NPoints*UGP${d}.root  MakeTpcLengthCorrectionMDN.C+ | tee ${b}.log
  end 
 dir TpcZCorrectionB.20*.C | grep GeV | grep fixed | sed 's/TpcZCorrectionB/TpcLengthCorrectionMDN/g' | awk '{print "ln -s TpcLengthCorrectionMDN.FXT.C "$9}'
 dir TpcZCorrectionB.20*.C | grep GeV | grep -v fixed | sed 's/TpcZCorrectionB/TpcLengthCorrectionMDN/g' | awk '{print "ln -s TpcLengthCorrectionMDN.COL.C "$9}'

  foreach b (`ls -1d NPoints*U+*.root | sed -e 's/.*GP//' -e 's/.root//'`)
  echo "${b}"
  root.exe -q -b NPoints*U+*${b}.root  MakeTpcLengthCorrectionMDN.C+ | tee ${b}.log
  end 
  foreach b (`ls -1d NPoints*UGP*.root | sed -e 's/.*GP//' -e 's/.root//' | sort -u `)
  echo "${b}"
  root.exe -q -b NPoints*UGP${b}.root  MakeTpcLengthCorrectionMDN.C+ | tee ${b}.log
  end 
  dir TpcSec*.20*root | awk -F\. '{printf("ln -s TpcLengthCorrectionMDN.%s.C TpcLengthCorrectionMDN.%s.%s.C\n",$5,$2,$3)}'
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
static Bool_t Sigma = kFALSE;
// enum EMDFPolyType {
//   kMonomials,
//   kChebyshev,
//   kLegendre
// };
#define FitPP_cxx
#include "FitPP.h"
void FitPP::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L FitPP.C
//      Root > FitPP t
//      Root > t.GetEntry(12); // Fill t data members with entry number 12
//      Root > t.Show();       // Show values of entry 12
//      Root > t.Show(16);     // Read and show values of entry 16
//      Root > t.Loop();       // Loop on all entries
//

//     This is the loop skeleton where:
//    jentry is the global entry number in the chain
//    ientry is the entry number in the current Tree
//  Note that the argument to GetEntry must be:
//    jentry for TChain::GetEntry
//    ientry for TTree::GetEntry and TBranch::GetEntry
//
//       To read only selected branches, Insert statements like:
// METHOD1:
//    fChain->SetBranchStatus("*",0);  // disable all branches
//    fChain->SetBranchStatus("branchname",1);  // activate branchname
// METHOD2: replace line
//    fChain->GetEntry(jentry);       //read all branches
//by  b_branchname->GetEntry(ientry); //read only this branch
   if (fChain == 0) return;

   Long64_t nentries = fChain->GetEntriesFast();

   Long64_t nbytes = 0, nb = 0;
   Double_t xx[2];
   for (Long64_t jentry=0; jentry<nentries;jentry++) {
      Long64_t ientry = LoadTree(jentry);
      if (ientry < 0) break;
      nb = fChain->GetEntry(jentry);   nbytes += nb;
      if (Cut(ientry) < 0) continue;
      // FitP->Draw("mu:x>>MuX","i&&j&&dmu<0.1&&prob>1e-5&&dsigma>0&&dsigma<0.01&&abs(mu)<0.1","prof")
      if (! (i&&j&&dmu<0.01&&dsigma>0&&dsigma<0.01&&abs(mu)<0.5)) continue;
      Double_t NodEdx = x;
      Double_t EtaG = y;
      xx[0] = TMath::Log(NodEdx);
      xx[1] = EtaG;
      if (! Sigma) {
	fit->AddRow(xx, mu, dmu*dmu);
      } else {
	fit->AddRow(xx, sigma, dsigma*dsigma);
      }
   }
}
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
Int_t h2mdf(const Char_t  *total = "mu") {
  TH2D *total2D = (TH2D *) gDirectory->Get(total);
  if (! total2D) {
    cout << "Histogram  has not been found " << endl;
    return 1;
  }
  TDirectory *dir = total2D->GetDirectory();
  if (!dir) return 1;
  TTree *FitP = (TTree*) dir->Get("FitP");
  if (!FitP) return 1;
  FitPP t(FitP);
  // Global data parameters 
  Int_t nVars       = 2;
  
  TMultiDimFit::EMDFPolyType type = TMultiDimFit::kChebyshev; // 1; //
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, type,"vk");

  Int_t mPowers[]   = {3, 5};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(1000);
  fit->SetMaxStudy(1000);
  fit->SetMaxTerms(20);
  fit->SetPowerLimit(9);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  // Print out the start parameters
  fit->Print("p");
#if 0
  TAxis *xa = total2D->GetXaxis();
  TAxis *ya = total2D->GetYaxis();
  Int_t nx = xa->GetNbins();
  Int_t ny = ya->GetNbins();
  Int_t iy, ix;
  for (iy = 1; iy <= ny; iy++) {
    for (ix = 1; ix <= nx; ix++) {
      Double_t error = total2D->GetBinError(ix,iy);
      if (error <= 4e-4) continue;
      if (error >  1e-2) continue;
      Double_t value = total2D->GetBinContent(ix,iy);
      if (TMath::Abs(value) > ymax) continue;
      Double_t NodEdx = xa->GetBinCenter(ix);
      Double_t EtaG = ya->GetBinCenter(iy);
      x[0]           = TMath::Log(NodEdx); 
      x[1]           = EtaG;
      Double_t yy = value;
      Double_t ee = error*error;
      fit->AddRow(x,yy,ee);
    }
  }
#else
  t.Loop();
#endif
  // Print out the statistics
  fit->Print("s");
  cout << "Sample size " << fit->GetSampleSize() << " ================================================================================" << endl;
  if (fit->GetSampleSize() < 100) {
    return 2;
  }
  // Book histograms 
  TDirectory *dirL = 0;
  if (! Sigma) dirL = gDirectory->mkdir("MuFit");
  else         dirL = gDirectory->mkdir("SigmaFit");
  dirL->cd();
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
  //
  // Now for the data
  //
  //  cout << PrintRow();
  return 0;
}
//____________________________________________________________________________
void  MakeTpcLengthCorrectionMDN(Int_t date = 0, Int_t time = 0){
  TFile *fIn[3] = {0};
  const Char_t *histN[2] = {"mu","sigma"};
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TFile *f = 0;
  TIter next(files);
  TString tag;
  while ( (f = (TFile *) next()) ) {
    TString F(f->GetName());
    if (! F.Contains("NPoints")) continue;
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
      tag.ReplaceAll("NPoints70","");
      tag.ReplaceAll("NPointsF","");
      tag.ReplaceAll("NPointsN","");
      tag.ReplaceAll("U+UPGP","");
      tag.ReplaceAll("UGP","");
      tag.ReplaceAll(".root","");
    }
    fIn[l] = f;
    cout << "l = " << l << " File = " << fIn[l]->GetName() << endl;
  }
  
  TString fOut;
  if (date > 0) fOut =  Form("TpcLengthCorrectionMDN.%8i.%06i.C",date,time);
  else          fOut =  Form("TpcLengthCorrectionMDN.%s.C",tag.Data());
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_MDFCorrection\")) return 0;" << endl;
  out << "  MDFCorrection_st row;" << endl;
  out << "  St_MDFCorrection *tableSet = new St_MDFCorrection(\"TpcLengthCorrectionMDN\",6);" << endl;
  out << "  Int_t nrows = 6;" << endl;
  Int_t idx = 0;
  for (Int_t l = 0; l < 3; l++) {
    fIn[l]->cd();
    for (Int_t m = 0; m < 2; m++) {
      Sigma = (m == 1);
      out << "  memset(&row,0,tableSet->GetRowSize());" << endl;
      out << "  row.nrows =  6; //" << gDirectory->GetName() << endl;
      idx++;
      out << "  row.idx   = " << Form("%2i", idx) << ";" << endl;
      if (fIn[l]) {
	fIn[l]->cd();
	TH2D *h = (TH2D *) gDirectory->Get(histN[m]);
	if (h) {
	  //	  out << "  //" << gDirectory->GetName() << "; MakeTpcLengthCorrectionMDN1(\"" << histN[m] << "\",5,1,20);" << endl;
	  //	  MakeTpcLengthCorrectionMDN1(histN[m],  5,type,20); 
	  if (!h2mdf(histN[m])) 	  PrintRow();
	}
      }
      out << "  tableSet->AddAt(&row);" << "// " << gDirectory->GetName() << ";\t" << idx << "\th2mdf(\"" << histN[m] << "\",5,1,20);" << endl;
      //      break;
    }
    //    break;
  }
  out << "  return (TDataSet *)tableSet;" << endl;
  out << "}" << endl;
  out.close();
}
//____________________________________________________________________________
