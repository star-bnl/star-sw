// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  rcd("TPoints2BUGPRunXII19pp510P13ia_dEdx")
  .x h2mdf.C("mu",5,1,20)
  .x h2mdf.C("sigma",5,1,20)
  root.exe TPoints2*UG*.root  MakeTpcLengthCorrectionMD2.C+

  foreach d (`ls -1d [0-9]*GeV*.root`)
     set b = `basename ${d} .root`;
     root.exe -q -b TPoints2*U*${d}  MakeTpcLengthCorrectionMD2.C | tee ${b}.log
  end 
 dir TpcZCorrectionB.20*.C | grep GeV | grep fixed | sed 's/TpcZCorrectionB/TpcLengthCorrectionMD2/g' | awk '{print "ln -s TpcLengthCorrectionMD2.FXT.C "$9}'
 dir TpcZCorrectionB.20*.C | grep GeV | grep -v fixed | sed 's/TpcZCorrectionB/TpcLengthCorrectionMD2/g' | awk '{print "ln -s TpcLengthCorrectionMD2.COL.C "$9}'
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
//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Sat Apr 16 19:27:33 2022 by ROOT version 5.34/39
// from TTree FitPP/Fit results
// found on file: TPoints2F+TPoints2FPGPdEdx.root
//////////////////////////////////////////////////////////
#ifndef FitPP_h
#define FitPP_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

// Fixed size dimensions of array or collections stored in the TTree if any.

class FitPP {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
   Float_t         i;
   Float_t         j;
   Float_t         x;
   Float_t         y;
   Float_t         mean;
   Float_t         rms;
   Float_t         peak;
   Float_t         mu;
   Float_t         sigma;
   Float_t         entries;
   Float_t         chisq;
   Float_t         prob;
   Float_t         a0;
   Float_t         a1;
   Float_t         a2;
   Float_t         a3;
   Float_t         a4;
   Float_t         a5;
   Float_t         a6;
   Float_t         Npar;
   Float_t         dpeak;
   Float_t         dmu;
   Float_t         dsigma;
   Float_t         da0;
   Float_t         da1;
   Float_t         da2;
   Float_t         da3;
   Float_t         da4;
   Float_t         da5;
   Float_t         da6;
   Float_t         muJ;
   Float_t         dmuJ;

   // List of branches
   TBranch        *b_i;   //!
   TBranch        *b_j;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_mean;   //!
   TBranch        *b_rms;   //!
   TBranch        *b_peak;   //!
   TBranch        *b_mu;   //!
   TBranch        *b_sigma;   //!
   TBranch        *b_entries;   //!
   TBranch        *b_chisq;   //!
   TBranch        *b_prob;   //!
   TBranch        *b_a0;   //!
   TBranch        *b_a1;   //!
   TBranch        *b_a2;   //!
   TBranch        *b_a3;   //!
   TBranch        *b_a4;   //!
   TBranch        *b_a5;   //!
   TBranch        *b_a6;   //!
   TBranch        *b_Npar;   //!
   TBranch        *b_dpeak;   //!
   TBranch        *b_dmu;   //!
   TBranch        *b_dsigma;   //!
   TBranch        *b_da0;   //!
   TBranch        *b_da1;   //!
   TBranch        *b_da2;   //!
   TBranch        *b_da3;   //!
   TBranch        *b_da4;   //!
   TBranch        *b_da5;   //!
   TBranch        *b_da6;   //!
   TBranch        *b_muJ;   //!
   TBranch        *b_dmuJ;   //!

   FitPP(TTree *tree=0);
   virtual ~FitPP();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
  //   virtual void     Loop2();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

FitPP::FitPP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("TPoints2F+TPoints2FPGPdEdx.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("TPoints2F+TPoints2FPGPdEdx.root");
      }
      f->GetObject("FitP",tree);

   }
   Init(tree);
}

FitPP::~FitPP()
{
   if (!fChain) return;
   //   delete fChain->GetCurrentFile();
}

Int_t FitPP::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t FitPP::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (fChain->GetTreeNumber() != fCurrent) {
      fCurrent = fChain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void FitPP::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normally not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);

   fChain->SetBranchAddress("i", &i, &b_i);
   fChain->SetBranchAddress("j", &j, &b_j);
   fChain->SetBranchAddress("x", &x, &b_x);
   fChain->SetBranchAddress("y", &y, &b_y);
   fChain->SetBranchAddress("mean", &mean, &b_mean);
   fChain->SetBranchAddress("rms", &rms, &b_rms);
   fChain->SetBranchAddress("peak", &peak, &b_peak);
   fChain->SetBranchAddress("mu", &mu, &b_mu);
   fChain->SetBranchAddress("sigma", &sigma, &b_sigma);
   fChain->SetBranchAddress("entries", &entries, &b_entries);
   fChain->SetBranchAddress("chisq", &chisq, &b_chisq);
   fChain->SetBranchAddress("prob", &prob, &b_prob);
   fChain->SetBranchAddress("a0", &a0, &b_a0);
   fChain->SetBranchAddress("a1", &a1, &b_a1);
   fChain->SetBranchAddress("a2", &a2, &b_a2);
   fChain->SetBranchAddress("a3", &a3, &b_a3);
   fChain->SetBranchAddress("a4", &a4, &b_a4);
   fChain->SetBranchAddress("a5", &a5, &b_a5);
   fChain->SetBranchAddress("a6", &a6, &b_a6);
   fChain->SetBranchAddress("Npar", &Npar, &b_Npar);
   fChain->SetBranchAddress("dpeak", &dpeak, &b_dpeak);
   fChain->SetBranchAddress("dmu", &dmu, &b_dmu);
   fChain->SetBranchAddress("dsigma", &dsigma, &b_dsigma);
   fChain->SetBranchAddress("da0", &da0, &b_da0);
   fChain->SetBranchAddress("da1", &da1, &b_da1);
   fChain->SetBranchAddress("da2", &da2, &b_da2);
   fChain->SetBranchAddress("da3", &da3, &b_da3);
   fChain->SetBranchAddress("da4", &da4, &b_da4);
   fChain->SetBranchAddress("da5", &da5, &b_da5);
   fChain->SetBranchAddress("da6", &da6, &b_da6);
   fChain->SetBranchAddress("muJ", &muJ, &b_muJ);
   fChain->SetBranchAddress("dmuJ", &dmuJ, &b_dmuJ);
   Notify();
}

Bool_t FitPP::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void FitPP::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t FitPP::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
  Int_t iok = -1;
  if (i == 0 || j == 0) return iok;
  if (dmu  >  0.01) return iok;
  if (prob < 1e-5) return iok;
  if (dsigma <= 0 || dsigma > 0.01) return iok;
  if (TMath::Abs(mu) > 0.5) return iok;
  iok = 1;
  return iok;
}
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
      Double_t Length = x;
      Double_t dxLog2 = y;
      xx[0] = Length;
      xx[1] = dxLog2;
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
void h2mdf(const Char_t  *total = "mu") {
  TH2D *total2D = (TH2D *) gDirectory->Get(total);
  if (! total2D) {
    cout << "Histogram  has not been found " << endl;
    return;
  }
  TDirectory *dir = total2D->GetDirectory();
  if (!dir) return;
  TTree *FitP = (TTree*) dir->Get("FitP");
  if (!FitP) return;
  FitPP t(FitP);
  // Global data parameters 
  Int_t nVars       = 2;
  
  TMultiDimFit::EMDFPolyType type = TMultiDimFit::kChebyshev; // 1; //
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
  fit = new TMultiDimFit(nVars, type,"vk");

  Int_t mPowers[]   = {9, 7};
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
      Double_t Length = xa->GetBinCenter(ix);
      Double_t dxLog2 = ya->GetBinCenter(iy);
      x[0]           = TMath::Log(Length); 
      x[1]           = dxLog2;
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
void  MakeTpcLengthCorrectionMD2(Int_t date = 0, Int_t time = 0){
  TFile *fIn[3] = {0};
  const Char_t *histN[2] = {"mu","sigma"};
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TFile *f = 0;
  TIter next(files);
  TString tag;
  while ( (f = (TFile *) next()) ) {
    TString F(f->GetName());
    if (! F.Contains("TPoints2")) continue;
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
      tag.ReplaceAll("TPoints2","");
      tag.ReplaceAll("70","");
      tag.ReplaceAll("F","");
      tag.ReplaceAll("N","");
      tag.ReplaceAll("UGP","");
      tag.ReplaceAll(".root","");
    }
    fIn[l] = f;
    cout << "l = " << l << " File = " << fIn[l]->GetName() << endl;
  }
  
  TString fOut;
  if (date > 0) fOut =  Form("TpcLengthCorrectionMD2.%8i.%06i.C",date,time);
  else          fOut =  Form("TpcLengthCorrectionMD2.%s.C",tag.Data());
  cout << "Create " << fOut << endl;
  out.open(fOut.Data());
  out << "TDataSet *CreateTable() {" << endl;
  out << "  if (!gROOT->GetClass(\"St_MDFCorrection\")) return 0;" << endl;
  out << "  MDFCorrection_st row;" << endl;
  out << "  St_MDFCorrection *tableSet = new St_MDFCorrection(\"TpcLengthCorrectionMD2\",6);" << endl;
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
	  //	  out << "  //" << gDirectory->GetName() << "; MakeTpcLengthCorrectionMD21(\"" << histN[m] << "\",5,1,20);" << endl;
	  //	  MakeTpcLengthCorrectionMD21(histN[m],  5,type,20); 
	  h2mdf(histN[m]); 
	  PrintRow();
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
