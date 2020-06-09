// @(#)root/main:$Name:  $:$Id: h2mdf.C,v 1.3 2014/12/22 23:50:53 fisyak Exp $
/*
  root.exe TpcRS*2020/ADC/?I*ADC*.root mdf3ADC.C+
  root.exe ?I*U.root mdf3ADC.C+
  CHECK parameterizaion
  root.exe 'lDb.C("r2020",0)'  ?I*U.root mdf3ADC.C+
  root.exe 'lDb.C("r2020",0)' TpcRS*2020/ADC/?I*ADC*.root mdf3ADC.C+
*/
//#define __CHECK__
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
#include "TSystem.h"
#ifdef __CHECK__
#include "StDetectorDbMaker/St_TpcAdcCorrection3MDF.h"
#endif
#else
class TMultiDimFit;
enum EMDFPolyType {
  kMonomials,
  kChebyshev,
  kLegendre
};
#endif
TMultiDimFit* fit = 0;
Double_t timebucket = 0;
Int_t    fgIO = 0;
//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Fri Jun  5 09:33:18 2020 by ROOT version 5.34/39
// from TTree FitP/Fit results
// found on file: EI_9ADCU.root
//////////////////////////////////////////////////////////

#ifndef FitP_h
#define FitP_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

// Fixed size dimensions of array or collections stored in the TTree if any.

class FitP {
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

   FitP(TTree *tree=0);
   virtual ~FitP();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

FitP::FitP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("EI_9ADCU.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("EI_9ADCU.root");
      }
      f->GetObject("FitP",tree);

   }
   Init(tree);
}

FitP::~FitP()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t FitP::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t FitP::LoadTree(Long64_t entry)
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

void FitP::Init(TTree *tree)
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
   Notify();
}

Bool_t FitP::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void FitP::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t FitP::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
void FitP::Loop()
{
//   In a ROOT session, you can do:
//      Root > .L FitP.C
//      Root > FitP t
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
  const Double_t DDmin =  22.0; // Drift distance min
  const Double_t DDmax = 210.0; // -"-            max
  const Double_t zMax  = 207.707 - DDmin;
  const Double_t zMin  = 207.707 - DDmax;
  Long64_t nbytes = 0, nb = 0;
  Double_t xx[3] = {timebucket, 0, 0};
#ifdef __CHECK__
  static Double_t cut = 100;
  const St_MDFCorrection3 *table3MDF = (const St_MDFCorrection3 *) St_TpcAdcCorrection3MDF::instance()->Table();
  Int_t l = fgIO;
#endif
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = LoadTree(jentry);
    if (ientry < 0) break;
    nb = fChain->GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;
    if (dmu <= 0 || dmu > 0.02) continue;
    if (TMath::Abs(mu) > 1) continue;
    if (prob < 0.01) continue;
    if (x < zMin || x > zMax) continue;
    xx[1] = x;
    xx[2] = y;
#ifdef __CHECK__
    Double_t Cor = St_TpcAdcCorrection3MDF::instance()->Eval(l,xx);
    Double_t dev = Cor - mu;
    Double_t devS = dev/dmu;
    if (devS > cut) {
      cout << fChain->GetCurrentFile()->GetName() << "\tio = " << fgIO 
	   << "\t tb = " << xx[0] << "\tZ = " << xx[1] << "\tadcL = " << xx[2] 
	   << "\tmu = "	 << mu << " +/- " << dmu << "\t est = " << Cor 
	   << "\tdev = " << dev << "\t" << devS
	   << endl;
      //      continue;
    }
#endif
    fit->AddRow(xx, mu, dmu*dmu);
  }
}
#endif // #ifdef FitP_cxx

//using namespace std;
//________________________________________________________________________________
Double_t funcMDF(Double_t *x, Double_t *p=0) {
  if (! fit ) return 0;
  return fit->Eval(x, p);
}
//________________________________________________________________________________
void mdf3ADC(Int_t max=5, Int_t t = 0, Int_t maxTerm = 50, Double_t ymin = 0.2, Double_t ymax = 1)
  {
  // Global data parameters 
  Int_t nVars       = 3;
  
  // make fit object and set parameters on it. 
  //  fit = new TMultiDimFit(nVars, TMultiDimFit::kMonomials,"vk");
#ifdef __CINT__
  EMDFPolyType type = (EMDFPolyType) t;
#else
  TMultiDimFit::EMDFPolyType type = (TMultiDimFit::EMDFPolyType) t;
#endif
  fit = new TMultiDimFit(nVars, type,"vk");

  Int_t mPowers[]   = {max , max, max};
  fit->SetMaxPowers(mPowers);
  fit->SetMaxFunctions(10000);
  fit->SetMaxStudy(10000);
  fit->SetMaxTerms(maxTerm);
  fit->SetPowerLimit(max);
  fit->SetMinAngle(10);
  fit->SetMaxAngle(10);
  fit->SetMinRelativeError(.01);

  // variables to hold the temporary input data 
  Double_t *x = new Double_t[nVars];
  
  // Print out the start parameters
  fit->Print("p");
  // Fill data  
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  Int_t nn = files->GetSize();
  if (! nn) return;
  TFile **FitFiles = new TFile *[nn];
  TIter next(files);
  TFile *f = 0;
  enum {ktmBins = 13};
  Double_t tmBins[ktmBins+1] = {3.50, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5, 13.5, 15.5, 17.5, 21.5, 31.5};
  TAxis tmbAx(ktmBins,tmBins);
  while ( (f = (TFile *) next()) ) { 
    TTree *tree = (TTree *) f->Get("FitP");
    if (! tree ) continue;
    TString fName(gSystem->BaseName(f->GetName()));
    fgIO = 0;
    if (fName.Contains("I_")) fgIO = 1;
    Int_t index = fName.Index("ADC");
    TString Name(fName.Data(),index);
    Name.ReplaceAll("ADCU.root","");
    Name.ReplaceAll("EI_","");
    Name.ReplaceAll("WI_","");
    Name.ReplaceAll("EO_","");
    Name.ReplaceAll("WO_","");
    Int_t bin = Name.Atoi();
    timebucket = tmbAx.GetBinCenter(bin);
    FitP t(tree);
    t.Loop();
  }
  // Print out the statistics
  fit->Print("s");
  
  // Book histograms 
  fit->MakeHistograms();

  // Find the parameterization 
  fit->FindParameterization();

  // Print coefficents 
  fit->Print("rc");
#if 0
  //
  // Now for the data
  //
  TF2 *mdfP = new TF2("mdfP", funcMDF,xa->GetXmin(), xa->GetXmax(),ya->GetXmin(), ya->GetXmax());
  new TCanvas("mdfPar","mdfPar");
  mdfP->Draw();
#endif
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
