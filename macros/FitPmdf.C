// @(#)root/main:$Name:  $:$Id: FitPmdf.C,v 1.1 2015/09/21 20:36:58 fisyak Exp $
/*
  rcd("TPointsBUGPRunXII19pp510P13ia_dEdx")
  .x FitPmdf.C("mu",5,1,20)
  .x FitPmdf.C("sigma",5,1,20)
 */
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
#include "TNtuple.h"
//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Tue Apr 21 11:57:44 2015 by ROOT version 5.34/29
// from TTree FitP/Fit results
// found on file: SparsesZdEdx.Frac.root
//////////////////////////////////////////////////////////

#include <TChain.h>

// Header file for the classes stored in the TTree if any.
#include <TObject.h>

// Fixed size dimensions of array or collections stored in the TTree if any.

class FitP {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leaf types
 //FitP_t          *FitPar;
   UInt_t          fUniqueID;
   UInt_t          fBits;
   Char_t          beg[1];
   Int_t           c;
   Int_t           ipT;
   Int_t           jeta;
   Double_t        RefMult;
   Double_t        RefMult_Min;
   Double_t        RefMult_Max;
   Double_t        pT;
   Double_t        pT_Min;
   Double_t        pT_Max;
   Double_t        eta;
   Double_t        eta_Min;
   Double_t        eta_Max;
   Double_t        KolmoD[15];
   Char_t          beg2[1];
   Int_t           iter;
   Double_t        Frac[5];
   Double_t        ErFrac[5];
   Double_t        CovFrac[15];
   Double_t        Chisq;
   Double_t        Prob;
   Double_t        mu;
   Double_t        dmu;
   Double_t        sigma;
   Double_t        dsigma;
   Double_t        ProbMu;
   Char_t          end[1];

   // List of branches
   TBranch        *b_FitPar_fUniqueID;   //!
   TBranch        *b_FitPar_fBits;   //!
   TBranch        *b_FitPar_beg;   //!
   TBranch        *b_FitPar_c;   //!
   TBranch        *b_FitPar_ipT;   //!
   TBranch        *b_FitPar_jeta;   //!
   TBranch        *b_FitPar_RefMult;   //!
   TBranch        *b_FitPar_RefMult_Min;   //!
   TBranch        *b_FitPar_RefMult_Max;   //!
   TBranch        *b_FitPar_pT;   //!
   TBranch        *b_FitPar_pT_Min;   //!
   TBranch        *b_FitPar_pT_Max;   //!
   TBranch        *b_FitPar_eta;   //!
   TBranch        *b_FitPar_eta_Min;   //!
   TBranch        *b_FitPar_eta_Max;   //!
   TBranch        *b_FitPar_KolmoD;   //!
   TBranch        *b_FitPar_beg2;   //!
   TBranch        *b_FitPar_iter;   //!
   TBranch        *b_FitPar_Frac;   //!
   TBranch        *b_FitPar_ErFrac;   //!
   TBranch        *b_FitPar_CovFrac;   //!
   TBranch        *b_FitPar_Chisq;   //!
   TBranch        *b_FitPar_Prob;   //!
   TBranch        *b_FitPar_mu;   //!
   TBranch        *b_FitPar_dmu;   //!
   TBranch        *b_FitPar_sigma;   //!
   TBranch        *b_FitPar_dsigma;   //!
   TBranch        *b_FitPar_ProbMu;   //!
   TBranch        *b_FitPar_end;   //!

   FitP(TTree *tree=0);
   virtual ~FitP();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
  //   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

FitP::FitP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("SparsesZdEdx.Frac.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("SparsesZdEdx.Frac.root");
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

   fChain->SetBranchAddress("fUniqueID", &fUniqueID, &b_FitPar_fUniqueID);
   fChain->SetBranchAddress("fBits", &fBits, &b_FitPar_fBits);
   fChain->SetBranchAddress("beg[1]", &beg, &b_FitPar_beg);
   fChain->SetBranchAddress("c", &c, &b_FitPar_c);
   fChain->SetBranchAddress("ipT", &ipT, &b_FitPar_ipT);
   fChain->SetBranchAddress("jeta", &jeta, &b_FitPar_jeta);
   fChain->SetBranchAddress("RefMult", &RefMult, &b_FitPar_RefMult);
   fChain->SetBranchAddress("RefMult_Min", &RefMult_Min, &b_FitPar_RefMult_Min);
   fChain->SetBranchAddress("RefMult_Max", &RefMult_Max, &b_FitPar_RefMult_Max);
   fChain->SetBranchAddress("pT", &pT, &b_FitPar_pT);
   fChain->SetBranchAddress("pT_Min", &pT_Min, &b_FitPar_pT_Min);
   fChain->SetBranchAddress("pT_Max", &pT_Max, &b_FitPar_pT_Max);
   fChain->SetBranchAddress("eta", &eta, &b_FitPar_eta);
   fChain->SetBranchAddress("eta_Min", &eta_Min, &b_FitPar_eta_Min);
   fChain->SetBranchAddress("eta_Max", &eta_Max, &b_FitPar_eta_Max);
   fChain->SetBranchAddress("KolmoD[15]", KolmoD, &b_FitPar_KolmoD);
   fChain->SetBranchAddress("beg2[1]", &beg2, &b_FitPar_beg2);
   fChain->SetBranchAddress("iter", &iter, &b_FitPar_iter);
   fChain->SetBranchAddress("Frac[5]", Frac, &b_FitPar_Frac);
   fChain->SetBranchAddress("ErFrac[5]", ErFrac, &b_FitPar_ErFrac);
   fChain->SetBranchAddress("CovFrac[15]", CovFrac, &b_FitPar_CovFrac);
   fChain->SetBranchAddress("Chisq", &Chisq, &b_FitPar_Chisq);
   fChain->SetBranchAddress("Prob", &Prob, &b_FitPar_Prob);
   fChain->SetBranchAddress("mu", &mu, &b_FitPar_mu);
   fChain->SetBranchAddress("dmu", &dmu, &b_FitPar_dmu);
   fChain->SetBranchAddress("sigma", &sigma, &b_FitPar_sigma);
   fChain->SetBranchAddress("dsigma", &dsigma, &b_FitPar_dsigma);
   fChain->SetBranchAddress("ProbMu", &ProbMu, &b_FitPar_ProbMu);
   fChain->SetBranchAddress("end[1]", &end, &b_FitPar_end);
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
//________________________________________________________________________________
//using namespace std;
TMultiDimFit* fit = 0;
//   enum EMDFPolyType {
//       kMonomials,
//       kChebyshev,
//       kLegendre
//    };
//________________________________________________________________________________
void FitPmdf(Int_t h = 0, Int_t max=5, TMultiDimFit::EMDFPolyType type = TMultiDimFit::kLegendre, Int_t maxTerm = 10, Double_t ymax = 1){
  TTree *tree = (TTree *) gDirectory->Get("FitP");
  if (! tree) {
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
  

  FitP t(tree);
  if (t.fChain == 0) return;

  Long64_t nentries = t.fChain->GetEntriesFast();

  Long64_t nbytes = 0, nb = 0;
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = t.LoadTree(jentry);
    if (ientry < 0) break;
    nb = t.GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;
    
    Double_t error = t.ErFrac[h];
    if (error <= 0) continue;
    Double_t value = t.Frac[h];
    x[0]           = TMath::Abs(t.pT);
    x[1]           = t.eta;
    Double_t yy = value;
    Double_t ee = error*error;
    fit->AddRow(x,yy,ee);
  }
  // Print out the start parameters
  fit->Print("p");
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
  for (Long64_t jentry=0; jentry<nentries;jentry++) {
    Long64_t ientry = t.LoadTree(jentry);
    if (ientry < 0) break;
    nb = t.GetEntry(jentry);   nbytes += nb;
    // if (Cut(ientry) < 0) continue;
    Double_t error = t.ErFrac[h];
    if (error <= 0) continue;
    Double_t value = t.Frac[h];
    x[0]           = TMath::Abs(t.pT);
    x[1]           = t.eta;
    Double_t pred  = fit->Eval(x);
    Double_t predE = fit->EvalError(x);
    Double_t dev = (value - pred)/TMath::Sqrt(error*error + predE*predE);
    cout << "pT = " << t.pT << " eta = " << t.eta << " frac = " << 100*value << " +/- " << 100*error << " pred = " << 100*pred << " +/- " << 100*predE << " dev = " << dev;
    if (TMath::Abs(dev) > 3.0) cout << " ================" << endl;
    else                       cout << endl;
  }
#if 0
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
#endif
}
//____________________________________________________________________________
