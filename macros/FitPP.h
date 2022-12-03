//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Sat Dec  3 13:50:53 2022 by ROOT version 5.34/39
// from TTree FitP/Fit results
// found on file: NPoints70UGP11p5GeV_2020.root
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
   Float_t         dpeak;
   Float_t         entries;
   Float_t         chisq;
   Float_t         prob;
   Float_t         Npar;
   Float_t         muJ;
   Float_t         dmuJ;
   Float_t         NormL;
   Float_t         mu;
   Float_t         sigma;
   Float_t         p3;
   Float_t         p4;
   Float_t         p5;
   Float_t         p6;
   Float_t         p7;
   Float_t         p8;
   Float_t         p9;
   Float_t         p10;
   Float_t         p11;
   Float_t         p12;
   Float_t         p13;
   Float_t         p14;
   Float_t         p15;
   Float_t         p16;
   Float_t         p17;
   Float_t         dNormL;
   Float_t         dmu;
   Float_t         dsigma;
   Float_t         dp3;
   Float_t         dp4;
   Float_t         dp5;
   Float_t         dp6;
   Float_t         dp7;
   Float_t         dp8;
   Float_t         dp9;
   Float_t         dp10;
   Float_t         dp11;
   Float_t         dp12;
   Float_t         dp13;
   Float_t         dp14;
   Float_t         dp15;
   Float_t         dp16;
   Float_t         dp17;

   // List of branches
   TBranch        *b_i;   //!
   TBranch        *b_j;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_mean;   //!
   TBranch        *b_rms;   //!
   TBranch        *b_peak;   //!
   TBranch        *b_dpeak;   //!
   TBranch        *b_entries;   //!
   TBranch        *b_chisq;   //!
   TBranch        *b_prob;   //!
   TBranch        *b_Npar;   //!
   TBranch        *b_muJ;   //!
   TBranch        *b_dmuJ;   //!
   TBranch        *b_NormL;   //!
   TBranch        *b_mu;   //!
   TBranch        *b_sigma;   //!
   TBranch        *b_p3;   //!
   TBranch        *b_p4;   //!
   TBranch        *b_p5;   //!
   TBranch        *b_p6;   //!
   TBranch        *b_p7;   //!
   TBranch        *b_p8;   //!
   TBranch        *b_p9;   //!
   TBranch        *b_p10;   //!
   TBranch        *b_p11;   //!
   TBranch        *b_p12;   //!
   TBranch        *b_p13;   //!
   TBranch        *b_p14;   //!
   TBranch        *b_p15;   //!
   TBranch        *b_p16;   //!
   TBranch        *b_p17;   //!
   TBranch        *b_dNormL;   //!
   TBranch        *b_dmu;   //!
   TBranch        *b_dsigma;   //!
   TBranch        *b_dp3;   //!
   TBranch        *b_dp4;   //!
   TBranch        *b_dp5;   //!
   TBranch        *b_dp6;   //!
   TBranch        *b_dp7;   //!
   TBranch        *b_dp8;   //!
   TBranch        *b_dp9;   //!
   TBranch        *b_dp10;   //!
   TBranch        *b_dp11;   //!
   TBranch        *b_dp12;   //!
   TBranch        *b_dp13;   //!
   TBranch        *b_dp14;   //!
   TBranch        *b_dp15;   //!
   TBranch        *b_dp16;   //!
   TBranch        *b_dp17;   //!

   FitPP(TTree *tree=0);
   virtual ~FitPP();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

#ifdef FitPP_cxx
FitPP::FitPP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("NPoints70UGP11p5GeV_2020.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("NPoints70UGP11p5GeV_2020.root");
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
   fChain->SetBranchAddress("dpeak", &dpeak, &b_dpeak);
   fChain->SetBranchAddress("entries", &entries, &b_entries);
   fChain->SetBranchAddress("chisq", &chisq, &b_chisq);
   fChain->SetBranchAddress("prob", &prob, &b_prob);
   fChain->SetBranchAddress("Npar", &Npar, &b_Npar);
   fChain->SetBranchAddress("muJ", &muJ, &b_muJ);
   fChain->SetBranchAddress("dmuJ", &dmuJ, &b_dmuJ);
   fChain->SetBranchAddress("NormL", &NormL, &b_NormL);
   fChain->SetBranchAddress("mu", &mu, &b_mu);
   fChain->SetBranchAddress("sigma", &sigma, &b_sigma);
   fChain->SetBranchAddress("p3", &p3, &b_p3);
   fChain->SetBranchAddress("p4", &p4, &b_p4);
   fChain->SetBranchAddress("p5", &p5, &b_p5);
   fChain->SetBranchAddress("p6", &p6, &b_p6);
   fChain->SetBranchAddress("p7", &p7, &b_p7);
   fChain->SetBranchAddress("p8", &p8, &b_p8);
   fChain->SetBranchAddress("p9", &p9, &b_p9);
   fChain->SetBranchAddress("p10", &p10, &b_p10);
   fChain->SetBranchAddress("p11", &p11, &b_p11);
   fChain->SetBranchAddress("p12", &p12, &b_p12);
   fChain->SetBranchAddress("p13", &p13, &b_p13);
   fChain->SetBranchAddress("p14", &p14, &b_p14);
   fChain->SetBranchAddress("p15", &p15, &b_p15);
   fChain->SetBranchAddress("p16", &p16, &b_p16);
   fChain->SetBranchAddress("p17", &p17, &b_p17);
   fChain->SetBranchAddress("dNormL", &dNormL, &b_dNormL);
   fChain->SetBranchAddress("dmu", &dmu, &b_dmu);
   fChain->SetBranchAddress("dsigma", &dsigma, &b_dsigma);
   fChain->SetBranchAddress("dp3", &dp3, &b_dp3);
   fChain->SetBranchAddress("dp4", &dp4, &b_dp4);
   fChain->SetBranchAddress("dp5", &dp5, &b_dp5);
   fChain->SetBranchAddress("dp6", &dp6, &b_dp6);
   fChain->SetBranchAddress("dp7", &dp7, &b_dp7);
   fChain->SetBranchAddress("dp8", &dp8, &b_dp8);
   fChain->SetBranchAddress("dp9", &dp9, &b_dp9);
   fChain->SetBranchAddress("dp10", &dp10, &b_dp10);
   fChain->SetBranchAddress("dp11", &dp11, &b_dp11);
   fChain->SetBranchAddress("dp12", &dp12, &b_dp12);
   fChain->SetBranchAddress("dp13", &dp13, &b_dp13);
   fChain->SetBranchAddress("dp14", &dp14, &b_dp14);
   fChain->SetBranchAddress("dp15", &dp15, &b_dp15);
   fChain->SetBranchAddress("dp16", &dp16, &b_dp16);
   fChain->SetBranchAddress("dp17", &dp17, &b_dp17);
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
   return 1;
}
#endif // #ifdef FitPP_cxx
