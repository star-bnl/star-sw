//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Sat Mar  9 16:44:52 2013 by ROOT version 5.34/05
// from TTree FitP/Fit results
// found on file: st_laser_adc_14046081_raw.F.Fit.g3.Student2sigma.root
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
   Float_t         sector;
   Float_t         row;
   Float_t         x;
   Float_t         y;
   Float_t         NormX;
   Float_t         dNormX;
   Float_t         MuX;
   Float_t         dMuX;
   Float_t         SigmaX;
   Float_t         dSigmaX;
   Float_t         deltaX;
   Float_t         ddeltaX;
   Float_t         chisqX;
   Float_t         NDFX;
   Float_t         NormY;
   Float_t         dNormY;
   Float_t         MuY;
   Float_t         dMuY;
   Float_t         SigmaY;
   Float_t         dSigmaY;
   Float_t         deltaY;
   Float_t         ddeltaY;
   Float_t         chisqY;
   Float_t         NDFY;

   // List of branches
   TBranch        *b_sector;   //!
   TBranch        *b_row;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_NormX;   //!
   TBranch        *b_dNormX;   //!
   TBranch        *b_MuX;   //!
   TBranch        *b_dMuX;   //!
   TBranch        *b_SigmaX;   //!
   TBranch        *b_dSigmaX;   //!
   TBranch        *b_deltaX;   //!
   TBranch        *b_ddeltaX;   //!
   TBranch        *b_chisqX;   //!
   TBranch        *b_NDFX;   //!
   TBranch        *b_NormY;   //!
   TBranch        *b_dNormY;   //!
   TBranch        *b_MuY;   //!
   TBranch        *b_dMuY;   //!
   TBranch        *b_SigmaY;   //!
   TBranch        *b_dSigmaY;   //!
   TBranch        *b_deltaY;   //!
   TBranch        *b_ddeltaY;   //!
   TBranch        *b_chisqY;   //!
   TBranch        *b_NDFY;   //!

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

#endif

#ifdef FitP_cxx
FitP::FitP(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("st_laser_adc_14046081_raw.F.Fit.g3.Student2sigma.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("st_laser_adc_14046081_raw.F.Fit.g3.Student2sigma.root");
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

   fChain->SetBranchAddress("sector", &sector, &b_sector);
   fChain->SetBranchAddress("row", &row, &b_row);
   fChain->SetBranchAddress("x", &x, &b_x);
   fChain->SetBranchAddress("y", &y, &b_y);
   fChain->SetBranchAddress("NormX", &NormX, &b_NormX);
   fChain->SetBranchAddress("dNormX", &dNormX, &b_dNormX);
   fChain->SetBranchAddress("MuX", &MuX, &b_MuX);
   fChain->SetBranchAddress("dMuX", &dMuX, &b_dMuX);
   fChain->SetBranchAddress("SigmaX", &SigmaX, &b_SigmaX);
   fChain->SetBranchAddress("dSigmaX", &dSigmaX, &b_dSigmaX);
   fChain->SetBranchAddress("deltaX", &deltaX, &b_deltaX);
   fChain->SetBranchAddress("ddeltaX", &ddeltaX, &b_ddeltaX);
   fChain->SetBranchAddress("chisqX", &chisqX, &b_chisqX);
   fChain->SetBranchAddress("NDFX", &NDFX, &b_NDFX);
   fChain->SetBranchAddress("NormY", &NormY, &b_NormY);
   fChain->SetBranchAddress("dNormY", &dNormY, &b_dNormY);
   fChain->SetBranchAddress("MuY", &MuY, &b_MuY);
   fChain->SetBranchAddress("dMuY", &dMuY, &b_dMuY);
   fChain->SetBranchAddress("SigmaY", &SigmaY, &b_SigmaY);
   fChain->SetBranchAddress("dSigmaY", &dSigmaY, &b_dSigmaY);
   fChain->SetBranchAddress("deltaY", &deltaY, &b_deltaY);
   fChain->SetBranchAddress("ddeltaY", &ddeltaY, &b_ddeltaY);
   fChain->SetBranchAddress("chisqY", &chisqY, &b_chisqY);
   fChain->SetBranchAddress("NDFY", &NDFY, &b_NDFY);
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
#endif // #ifdef FitP_cxx
