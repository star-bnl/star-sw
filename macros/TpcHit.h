//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Wed Dec 18 09:15:12 2019 by ROOT version 5.34/39
// from TTree TpcHit/TpcHit
// found on file: hlt_20349015_10_01_000.root
//////////////////////////////////////////////////////////

#ifndef TpcHit_h
#define TpcHit_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.

// Fixed size dimensions of array or collections stored in the TTree if any.

class TpcHit {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
   TFile          *fOut;
   // Declaration of leaf types
   Int_t           sector;
   Int_t           row;
   Float_t         x;
   Float_t         y;
   Float_t         z;
   Float_t         q;
   Float_t         adc;
   Float_t         pad;
   Float_t         timebucket;
   Float_t         IdTruth;
   Float_t         npads;
   Float_t         ntbks;
   Float_t         xL;
   Float_t         yL;
   Float_t         zL;
   Float_t         dX;
   Int_t           trigId;
   Int_t           us;
   Int_t           fl;
   Float_t         time;
   Float_t         timeb;

   // List of branches
   TBranch        *b_sector;   //!
   TBranch        *b_row;   //!
   TBranch        *b_x;   //!
   TBranch        *b_y;   //!
   TBranch        *b_z;   //!
   TBranch        *b_q;   //!
   TBranch        *b_adc;   //!
   TBranch        *b_pad;   //!
   TBranch        *b_timebucket;   //!
   TBranch        *b_IdTruth;   //!
   TBranch        *b_npads;   //!
   TBranch        *b_ntbks;   //!
   TBranch        *b_xL;   //!
   TBranch        *b_yL;   //!
   TBranch        *b_zL;   //!
   TBranch        *b_dX;   //!
   TBranch        *b_trigId;   //!
   TBranch        *b_us;   //!
   TBranch        *b_fl;   //!
   TBranch        *b_time;   //!
   TBranch        *b_timeb;   //!

   TpcHit(TTree *tree=0);
   virtual ~TpcHit();
   virtual Int_t    Cut(Long64_t entry);
   virtual void     Fill(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
};

#endif

#ifdef TpcHit_cxx
TpcHit::TpcHit(TTree *tree) : fChain(0) 
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("hlt_20349015_10_01_000.root");
      if (!f || !f->IsOpen()) {
         f = new TFile("hlt_20349015_10_01_000.root");
      }
      f->GetObject("TpcHit",tree);

   }
   Init(tree);
}

TpcHit::~TpcHit()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
   if (fOut) fOut->Write();
}

Int_t TpcHit::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t TpcHit::LoadTree(Long64_t entry)
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

void TpcHit::Init(TTree *tree)
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

   fChain->SetBranchAddress("sector/I", &sector, &b_sector);
   fChain->SetBranchAddress("row/I", &row, &b_row);
   fChain->SetBranchAddress("x", &x, &b_x);
   fChain->SetBranchAddress("y", &y, &b_y);
   fChain->SetBranchAddress("z", &z, &b_z);
   fChain->SetBranchAddress("q", &q, &b_q);
   fChain->SetBranchAddress("adc", &adc, &b_adc);
   fChain->SetBranchAddress("pad", &pad, &b_pad);
   fChain->SetBranchAddress("timebucket", &timebucket, &b_timebucket);
   fChain->SetBranchAddress("IdTruth", &IdTruth, &b_IdTruth);
   fChain->SetBranchAddress("npads", &npads, &b_npads);
   fChain->SetBranchAddress("ntbks", &ntbks, &b_ntbks);
   fChain->SetBranchAddress("xL", &xL, &b_xL);
   fChain->SetBranchAddress("yL", &yL, &b_yL);
   fChain->SetBranchAddress("zL", &zL, &b_zL);
   fChain->SetBranchAddress("dX", &dX, &b_dX);
   fChain->SetBranchAddress("trigId/I", &trigId, &b_trigId);
   fChain->SetBranchAddress("us/I", &us, &b_us);
   fChain->SetBranchAddress("fl/I", &fl, &b_fl);
   fChain->SetBranchAddress("time", &time, &b_time);
   fChain->SetBranchAddress("timeb", &timeb, &b_timeb);
   Notify();
}

Bool_t TpcHit::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normally not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void TpcHit::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t TpcHit::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
   return 1;
}
#endif // #ifdef TpcHit_cxx
