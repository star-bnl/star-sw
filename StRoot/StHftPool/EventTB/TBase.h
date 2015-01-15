//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Wed Dec 27 09:49:38 2006 by ROOT version 5.15/01
// from TTree T/TTree with SVT + SSD hits and tracks
// found on file: Event_6065058_raw_3020003.root
//////////////////////////////////////////////////////////

#ifndef TBase_h
#define TBase_h
//#define __USE_GLOBAL__
#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include "EventT.h"
const Int_t kMaxfTracks  = 10000;
const Int_t kMaxfHits =   400000;

class TBase {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain

   // Declaration of leave types
   EventT          *fEventT;
   TBase(TTree *tree=0);
   virtual ~TBase();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop();
   virtual void     MakeNt();
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
   //   ClassDef(TBase,1)
};


TBase::TBase(TTree *tree) : fEventT(0)
{
// if parameter tree is not specified (or zero), connect the file
// used to generate this class and read the Tree.
   if (tree == 0) {
      TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("Event_6065058_raw_3020003.root");
      if (!f) {
         f = new TFile("Event_6065058_raw_3020003.root");
      }
      tree = (TTree*)gDirectory->Get("T");

   }
   Init(tree);
}

TBase::~TBase()
{
   if (!fChain) return;
   delete fChain->GetCurrentFile();
}

Int_t TBase::GetEntry(Long64_t entry)
{
// Read contents of entry.
   if (!fChain) return 0;
   return fChain->GetEntry(entry);
}
Long64_t TBase::LoadTree(Long64_t entry)
{
// Set the environment to read one entry
   if (!fChain) return -5;
   Long64_t centry = fChain->LoadTree(entry);
   if (centry < 0) return centry;
   if (!fChain->InheritsFrom(TChain::Class()))  return centry;
   TChain *chain = (TChain*)fChain;
   if (chain->GetTreeNumber() != fCurrent) {
      fCurrent = chain->GetTreeNumber();
      Notify();
   }
   return centry;
}

void TBase::Init(TTree *tree)
{
   // The Init() function is called when the selector needs to initialize
   // a new tree or chain. Typically here the branch addresses and branch
   // pointers of the tree will be set.
   // It is normaly not necessary to make changes to the generated
   // code, but the routine can be extended by the user if needed.
   // Init() will be called many times when running on PROOF
   // (once per file to be processed).

   // Set branch addresses and branch pointers
   if (!tree) return;
   fChain = tree;
   fCurrent = -1;
   fChain->SetMakeClass(1);
   if (! fEventT) fEventT = new EventT();
   fChain->SetBranchAddress("T",&fEventT);
   Notify();
}

Bool_t TBase::Notify()
{
   // The Notify() function is called when a new file is opened. This
   // can be either for a new TTree in a TChain or when when a new TTree
   // is started when using PROOF. It is normaly not necessary to make changes
   // to the generated code, but the routine can be extended by the
   // user if needed. The return value is currently not used.

   return kTRUE;
}

void TBase::Show(Long64_t entry)
{
// Print contents of entry.
// If entry is not specified, print current entry
   if (!fChain) return;
   fChain->Show(entry);
}
Int_t TBase::Cut(Long64_t entry)
{
// This function may be called from Loop.
// returns  1 if entry is accepted.
// returns -1 otherwise.
  if (entry);
   return 1;
}
#endif
