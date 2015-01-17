//////////////////////////////////////////////////////////
// This class has been automatically generated on
// Fri Jan 16 14:42:01 2015 by ROOT version 5.34/25
// from TTree t/TTree with HFT hits and tracks
// found on file: st_physics_15100090_raw_5500008.tree.root
//////////////////////////////////////////////////////////

#ifndef HftT_h
#define HftT_h

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>

// Header file for the classes stored in the TTree if any.
#include <TObject.h>
#include "EventT.h"

class HftT {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
   EventT         *_e; //!
   TString         fOutFileName;
   HftT(TTree *tree=0);
   virtual ~HftT();
   virtual Int_t    Cut(Long64_t entry);
   virtual Int_t    GetEntry(Long64_t entry);
   virtual Long64_t LoadTree(Long64_t entry);
   virtual void     Init(TTree *tree);
   virtual void     Loop(Int_t N=0);
   virtual Bool_t   Notify();
   virtual void     Show(Long64_t entry = -1);
   virtual void     SetOutFileName(const Char_t *Out) {fOutFileName = Out;}
   ClassDef(HftT,0)
};
#endif
