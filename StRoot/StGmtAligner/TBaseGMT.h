#ifndef __TBASE__h
#define __TBASE__h
#include "EventT.h"
#include "TObject.h"
class TBaseGMT : public TObject {
public :
   TTree          *fChain;   //!pointer to the analyzed TTree or TChain
   Int_t           fCurrent; //!current Tree number in a TChain
   EventT         *fEvent;
   TString         fOutFileName;
   virtual void     Init(TTree *tree);
   TBaseGMT(TTree *tree = 0) : fEvent(0) {
     // if parameter tree is not specified (or zero), connect the file
     // used to generate this class and read the Tree.
     if (tree == 0) {
       TFile *f = (TFile*)gROOT->GetListOfFiles()->FindObject("/star/data09/calib/fisyak/Pass112/TpcSsd/065/Event_6065045_raw_1010001.root");
       if (!f) {
         f = new TFile("/star/data09/calib/fisyak/Pass112/TpcSsd/065/Event_6065045_raw_1010001.root");
       }
       tree = (TTree*)gDirectory->Get("T");
     }
     Init(tree);
   }
   virtual ~TBaseGMT() {} // if (!fChain) return; delete fChain->GetCurrentFile();}
   virtual Int_t    Cut(Long64_t entry) {}
   virtual Int_t    GetEntry(Long64_t entry) { if (!fChain) return 0; return fChain->GetEntry(entry);}
   virtual Long64_t LoadTree(Long64_t entry) {
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
   virtual void     Loop() {Loop(0);}
   virtual void     Loop(Int_t Nevents);
   virtual void     SetOutFileName(const Char_t *name="Out.root") {fOutFileName = name;}
   virtual Bool_t   Notify() {return kTRUE;}
   virtual void     Show(Long64_t entry = -1) {if (!fChain) return; fChain->Show(entry);}
   ClassDef(TBaseGMT,1)
};
#endif
