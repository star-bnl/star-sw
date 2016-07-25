#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include "TChain.h"
#include "TFile.h"
#include "TList.h"
#include "TDirIter.h"
#include "TROOT.h"
#endif
TChain *tchain = 0;
TChain *Chain(const Char_t *TreeName = "MuDst") {
  TCollection *files = gROOT->GetListOfFiles();
  if (! files) return tchain;
  TIter next(files);
  TFile *f = 0;
  tchain = new TChain(TreeName);
  Int_t NFiles = 0;
  ULong64_t nEvents = 0;
  ULong64_t nEvTot = 0;
  while ( (f = (TFile *) next()) ) {   
    TTree *tree = (TTree *) f->Get(TreeName);
    if (tree) {
      NFiles++;
      nEvents = tree->GetEntries();
      cout << "#\t" << NFiles << "\t" << f->GetName() << "\t" << nEvents << endl;
      nEvTot += nEvents;
      tchain->Add(f->GetName());
    } else {
      cout << "#\t" << NFiles << "\t" << f->GetName() << "\t Chain is missing" << endl;
    }
    delete f; 
  }
  cout << "chained " << NFiles  << " files\t" 
       << "\twith total\t" << nEvTot << " events" << endl;
  return tchain;
}
//________________________________________________________________________________
TChain *Chain(const Char_t *TreeName, Char_t *files) {
  TDirIter Dir(files);
  tchain = new TChain(TreeName);
  Int_t NFiles = 0;
  ULong64_t nEvents = 0;
  ULong64_t nEvTot = 0;
  Char_t *file = 0;
  while ((file = (Char_t *) Dir.NextFile())) {
    cout << "Try to open " << file << endl;
    TFile *f = new TFile(file);
    if (! f) continue;
    if (f && f->IsZombie()) {cout << f->GetName() << " is Zombie" << endl; continue;}
    else                    {cout << f->GetName() << " is o.k."   << endl;}
    TTree *tree = (TTree *) f->Get(TreeName);
    if (tree) {
      NFiles++;
      nEvents = tree->GetEntries();
      cout << "#\t" << NFiles << "\t" << f->GetName() << "\t" << nEvents << endl;
      nEvTot += nEvents;
      tchain->Add(f->GetName());
    } else {
      cout << "#\t" << NFiles << "\t" << f->GetName() << "\t Chain is missing" << endl;
    }
    delete f;
  }
  cout << "chained " << NFiles  << " files\t" 
       << "\twith total\t" << nEvTot << " events" << endl;
  return tchain;
}
