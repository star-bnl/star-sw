#if defined(__CINT__) && ! defined(__MAKECINT__)
TChain *tChain = 0;
TChain *Chain(const Char_t *TreeName = "MuDst") {
  TCollection *files = gROOT->GetListOfFiles();
  if (! files) return tChain;
  TIter next(files);
  TFile *f = 0;
  tChain = new TChain(TreeName);
  Int_t NFiles = 0;
  ULong64_t nEvents = 0;
  ULong64_t nEvTot = 0;
  while ( (f = (TFile *) next()) ) {   
    TTree *tree = (TTree *) f->Get(TreeName);
    cout << "#\t" << NFiles << "\t" << f->GetName();
    if (tree) {
      NFiles++;
      nEvents = tree->GetEntries();
      cout << "\t" << nEvents << endl;
      nEvTot += nEvents;
      tChain->Add(f->GetName());
    } else {
      cout << "\tTTree is missing" << endl;
    }
    delete f; 
  }
  cout	<< "chained " << NFiles  << " files \t" 
	<< "with total " << nEvTot << " events \t" 
  	<< "chain returned pointer: " << tChain << endl;
  return tChain;
}
#else
#include "Riostream.h"
#include "TChain.h"
#include "TFile.h"
#include "TList.h"
#include "TDirIter.h"
TChain *tChain = 0;
TChain *Chain(const Char_t *files = "./*.MuDst.root",const Char_t *TreeName = "MuDst") {
  TDirIter Dir(files);
  //  TTreeIter iter(TreeName);
  //  iter.AddFile(files);
  TFile *f = 0;
  tChain = new TChain(TreeName);
  Int_t NFiles = 0;
  ULong64_t nEvents = 0;
  ULong64_t nEvTot = 0;
  Char_t *file = 0;
  while ( (file = (Char_t *) Dir.NextFile()) ) {   
    f = new TFile(file);
    if (! f) {cout << "missing file " << file << endl; continue;}
    TTree *tree = (TTree *) f->Get(TreeName);
    cout << "#\t" << NFiles << "\t" << f->GetName();
    if (tree) {
      NFiles++;
      nEvents = tree->GetEntries();
      cout << "\tNo,Events = " << nEvents << endl;
      nEvTot += nEvents;
      tChain->Add(f->GetName());
    } else {
      cout << "\tTTree is missing" << endl;
    }
    delete f; 
  }
  cout	<< "chained " << NFiles  << " files \t" 
	<< "with total " << nEvTot << " events \t" 
  	<< "chain returned pointer: " << tChain << endl;
  return tChain;
}

#endif
