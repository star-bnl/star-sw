#include "Riostream.h"
#include "TChain.h"
#include "TFile.h"
#include "TList.h"
#include "TDirIter.h"
#ifdef __CINT__
TChain *Chain(const Char_t *TreeName = "MuDst") {
  TChain *chain = 0;
  TCollection *files = gROOT->GetListOfFiles();
  if (! files) return chain;
  TIter next(files);
  TFile *f = 0;
  chain = new TChain(TreeName);
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
      chain->Add(f->GetName());
    } else {
      cout << "\tTTree is missing" << endl;
    }
    delete f; 
  }
  cout	<< "chained " << NFiles  << " files \t" 
	<< "with total " << nEvTot << " events \t" 
  	<< "chain returned pointer: " << chain << endl;
  return chain;
}
#else
TChain *Chain(const Char_t *files = "./*.MuDst.root",const Char_t *TreeName = "MuDst") {
  TDirIter Dir(files);
  //  TTreeIter iter(TreeName);
  //  iter.AddFile(files);
  TChain *chain = 0;
  TFile *f = 0;
  chain = new TChain(TreeName);
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
      cout << "\t" << nEvents << endl;
      nEvTot += nEvents;
      chain->Add(f->GetName());
    } else {
      cout << "\tTTree is missing" << endl;
    }
    delete f; 
  }
  cout	<< "chained " << NFiles  << " files \t" 
	<< "with total " << nEvTot << " events \t" 
  	<< "chain returned pointer: " << chain << endl;
  return chain;
}

#endif
