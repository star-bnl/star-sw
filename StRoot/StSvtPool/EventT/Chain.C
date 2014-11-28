#include "Riostream.h"
#include "TChain.h"
#include "TFile.h"
#include "TList.h"

TChain *Chain(const Char_t *TreeName = "T") {
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
    if (tree) {
      NFiles++;
      nEvents = tree->GetEntries();
      cout << "#\t" << NFiles << "\t" << f->GetName() << "\t" << nEvents << endl;
      nEvTot += nEvents;
      chain->Add(f->GetName());
    }
    delete f; 
  }
  cout	<< "chained " << NFiles  << " files \t" 
	<< "with total " << nEvTot << " events \t" 
  	<< "chain returned pointer: " << chain << endl;
  return chain;
}
