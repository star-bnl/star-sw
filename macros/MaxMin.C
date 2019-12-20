#include "Riostream.h"
#include "TObjString.h"
#include "TFile.h"
#include "TKey.h"
#include "TClass.h"
#include "TROOT.h"
#include "TH1.h"
void MaxMin() {// check max and min values in histograms
  Int_t NF = 0;
  TSeqCollection *files = gROOT->GetListOfFiles();
  if (! files) return;
  TIter next(files);
  TFile *f = 0;
  while ( (f = (TFile *) next()) ) { 
    TString F(f->GetName());
    // loop over all keys in this directory
    cout << Form("%-80s",f->GetName());
    TIter nextkey( f->GetListOfKeys() );
    TKey *key;
    while ( (key = (TKey*)nextkey())) {
      TObject *o = f->Get(key->GetName());
      if (! o) continue;
      if (o->IsA()->InheritsFrom( "TH1" )) {
	TH1 *h = (TH1 *) o;
	Double_t max = h->GetMaximum();
	Double_t min = h->GetMinimum();
	cout << ":\t" << h->GetName() << Form("\tmax = %7.3f\tmin = %7.3f",max, min);
      }
    }
    cout << endl;
  }
}
