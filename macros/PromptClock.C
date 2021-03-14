#if !defined(__CINT__) || defined(__MAKECINT__)
#include "Riostream.h"
#include <stdio.h>
#include "TROOT.h"
#include "TSystem.h"
#include "TMath.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "THnSparse.h"
#include "TStyle.h"
#include "TF1.h"
#include "TProfile.h"
#include "TTree.h"
#include "TChain.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TFitResult.h"
#include "TLegend.h"
#include "TCanvas.h"
#endif
void PromptClock() {
  struct File_t {
    const Char_t *file;
    const Char_t *tag;
  };
  enum {N = 4};
  const File_t Files[N] = {
    {"Prompt1400/TGPTpcHitZTMfl0.root", "all"},
    {"Prompt1/T750GPTpcHitZTMfl0.root", "vpd750"},
    {"Prompt1400/T1400GPTpcHitZTMfl0.root", "vpd1400"},
    {"Prompt1.LocalClock/TGPTpcHitZTMfl0.root", "Local"}
    //    {"Prompt1/T750GPTpcHitZTMfl0.root", "all"}, 
    //    {"Prompt1/TGPTpcHitZTMfl0.root", "all"}
  };
  TCanvas *c1 = new TCanvas("c1","c1");
  c1->Divide(1,2);
  const Char_t *IO[2] = {"I","O"};
  for (Int_t i = 0; i < 2; i++) {
    c1->cd(i+1);
    TLegend *l = new TLegend(0.6,0.7,0.7,0.9);
    TString same("");
    for (Int_t j = 0; j < N; j++) {
      TFile *f = new TFile(Files[j].file);
      if (! f) {
	cout << Files[j].file << " is not found" << endl;
	continue;
      }
      TNtuple *FitP = (TNtuple *) f->Get("FitP");
      if (! FitP) {
	//	delete f;
	cout << "FitP in file " << Files[j].file << " is not found" << endl;
	continue;
      }
      FitP->SetMarkerColor(j+1);
      if (!i ) FitP->Draw(Form("mu:x >> %s%s(24,0.4,24.5,500,8.6,10.2)",IO[i],Files[j].tag),"i&&j&&j<=40",same);
      else     FitP->Draw(Form("mu:x >> %s%s(24,0.4,24.5,500,3.0,3.6)",IO[i],Files[j].tag),"i&&j&&j>40",same);
      TH2 *hist = (TH2 *) f->Get(Form("%s%s",IO[i],Files[j].tag));
      if (hist) {
	hist->SetStats(0);
	hist->SetMarkerColor(j+1);
	hist->SetXTitle("sector");
	hist->SetYTitle("Prompt Hit position (time buckets)");
	if (i == 0) hist->SetTitle("Inner");
	else        hist->SetTitle("Outer");
	hist->Draw(same);
	l->AddEntry(hist,Files[j].tag);
	same = "same";
      }
      //      delete f;
    }
    l->Draw();
  }  
}
