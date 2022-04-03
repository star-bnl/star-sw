#include "Ask.h"
//________________________________________________________________________________
TH3F *TH3Sum(const Char_t *name = "EO", Int_t i1 = 1, Int_t i2 = 13) {
  TH3F *sum = 0;
  for (Int_t i = i1; i <= i2; i++) {
    TH3F *h3 = (TH3F *) gDirectory->Get(Form("%s_%i",name,i));
    if (h3) {
      if (! sum) {
	sum = new TH3F(*h3);
        sum->SetName(name);
      } else  
	sum->Add(h3);
    }
  }
  return sum;
}
//________________________________________________________________________________
void Draw2ADC() {
  TFile *files[2] = {TFile::Open("TpcRS_ped/ADC/adc.root"),
		     TFile::Open("TpcRS_NoTpxAfterBurner/ADC/adc.root")};
  TCanvas *c1 = new TCanvas("c1","c1",800,1200);
  c1->Divide(1,2);
  enum {kTPC = 4, kVar = 6, kOpt = 2, ktmBins = 13};
  const Char_t *tpcName[4] = {"I","O","IC","OC"};
  const Char_t *WE[4] = {"W","E","West","East"};
  const Char_t *IO[4] = {"I","O","Inner","Outer"};
  const Char_t *WO[2] = {"Burn","NotBurn"};
  for (Int_t we = 0; we < 2; we++) {
    for (Int_t io = 0; io < 2; io++) {
      for (Int_t tb = 0; tb < ktmBins; tb++) {
	for (Int_t k = 0; k < 2; k++) {
	  files[k]->cd();
	  TString Name(Form("%s%s_%i",WE[we],IO[io],tb+1));
	  TH3F *h3 = (TH3F *) gDirectory->Get(Name);
	  if (! h3) continue;
	  Name += WO[k];
	  h3->SetName(Name);
	  c1->cd(k+1)->SetLogz(1);
	  h3->Project3D("zy")->Draw("colz");
	  TLegend *l = new TLegend(0.1,0.1,0.5,0.2);
	  l->AddEntry(h3,gDirectory->GetName());
	  l->Draw();
	}
	c1->Update();
	if (! gROOT->IsBatch() && Ask()) return;
      }
    }
  }
}
