TFile *_file0 = TFile::Open("Pass125_TpcOnly_021DAnodes.root");
TFile *_file1 = TFile::Open("Pass125_TpcOnly_049DAnodes.root");
TFile *_file2 = TFile::Open("Pass125_TpcOnly_Cu62DAnodes.root");
void DrawSvtWafer(const Char_t *name = "B1L01W1H1") {
  TH1 *h[3] = {0,0,0};
  h[0] = (TH1*) _file0->Get(name); 
  h[1] = (TH1*) _file1->Get(name);
  h[2] = (TH1*) _file2->Get(name);
  TString opt("");
  Double_t ymax = 0;
  Double_t y;
  for (Int_t i = 0; i < 3; i++) {
    if (h[i]) {y = h[i]->GetMaximum(); if (y > ymax) ymax = y;}
  }
  for (Int_t i = 0; i < 3; i++) {
    if (! h[i]) continue;
    h[i]->SetStats(0);
    h[i]->SetMinimum(1);
    h[i]->SetMaximum(ymax); h[i]->SetLineColor(i+1);
    h[i]->Draw(opt);
    opt = "sames";
  }
}
