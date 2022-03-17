void NpadNtbk() {
  TFile *files[2] = {
    TFile::Open("daq_19GeV/st_physics_adc_20093036_raw_5000003.tags.Plots.R.root"),
    TFile::Open("TpcRS_19GeV_Altro_Z7/R.root")
  };
  if (! files[0] || ! files[1]) {
    cout << "Files are not found" << endl;
    return;
  }
  const Char_t *fileName[2] = {"daq","MC"};
  const Char_t *HistNames[4] = {"NpadsInner", "NpadsOuter", "NtmbksInner","NtmbksOuter"};
  TCanvas *c1 = new TCanvas();
  c1->Divide(2,2);
  for (Int_t i = 0; i < 4; i++) {
    c1->cd(i+1);
    TString same;
    TLegend *l = new TLegend(0.5,0.6,0.7,0.8);
    for (Int_t j = 0; j < 2; j++) {
      TH2 *h2 = (TH2*) files[j]->Get(HistNames[i]);
      if (! h2) continue;
      TH1 *h1 = h2->ProjectionY(Form("%s_%s",fileName[j],HistNames[i]));
      h1->SetNormFactor(1);
      h1->SetLineColor(j+1);
      h1->SetMarkerColor(j+1);
      h1->Draw(same);
      same = "sames";
      l->AddEntry(h1,fileName[j]);
    }
    l->Draw();
    c1->Update();
  }
}
