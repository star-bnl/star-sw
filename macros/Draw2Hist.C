void Draw2Hist(TFile *f0=0, TFile *f1=0, const Char_t *Name="StEQaV0VtxPhiDist") {
  // Draw2Hist(_file0, _file1, "StEQaV0VtxPhiDist");
  if (! f0 || ! f1) return;
  TH1 *h0 = (TH1 *) f0->Get(Name); if (! h0) return;
  h0->Draw();
  TH1 *h1 = (TH1 *) f1->Get(Name); if (! h1) return;
  h1->SetLineColor(2);
  h1->SetMarkerColor(2);
  h1->Draw("same");
  h0->KolmogorovTest(h1,"UOND");
}
