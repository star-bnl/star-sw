void DrawRes(const Char_t *pad = "pad", const Char_t *AdcL = "AdcL", const Char_t *Inner = "Inner") {
  TString Pad(pad);
  TLegend *l = new TLegend(0.4,0.4,0.8,0.9);
  TH1 *h = 0;
  TH2 *h2 = 0;
  Int_t j = 0;
  for (Int_t i = 1; i <= 7; i++) {
    cout << "Get " << Form("%s_%s_RC_%sp%i_2",pad,AdcL,Inner,i) << endl;
    h = (TH1*) gDirectory->Get(Form("%s_%s_RC_%sp%i_2",pad,AdcL,Inner,i));
    if (! h) {
      cout << "Get " << Form("%s_%s_RC_%sp%i",pad,AdcL,Inner,i) << endl;
      h2 = (TH2 *) gDirectory->Get(Form("%s_%s_RC_%sp%i",pad,AdcL,Inner,i));
      if (! h2) continue;
      h2->FitSlicesY();
      h = (TH1*) gDirectory->Get(Form("%s_%s_RC_%sp%i_2",pad,AdcL,Inner,i));
    }
    if (! h) continue;
    h->SetStats(0); 
    h->SetMarkerStyle(20); 
    h->SetMarkerColor(i); 
    if (! j) {h->Draw(""); h->SetTitle(Form("#sigma_{%s} vs %s for %s sector",pad,AdcL,Inner));}
    else      h->Draw("same");
    j++;
    if (i == 1) l->AddEntry(h,"All");
    else        {
      if (Pad.Contains("pad")) l->AddEntry(h,Form("npad = %i",i));
      else                     l->AddEntry(h,Form("ntmbk/3+1 = %i",i));
    }
  }
  l->Draw();
}
// KEY: TH2D     pad_AdcL_RC_Outerp5;1   pad_{MC} - pad_{RC} versus AdcL_{RC} for Outer sectors for clusters with 5 pads
