void PlotEff() {
  TFile *files[3] = {TFile::Open("pion+.root"), TFile::Open("alpha.root"), TFile::Open("He3.root")};
  Char_t *names[3]= {            "#pi^{+}",               "#alpha",                  "He^{3}"};
  TLegend *l = new TLegend(0.4,0.4,0.7,0.7);
  TString same("");
  for (Int_t i = 0; i < 3; i++) {
    if (! files[i]) continue;
    TH1 *EffGPosMcRecGl_z = (TH1 *) files[i]->Get("EffGPosMcRecGl_z");
    if (! EffGPosMcRecGl_z) continue;
    EffGPosMcRecGl_z->SetMarkerColor(i+1);
    EffGPosMcRecGl_z->Draw(same);
    l->AddEntry(EffGPosMcRecGl_z,Form("%s, Efficiency",names[i]));
    same = "same";
    TH1 *LostGPosMcLostGl_z = (TH1*) files[i]->Get("LostGPosMcLostGl_z");
    if (! LostGPosMcLostGl_z ) continue;
    LostGPosMcLostGl_z->SetMarkerStyle(22);
    LostGPosMcLostGl_z->SetMarkerColor(i+1);
    LostGPosMcLostGl_z->Draw(same);
    same = "same";
    l->AddEntry(LostGPosMcLostGl_z,Form("%s, Lost",names[i]));
  }
  l->Draw();
}
