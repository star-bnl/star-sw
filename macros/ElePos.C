void ElePos() {
  TCanvas *c1 = new TCanvas();
  c1->DrawFrame(-1,0.5,4,0.6);
  gROOT->LoadMacro("lBichsel.C");
  lBichsel();
  gROOT->LoadMacro("bichselG10.C+");
  bichselG10();
  const Char_t *fNames[2] = {"electrons","positrons"};
  TLegend *l = new TLegend(0.4,0.6,0.8,0.8);
  TString same("");
  for (Int_t i = 0; i < 2; i++) {
    TString fName(fNames[i]);
    fName += ".root";
    TFile *_file0 = TFile::Open(fName);
    if (! _file0) continue;
    TH2 *h2 = (TH2 *) gDirectory->Get("TdEdxP70");
    if (! h2) continue;
    h2->FitSlicesY();
    TH1 *h1 = (TH1 *) gDirectory->Get("TdEdxP70_1");
    h1->SetTitle("<log _{10} dE/dx (keV/cm) > versus log _{10} p (GeV/c)");
    h1->SetStats(0);
    h1->SetMarkerColor(i+2);
    h1->Draw(same); same = "same";
    l->AddEntry(h1,fNames[i]);
  }
  TF1 *f1 = (TF1 *) gROOT->GetListOfFunctions()->FindObject("I70B70e1");
  f1->Draw("same");
  l->AddEntry(f1,"Bichsel for electrons");
}
