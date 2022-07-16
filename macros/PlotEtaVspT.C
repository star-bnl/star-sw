void PlotEtaVspT() {
  const Char_t *Names[6] = { "EtaVspTGlP", "EtaVspTGlN", "EtaVspTPrP", "EtaVspTPrN", "EtaVspTPC", "EtaVspTNC"};
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1) c1 = new TCanvas("c1","c1",200,10,1200,1800);
  c1->Clear();
  c1->Divide(2,3);
  for (Int_t i = 0; i < 6; i++) {
    c1->cd(i+1)->SetLogz(1);
    TH2F *h2 = (TH2F *) gDirectory->Get(Names[i]);
    if (h2) {
      h2->Draw("colz");
      c1->Update();
    }
  }
}
