//void EtapT(const Char_t *file0 = "9p2GeVc.L.dev.19.root", const Char_t *file1 = "9p2GeVc.L.dEdx.3.root") {
void EtapT(const Char_t *file0 = "9p8GeV_fixedTarget.I.dev.root", const Char_t *file1 = "9p8GeV_fixedTarget.I.dEdx.root") {
  Double_t min = 1;
  TFile *_file0 = TFile::Open(file0);
  TFile *_file1 = TFile::Open(file1);
  TCanvas *c1 = new TCanvas();
  c1->Divide(2,1);
  c1->cd(1)->SetRightMargin(0.2);
  c1->cd(1)->SetLogz(1);
  TH2 *EtaVspT0 = (TH2 *) _file0->Get("/Tracks/EtaVspT");
  min = 1e-5*EtaVspT0->GetMaximum(); EtaVspT0->SetMinimum(min);
  EtaVspT0->Draw("colz");
  TH2 *EtaVspT1 = (TH2 *) _file1->Get("/Tracks/EtaVspT");
  c1->cd(2)->SetRightMargin(0.2);
  c1->cd(2)->SetLogz(1);
  min = 1e-5*EtaVspT1->GetMaximum(); EtaVspT1->SetMinimum(min);
  EtaVspT1->Draw("colz");

  TCanvas *c2 = new TCanvas("c2","c2");
  c2->Divide(2,1);
  c2->cd(1)->SetRightMargin(0.2);
  c2->cd(1)->SetLogz(1);
  TH2 *EtaVspTAll0 = (TH2 *) _file0->Get("/Tracks/EtaVspTAll");
  min = 1e-5*EtaVspTAll0->GetMaximum(); EtaVspTAll0->SetMinimum(min);
  EtaVspTAll0->Draw("colz");
  TH2 *EtaVspTAll1 = (TH2 *) _file1->Get("/Tracks/EtaVspTAll");
  c2->cd(2)->SetLogz(1);
  c2->cd(2)->SetRightMargin(0.2);
  min = 1e-5*EtaVspTAll1->GetMaximum(); EtaVspTAll1->SetMinimum(min);
  EtaVspTAll1->Draw("colz");
}
