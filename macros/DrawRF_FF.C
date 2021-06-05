void DrawRF_FF() {
  c1 = new TCanvas("c1","c1");
  gStyle->SetOptStat(0);
  c1->Divide(1,3);
  c1->cd(1);
  dY->Draw("colz");
  TH2F *ddY = new TH2F(*dY);
  ddY->SetName("ddY");
  _file0->cd();
  c1->cd(2);
  dY->Draw("colz");
  ddY->Add(dY,-1);
  c1->cd(3);
  ddY->Draw("colz");
}
