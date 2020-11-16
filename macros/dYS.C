/*
  root.exe MuTpcG.root dYS.C
 */
void dYS() {
  TH3F *h3 = (TH3F*) gDirectory->Get("dYS");
  if (! h3) return;
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1 ) c1 = new TCanvas("c1","c1");
  c1->SetLogz(1);
  TH2  *h2 = (TH2 *) h3->Project3D("zx");
  h2->SetStats(0);
  h2->Draw("colz");
  h2->FitSlicesY();
  TH1 *h1 = (TH1 *) gDirectory->Get(Form("%s_1",h2->GetName()));
  if (! h1) return;
  cout << "Found " << h1->GetName() << endl;
  h1->Draw("sames");
  h1->Fit("pol0","er","",0.5,12.5);
  h1->Fit("pol0","er+","",12.5,24.5);
}
