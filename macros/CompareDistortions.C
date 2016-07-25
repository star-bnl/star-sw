void CompareDistortions(const Char_t *corr  = "Corr3", const Char_t *plot = "ddR") {
  TString t = (Form("%s.20130215.202757.FF.root" ,corr));
  TFile *fileFF = new TFile(t); if (! fileFF) {cout << "file\t" << t << " not found" << endl; return;}
  t = Form("%s.20130329.105728.RFF.root",corr);
  TFile *fileRF = new TFile(t); if (! fileRF) {cout << "file\t" << t << " not found" << endl; return;}
  t = Form("%s%sFF",plot,corr);
  TH2F *ff = (TH2F *) fileFF->Get(t); if (! ff ) {cout << "histogram\t" << t << " not found" << endl; return;}
  t = Form("%s%sRFF",plot,corr);
  TH2F *rf = (TH2F *) fileRF->Get(t); if (! rf ) {cout << "histogram\t" << t << " not found" << endl; return;}
  gStyle->SetOptStat(0);
  TCanvas *c1 = (TCanvas *) gROOT->GetListOfCanvases()->FindObject("c1");
  if (! c1) c1 = new TCanvas();
  c1->SetTitle(Form("%s %s",corr,plot));
  c1->Clear();
  c1->Divide(1,4);
  c1->cd(1); ff->Draw("colz");
  c1->cd(2); rf->Draw("colz");
  TH2F *sum = new TH2F(*ff);
  sum->SetName("sum");
  sum->SetTitle(Form("%s sum FF + RFF",sum->GetTitle()));
  sum->Add(rf);
  c1->cd(3); sum->Draw("colz");
  TH2F *dif = new TH2F(*ff);
  dif->SetName("dif");
  dif->SetTitle(Form("%s dif FF - RFF",dif->GetTitle()));
  dif->Add(rf,-1.);
  c1->cd(4); dif->Draw("colz");
} 
/*
OShortR : d(rho*phi) ok  -> sum ~ 0      d(rho) -> dif ~ 0
OBmap2D : d(rho*phi) ok  -> sum ~ 0      d(rho) -> dif ~ 0
OBmap   : d(rho*phi) ok  -> sum ~ 200 um d(rho) -> dif ~ 150 um
TWwist  : d(rho*phi) ok  -> diff = 0     d(rho) -> dif 400 ??????
Opr13   : d(rho*phi) ok  -> sum  = 0     d(rho) -> dif ~ 0
OIFC    : d(rho*phi) ok  -> sum  = 0     d(rho) -> dif ~ 0
OSectorAlign : d(rho*phi) ok  -> sum  = 0     d(rho) -> dif ~ 0
*/
