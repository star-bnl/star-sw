Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  return BetheBloch::Sirrf(poverm,60.,type==3);
}
void sirrfA() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
  }
  TCanvas *c1 = new TCanvas("c1");
  c1->SetLogx();
  c1->SetLogy();
  TH1F *hr = c1->DrawFrame(1.e-1,1,1.e3,1.e2);
  hr->SetXTitle("Momentum (GeV/c)");
  hr->SetYTitle("dE/dx (keV/cm)");
  TF1 *sip = new TF1("sip",sifunc,1.e-1,1.e3,2);
  sip->SetParameter(0,0.93827231);
  sip->SetParameter(1,0);
  sip->SetLineColor(1);
  sip->Draw("same");
  TF1 *sipi = new TF1("sipi",sifunc,1.e-1,1.e3,2);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  sipi->SetLineColor(3);
  sipi->Draw("same");
  TF1 *sie = new TF1("sie",sifunc,1.e-1,1.e3,2);
  sie->SetParameter(0,0.51099907e-3);
  sie->SetParameter(1,3);
  sie->SetLineColor(4);
  sie->Draw("same");
  TF1 *siep = new TF1("siep",sifunc,1.e-1,1.e3,2);
  siep->SetParameter(0,0.51099907e-3);
  siep->SetParameter(1,0);
  siep->SetLineColor(5);
  siep->Draw("same");
  TF1 *siK = new TF1("siK",sifunc,1.e-1,1.e3,2);
  siK->SetParameter(0,0.493677);
  siK->SetParameter(1,0);
  siK->SetLineColor(2);
  siK->Draw("same");
}
