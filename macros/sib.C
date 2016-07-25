
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t poverm = pove;//par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return BetheBloch::Sirrf(poverm,60.,k);
}

Double_t bbfunc(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t poverm = pove;///par[0];
  BetheBloch BB;
  Double_t value = 1.e6*BB(poverm);
  //  printf("x : %f p: %f  val : %f \n",x[0],poverm,value);
  return value;
}
void sib() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
  }
  TCanvas *c1 = new TCanvas("c1");
  //  c1->SetLogx();
  c1->SetLogy();
  //  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  TH1F *hr = c1->DrawFrame(-0.6,1,4,20);
  hr->SetXTitle("Log10(#beta#gamma)");
  hr->SetYTitle("dE/dx (keV/cm)");
  TF1 *sipi = new TF1("sipi",sifunc,-0.6,4,2);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  sipi->SetLineColor(3);
  sipi->SetLineWidth(8);
  sipi->Draw("same");
  TF1 *sie = new TF1("sie",sifunc,-0.6,4,2);
  sie->SetParameter(0,0.13956995);
  sie->SetParameter(1,3);
  sie->SetLineColor(1);
  sie->SetLineWidth(8);
  sie->Draw("same");
  TF1 *bbpi = new TF1("bbpi",bbfunc,-0.6,4,2);
  bbpi->SetParameter(0,0.13956995);
  bbpi->SetParameter(1,0);
  bbpi->SetLineColor(4);
  bbpi->SetLineWidth(8);
  bbpi->Draw("same");
}
