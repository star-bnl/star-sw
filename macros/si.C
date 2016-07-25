class Bichsel;
Bichsel *m_Bichsel = 0;
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  Int_t type = par[1];
  Int_t k = 0;
  if (type == 3) k = 1;
  return BetheBloch::Sirrf(poverm,par[2],k);
}

Double_t bbfunc(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  BetheBloch BB;
  Double_t value = 1.e6*BB(poverm);
  //  printf("x : %f p: %f  val : %f \n",x[0],poverm,value);
  return value;
}
Double_t bichselZ(Double_t *x,Double_t *par) {
  return 1.e-6*TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(x[0]),1.0));
}
Double_t bichsel70(Double_t *x,Double_t *par) {
  return 1.e-6*m_Bichsel->GetI70(TMath::Log10(x[0]),1.0);
}
Double_t bichsel60(Double_t *x,Double_t *par) {
  return 1.e-6*m_Bichsel->GetI60(TMath::Log10(x[0]),1.0);
}
void si() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StBichsel");
  }
  if (!m_Bichsel) m_Bichsel = Bichsel::Instance();
  TCanvas *c1 = new TCanvas("c1");
  c1->SetLogx();
  c1->SetLogy();
  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  hr->SetXTitle("Momentum (GeV/c)");
  hr->SetYTitle("dE/dx (keV/cm)");
  TF1 *sipi = new TF1("sipi",sifunc,1.e-2,1.e3,3);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  sipi->SetParameter(2,60);
  sipi->SetLineColor(7);
  sipi->Draw("same");
  TF1 *sipi2 = new TF1("sipi2",sifunc,1.e-2,1.e3,3);
  sipi2->SetParameter(0,0.13956995);
  sipi2->SetParameter(1,0);
  sipi2->SetParameter(2,20);
  sipi2->SetLineColor(1);
  sipi2->Draw("same");
#if 1
  TF1 *siK = new TF1("siK",sifunc,1.e-2,1.e3,3);
  siK->SetParameter(0,0.493677);
  siK->SetParameter(1,0);
  siK->SetParameter(2,60);
  siK->SetLineColor(6);
  siK->Draw("same");
  TF1 *sip = new TF1("sip",sifunc,1.e-2,1.e3,3);
  sip->SetParameter(0,0.93827231);
  sip->SetParameter(1,0);
  sip->SetParameter(2,60);
  sip->SetLineColor(3);
  sip->Draw("same");
  TF1 *sie = new TF1("sie",sifunc,1.e-2,1.e3,3);
  sie->SetParameter(0,0.51099907e-3);
  sie->SetParameter(1,1);
  sie->SetParameter(2,60);
  sie->SetLineColor(4);
  sie->Draw("same");
#endif
#if 0
  TF1 *bbpi = new TF1("bbpi",bbfunc,1.e-2,1.e3,2);
  bbpi->SetParameter(0,0.13956995);
  bbpi->SetParameter(1,0);
  bbpi->SetLineColor(2);
  bbpi->Draw("same");
#endif
}
