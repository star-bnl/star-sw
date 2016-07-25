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
// Double_t bbfunc(Double_t *x,Double_t *par) {
//   Double_t pove   = x[0];
//   Double_t poverm = pove/par[0];
//   BetheBloch BB;
//   Double_t value = 1.e6*BB(poverm);
//   //  printf("x : %f p: %f  val : %f \n",x[0],poverm,value);
//   return value;
// }
Double_t bichselZ(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return TMath::Exp(m_Bichsel->GetMostProbableZ(TMath::Log10(poverm),par[3]));
}
Double_t bichsel70(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return m_Bichsel->GetI70(TMath::Log10(poverm),par[3]);
}
Double_t bichsel60(Double_t *x,Double_t *par) {
  Double_t pove   = x[0];
  Double_t poverm = pove/par[0];
  return m_Bichsel->GetI60(TMath::Log10(poverm),par[3]);
}
void sibg() {
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
  //  TH1F *hr = c1->DrawFrame(2.e-2,1,1.e3,1.e2);
  TH1F *hr = c1->DrawFrame(1.e-1,1,1.e4,1.e2);
  //  hr->SetXTitle("Momentum (GeV/c)");
  hr->SetXTitle("beta*gamma");
  hr->SetYTitle("dE/dx (keV/cm)");
  //                     Mass Type Length  log2(dx)
  Double_t params[4] = {  1.0,  0.,   60., 1.};
  TF1 *sipi = new TF1("sipi",sifunc,1.e-1,1.e4,4);
  //  sipi->SetParameter(0,0.13956995);
  sipi->SetParameters(params);
  sipi->SetLineColor(7);
  sipi->Draw("same");
  TF1 *sipiz = new TF1("sipiz",bichselZ,1.e-1,1.e4,4);
  sipiz->SetParameters(params);
  sipiz->SetLineColor(1);
  sipiz->Draw("same");
  TF1 *sipi70 = new TF1("sipi70",bichsel70,1.e-1,1.e4,4);
  sipi70->SetParameters(params);
  sipi70->SetLineColor(3);
  sipi70->Draw("same");
  TF1 *sipi60 = new TF1("sipi60",bichsel60,1.e-1,1.e4,4);
  sipi60->SetParameters(params);
  sipi60->SetLineColor(4);
  sipi60->Draw("same");
  params[3] = 0.;
  TF1 *sipi1z = new TF1("sipi1z",bichselZ,1.e-1,1.e4,4);
  sipi1z->SetParameters(params);
  sipi1z->SetLineColor(1);
  sipi1z->Draw("same");
  TF1 *sipi170 = new TF1("sipi170",bichsel70,1.e-1,1.e4,4);
  sipi170->SetParameters(params);
  sipi170->SetLineColor(3);
  sipi170->Draw("same");
  TF1 *sipi160 = new TF1("sipi160",bichsel60,1.e-1,1.e4,4);
  sipi160->SetParameters(params);
  sipi160->SetLineColor(4);
  sipi160->Draw("same");
}
