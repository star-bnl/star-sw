Double_t Girrf(Double_t poverm, Int_t k = 0) {
  //        returns value of relative ionisation normalised to value
  //        at p/m=4 poverm    p/m (=beta gamma)             (input)
  //        k = 0/1 (default) pi+/-,K+/-,P/pbar, deuteron, ..
  //        k = 2   e+
  //        k = 3   e-
  Double_t par[3][8] = {
    {1.73553e+00,  6.37205e-01, -2.83910e-01,  1.37318e-01,  
     6.24651e-02, -6.89529e-02,  1.83442e-02, -1.58792e-03},// pbar
    {1.53185e+00,  7.12521e-01,  1.08820e-02,  8.23682e-02, 
    -1.44492e-01,  7.37149e-02, -1.67665e-02,  1.43531e-03},// e+
    {1.51295e+00,  9.14499e-01, -5.50513e-02, -8.04242e-02, 
    -1.29549e-02,  3.82555e-02, -1.35265e-02,  1.43929e-03} // e-
  };
  Double_t parG[10] = {
    -9.51524e-01,  1.62233e+00, -2.03613e+00,  1.40230e+00, -4.77226e-01,
     6.16030e-02,  8.66171e-01, -2.35670e-02,  3.20828e-02,  0.00000e+00
  };    
  Double_t beta2inv = 1. + 1./(poverm*poverm);
  Double_t x = TMath::Log10 (poverm);
  if (x > 3.5) x = 3.5;
  Int_t l = 0;
  if (k == 1) l = 1; // e+
  if (k == 2) l = 2; // e-
  Double_t sirrf = par[l][0];
  Double_t xx[8];
  xx[0] = 1; 
  Int_t i;
  for (i=1; i<8; i++) {xx[i] = x*xx[i-1]; sirrf += par[l][i]*xx[i];}
  Double_t corr = 0;
#if 1
  corr = parG[0];
  if (x > 1.75) {
    x = 1.75;
    for (i=1; i<6; i++) xx[i] = x*xx[i-1];
  }
  for (i=1; i<6; i++) corr += xx[i]*parG[i];
  Double_t bL = TMath::Log(beta2inv);
  corr += bL*(parG[6] + bL*(parG[7] + bL*parG[8])); 
#endif
  //  printf("k:%i l:%i poverm: %f x:%f corr: %f sirrf: %f\n",k,l,poverm,x,sirrf);
  return 1.e-6*sirrf*beta2inv*TMath::Exp(corr);
  //  return 1.e-6*sirrf*TMath::Exp(corr);
}
//________________________________________________________________________________
Double_t sifunc(Double_t *x,Double_t *par) {
  Double_t pove   = pow(10.,x[0]);
  Double_t poverm = pove;///par[0];
  //  if (poverm > 1.e3) poverm = 1.e4;
  Int_t type = par[1];
  return Girrf(poverm,type);
}
void sirrfB() {
  if (gClassTable->GetID("StBichsel") < 0) {
    gSystem->Load("libStar");
    gSystem->Load("St_base");
    gSystem->Load("StarClassLibrary");
  }
  TF1 *sipi = new TF1("sipi",sifunc,-1.,4.,2);
  sipi->SetParameter(0,0.13956995);
  sipi->SetParameter(1,0);
  sipi->SetLineColor(3);
  //  sipi->SetMinimum(1.e-6);
  //  sipi->SetMaximum(2.5e-6);
  sipi->Draw();//"same");
  TF1 *sie = new TF1("sie",sifunc,-1.,4.,2);
  sie->SetParameter(0,0.51099907e-3);
  sie->SetParameter(1,2);
  sie->SetLineColor(4);
  sie->Draw("same");
  TF1 *siep = new TF1("siep",sifunc,-1.,4.,2);
  siep->SetParameter(0,0.51099907e-3);
  siep->SetParameter(1,1);
  siep->SetLineColor(5);
  siep->Draw("same");
  TF1 *sip = new TF1("sip",sifunc,-1.,4.,2);
  sip->SetParameter(0,0.93827231);
  sip->SetParameter(1,0);
  sip->SetLineColor(1);
  sip->Draw("same");
  TF1 *siK = new TF1("siK",sifunc,-1.,4.,2);
  siK->SetParameter(0,0.493677);
  siK->SetParameter(1,0);
  siK->SetLineColor(2);
  siK->Draw("same");
}
