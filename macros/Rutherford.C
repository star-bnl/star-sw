//________________________________________________________________________________
Double_t tmax(Double_t m, Double_t M, Double_t bg) {
  Double_t mOverM = m/M;
  Double_t bg2 = bg*bg;
  Double_t gamma = TMath::Sqrt(bg2 + 1);
  return 2*m*bg2/(1. + mOverM*(2*gamma + mOverM)); 
}
//________________________________________________________________________________
void RutherfordL10() {
  Double_t m = 0.51099907e-3;
  Double_t M = 0.13956995;
  Double_t bgL10 = TMath::Log10(4.);
  Double_t bg = TMath::Power(10.,bgL10);
  Double_t p = M*bg;
  Double_t Emax = 1e9*tmax(m,M,bg);
  Double_t beta = bg/TMath::Sqrt(1. + bg*bg);
  Double_t beta2 = beta*beta;
  Int_t nx = 951;
  Double_t EL10min = 9.39162204620773045e-01;
  Double_t EL10max = 5.40737225464206350e+00;
  TH1D *dNdEL10 = new TH1D("dNdEL10R","Rutherford sigma dNdE",nx,EL10min,EL10max);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t EL10 = dNdEL10->GetBinCenter(i);
    Double_t E = TMath::Power(10.,EL10);
    Double_t cs = 1.09357e+02/beta2*(1 - beta2*E/Emax)/(E*E);
    dNdEL10->SetBinContent(i,cs);
  }
}
//________________________________________________________________________________
void Rutherford() {
  Double_t m = 0.51099907e-3;
  Double_t M = 0.13956995;
  Double_t bgL = TMath::Log(4.);
  Double_t bg = TMath::Exp(bgL);
  Double_t p = M*bg;
  Double_t Emax = 1e9*tmax(m,M,bg);
  Double_t beta = bg/TMath::Sqrt(1. + bg*bg);
  Double_t beta2 = beta*beta;
  Int_t nx = 951;
  Double_t ELmin = 9.39162204620773045e-01*TMath::Log(10.);
  Double_t ELmax = 5.40737225464206350e+00*TMath::Log(10.);
  TH1D *dNdEL = new TH1D("dNdlogER","Rutherford sigma dNdlog(E)",nx,ELmin,ELmax);
  for (Int_t i = 1; i <= nx; i++) {
    Double_t EL = dNdEL->GetBinCenter(i);
    Double_t E = TMath::Exp(EL);
    Double_t cs = 1.09357e+02/beta2*(1 - beta2*E/Emax)/(E*E)*E;
    dNdEL->SetBinContent(i,cs);
  }
}
