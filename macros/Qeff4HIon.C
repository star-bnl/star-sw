Double_t V(Double_t Zi, Double_t beta) {
  Double_t x = beta*TMath::Power(Zi,-2./3.);
  return 121.4139*x + 0.0378*TMath::Sin(190.7165*x);
}
//________________________________________________________________________________
Double_t Qeff(Double_t *x, Double_t *p) {
  Double_t bg = x[0]/3.727417; // for alpha
  Double_t Zi = p[0];
  Double_t beta = bg/TMath::Sqrt(1 + bg*bg);
  Double_t v = V(Zi,beta);
  Double_t Q = Zi*(1 - (1.034 - 0.1777*TMath::Exp(-0.08114*Zi))*TMath::Exp(-v));
  cout << "bg " << bg << " Zi " << Zi << " beta " << beta << " v " << v << " Q " << Q << endl;
  return Q;
}
//________________________________________________________________________________
TF1 *Qeff4HIon(Double_t Zi=2) {
  TF1 *f =  new TF1(Form("Q%i",(Int_t)Zi),Qeff,1e-2,6,1);// GeV/c
  f->SetParameter(0,Zi);
  return f;
}
