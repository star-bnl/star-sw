Double_t sagg(Double_t *x, Double_t *p = 0) {
  Double_t pT = x[0];
  Double_t B  = p[0];
  Double_t L  = p[1];
  Double_t R  = 100 * pT/(0.3 * 0.1 * B);
  if (2*R < L) return 0;
  Double_t alpha = TMath::ASin(L/(2*R));
  return R*(1 - TMath::Cos(alpha));
}
TF1 *Saggita() {
  TF1 *saggita = new TF1("saggita",sagg, 0.1,20, 2);
  saggita->SetParameter(0,5); // B = 5 kG
  //  saggita->SetParameter(1,1); // global  L = 130 cm
  //  saggita->SetParameter(1,130); // global  L = 130 cm
  saggita->SetParameter(1,180);    //  primary L = 180 cm
  saggita->Draw();
  return saggita;
}
/* For equidistant measurement 
    s = (y1 + y2)/2 - y2 = +/- 0.3*B*L^2/(8*pT)     = L^2/(8*R)
   [m]   [m]  [m]    [m]          [T][m]    [GeV/c]
*/
