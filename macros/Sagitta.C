//________________________________________________________________________________
Double_t sagg(Double_t *x, Double_t *p = 0) {
  Double_t pT = x[0];
  Double_t B  = p[0];
  Double_t L  = p[1];
  Double_t R  = 100 * pT/(0.3 * 0.1 * B);
  if (2*R < L) return 0;
  Double_t alpha = TMath::ASin(L/(2*R));
  return R*(1 - TMath::Cos(alpha));
}
//________________________________________________________________________________
TF1 *Sagitta() {
  TF1 *sagitta = new TF1("sagitta",sagg, 0.01,20, 2);
  sagitta->SetParameter(0,5); // B = 5 kG
  //  sagitta->SetParameter(1,1); // global  L = 130 cm
  //  sagitta->SetParameter(1,130); // global  L = 130 cm
  sagitta->SetParameter(1,180);    //  primary L = 180 cm
  sagitta->Draw();
  return sagitta;
}
//________________________________________________________________________________
Double_t curv(Double_t *x, Double_t *p = 0) {
  Double_t pT = x[0];
  Double_t B  = p[0];
  Double_t L  = p[1];
  Double_t R  = 100 * pT/(0.3 * 0.1 * B);
  return 1./R;
}
//________________________________________________________________________________
TF1 *Curvature() {
  TF1 *curvature = new TF1("curvature",curv, 0.01,20, 2);
  curvature->SetParameter(0,5); // B = 5 kG
  curvature->SetParameter(1,180);    //  primary L = 180 cm
  curvature->Draw();
  return curvature;
}
//________________________________________________________________________________
/* For equidistant measurement 
    s = (y1 + y2)/2 - y2 = +/- 0.3*B*L^2/(8*pT)     = L^2/(8*R)
   [m]   [m]  [m]    [m]          [T][m]    [GeV/c]
*/
