Double_t Ec(Double_t *x, Double_t *p) {
  Double_t ws = 30; // eV
  Double_t Fs = 0.174;
  Double_t F  = Fs;
  Double_t w  =  26.2; // eV
  if (x[0] < w/2 || x[0] > 3.064*w) return 0;
  if (x[0] < w) return 1;
  return TMath::Power(w/x[0],4);
}
//________________________________________________________________________________
TF1 *fEc() {
  return new TF1("Ec",Ec,0,100,0);
}
