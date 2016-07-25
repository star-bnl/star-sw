Double_t Johnson(Double_t *x, Double_t *par) {
  Double_t xx = x[0];
  Double_t dev = par[0]+par[1]*log(xx/(1.-xx));
  return par[1]/(xx*(1-xx))*exp(-0.5*dev*dev);
}
