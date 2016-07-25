void PrintTimeC() {
  TH1D *mu = (TH1D*) gDirectory->Get("mu");
  if (! mu ) return;
  Double_t p[4] = { -1.12369e+00,1.03878e-09,5.53899e-04,7.37247e-03};
  TDatime t;
  Int_t n = mu->GetNbinsX();
  for (Int_t j = 1; j <= n; j++) {
    Double_t err = mu->GetBinError(j);
    if (err <= 0.0) continue;
    Double_t cont = mu->GetBinContent(j);
    Double_t x    = mu->GetBinCenter(j);
    Double_t dev  = (cont - (p[0]+p[1]*x) - p[2])/p[3];
    if (TMath::Abs(dev) > 3) {
      printf("%i\t%f\t%f\t%f\t",j,x,cont,dev);
      t = TDatime((int) x);
      t.Print();
    }
  }
}
