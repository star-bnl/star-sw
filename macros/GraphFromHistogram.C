#include "TGraphErrors.h"
#include "TH1.h"
TGraphErrors *GraphFromHistogram(TH1D *hist, Bool_t Log10 = kFALSE) {
  if (! hist) return 0;
  Int_t npoints = hist->GetNbinsX();
  Double_t *x = new Double_t[npoints];
  Double_t *y = new Double_t[npoints];
  Double_t *e = new Double_t[npoints];
  Int_t np = 0;
  for (Int_t i = 1; i <=  npoints; i++) {
    e[np] = hist->GetBinError(i);
    if (e[np] <= 0) continue;
    Double_t X = hist->GetBinCenter(i);
    if (Log10) {
      x[np] = TMath::Power(10.,X);
    } else {
      x[np] = X;
    }
    y[np] = hist->GetBinContent(i);
    cout << np << "\tx ="  << x[np] << "\ty = " << y[np] << " +/- " << e[np] << endl;
    np++;
  }
  TGraphErrors *grr = new TGraphErrors(np, x, y, 0, e);
  delete [] x;
  delete [] y;
  delete [] e;
  return grr;
}
