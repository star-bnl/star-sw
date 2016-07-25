#include "TGraphErrors.h"
#include "TH1.h"
TGraphErrors *GraphFromHistogram(TH1D *hist) {
  if (! hist) return 0;
  Int_t npoints = hist->GetNbinsX();
  Double_t *x = new Double_t[npoints];
  Double_t *y = new Double_t[npoints];
  Double_t *e = new Double_t[npoints];
  Int_t np = 0;
  for (Int_t i = 1; i <=  npoints; i++) {
    e[np] = hist->GetBinError(i+1);
    if (e[np] <= 0) continue;
    x[np] = hist->GetBinCenter(i+1);
    y[np] = hist->GetBinContent(i+1);
    np++;
  }
  TGraphErrors *grr = new TGraphErrors(np, x, y, 0, e);
  delete [] x;
  delete [] y;
  delete [] e;
  return grr;
}
