#include "TF1F.h"
#include "TMath.h"
ClassImp(TF1F);
//________________________________________________________________________________
void TF1F::Save(Double_t xmin, Double_t xmax, Double_t ymin, Double_t ymax, Double_t zmin, Double_t zmax) {
  fXmin = xmin; fXmax = xmax; fStep = 20, fdX = 1./fStep; fNpx = TMath::Nint((fXmax - fXmin)/fdX);
  TF1::Save(xmin,xmax,ymin,ymax,zmin,zmax);
}
//________________________________________________________________________________
Double_t TF1F::GetSaveL(Double_t *xx) {
  // Get value corresponding to X in array of fSave values
  if (xx[0] < fXmin || xx[0]  > fXmax || fdX <= 0) return 0.;
  Int_t bin     = TMath::Nint((xx[0]-fXmin)/fdX);
#if 1 /* step function */
  return fSave[bin];
#else
  Double_t xlow = fXmin + bin*fdX;
  Double_t xup  = xlow + fdX;
  Double_t ylow = fSave[bin];
  Double_t yup  = fSave[bin+1];
  Double_t y    = ((xup*ylow-xlow*yup) + xx[0]*(yup-ylow))/fdX;
  return y;
#endif
}
//________________________________________________________________________________
Double_t TF1F::GetSaveL(Int_t N, Double_t x, Double_t *y) {
  // Get values y[N] corresponding to x+i, i = [0, ..., N-1];
  //  memset(y, 0, N*sizeof(Double_t));
  Int_t bin     = TMath::Nint((x-fXmin)/fdX);
  Int_t i1 = 0;
  while (bin < 0) {i1++; bin += fStep;}
  for (Int_t i = i1; i < N && bin < GetNpx() - 3; i++, bin += fStep) {
    y[i] = fSave[bin];
  }
  return y[0];
}
//________________________________________________________________________________
Double_t TF1F::GetSaveL(Int_t N, Double_t *x, Double_t *y) {
  // Get values y[N] corresponding to x[N] in array of fSave values
  memset(y, 0, N*sizeof(Double_t));
  if (GetNpx() <= 0) return 0.;
  for (Int_t i = 0; i < N; i++) {
    if (x[i] > fXmin) {
      Int_t bin     = Int_t((x[i]-fXmin)/fdX);
      if (bin < GetNpx() - 3) y[i] = fSave[bin];
    }
  }
  return y[0];
}
#if ROOT_VERSION_CODE >= 393216 /* = ROOT_VERSION(6,0,0) */
TF1F::TF1F(): TF1(){fNpx = 200;}
TF1F::TF1F(const char *name,const char *formula, Double_t xmin, Double_t xmax)
      :TF1(name,formula,xmax,xmin) {fNpx = 200;}
TF1F::TF1F(const char *name, Double_t (*fcn)(Double_t *, Double_t *), Double_t xmin, Double_t xmax, Int_t npar, Int_t ndim)
      : TF1(name, fcn, xmin, xmax, npar,ndim) {fNpx = 200;}
#if 0
TF1F::TF1F(const char *name, ROOT::Math::ParamFunctor f, Double_t xmin, Double_t xmax, Double_t ymin, Double_t ymax, Int_t npar, Int_t ndim)
      : TF1(name, f, xmin, xmax, npar,ndim) {fNpx = 200;}
#endif
#endif /* ROOT 6 */
