#ifndef TF1F_h
#define TF1F_h
#include "TF1.h"

class TF1F : public TF1 {
 public:
  TF1F() : TF1() 
    {fNpx       = 200;}
  TF1F(const char *name, const char *formula, Double_t xmin=0, Double_t xmax=1) : TF1(name, formula, xmin, xmax) , fdX(-1) 
    {fNpx       = 200;}
  TF1F(const char *name, Double_t xmin, Double_t xmax, Int_t npar) : TF1(name, xmin, xmax, npar) , fdX(-1) 
    {fNpx       = 200;}
  TF1F(const char *name, void *fcn, Double_t xmin, Double_t xmax, Int_t npar) : TF1(name, fcn, xmin, xmax, npar) , 
    fdX(-1) {fNpx       = 200;}
  TF1F(const char *name, Double_t (*fcn)(Double_t *, Double_t *), Double_t xmin=0, Double_t xmax=1, Int_t npar=0) :
    TF1(name, fcn, xmin, xmax, npar) , fdX(-1) {fNpx       = 200;};
  virtual ~TF1F() {}
  Double_t GetSaveL(Double_t *xx) {
    // Get value corresponding to X in array of fSave values
    Double_t x = xx[0];
    if (fNsave <= 0) return 0.;
    if (fSave == 0) return 0.;
    Int_t np = fNsave - 3;
    if (fdX <= 0) {
      fXmin = Double_t(fSave[np+1]);
      fXmax = Double_t(fSave[np+2]);
      fdX   = (fXmax-fXmin)/np;
    }
    if (x < fXmin || x  > fXmax || fdX <= 0) return 0.;
    Int_t bin     = Int_t((x-fXmin)/fdX);
#if 0 /* step function */
    return fSave[bin];
#else
    Double_t xlow = fXmin + bin*fdX;
    Double_t xup  = xlow + fdX;
    Double_t ylow = fSave[bin];
    Double_t yup  = fSave[bin+1];
    Double_t y    = ((xup*ylow-xlow*yup) + x*(yup-ylow))/fdX;
    return y;
#endif
  }
  Double_t GetSaveL(Int_t N, Double_t *x, Double_t *y) {
    // Get values y[N] corresponding to x[N] in array of fSave values
    for (Int_t i = 0; i < N; i++) y[i] = 0;
    if (fNsave <= 0) return 0.;
    if (fSave == 0) return 0.;
    Int_t np = fNsave - 3;
    if (fdX <= 0) {
      fXmin = Double_t(fSave[np+1]);
      fXmax = Double_t(fSave[np+2]);
      fdX   = (fXmax-fXmin)/np;
    }
    if (fdX <= 0) return 0.;
    for (Int_t i = 0; i < N; i++) {
      if (x[i] > fXmin) {
	Int_t bin     = Int_t((x[i]-fXmin)/fdX);
	if (bin < fNsave - 3) y[i] = fSave[bin];
      }
    }
    return y[0];
  }
  Double_t GetSaveL(Int_t N, Double_t x, Double_t *y) {
    // Get values y[N] corresponding to x[N] in array of fSave values
    for (Int_t i = 0; i < N; i++) y[i] = 0;
    if (fNsave <= 0) return 0.;
    if (fSave == 0) return 0.;
    Int_t np = fNsave - 3;
    if (fdX <= 0) {
      fXmin = Double_t(fSave[np+1]);
      Double_t fXmax = Double_t(fSave[np+2]);
      fdX   = (fXmax-fXmin)/np;
    }
    if (fdX <= 0) return 0.;
    for (Int_t i = 0; i < N; i++) {
      Double_t X = x + i;
      if (X > fXmin) {
	Int_t bin     = Int_t((X-fXmin)/fdX);
	if (bin < fNsave - 3) y[i] = fSave[bin];
      }
    }
    return y[0];
  }
 protected:
  Double_t fXmin;
  Double_t fXmax;
  Double_t fdX;
  ClassDef(TF1F,1)
    
};
#endif
