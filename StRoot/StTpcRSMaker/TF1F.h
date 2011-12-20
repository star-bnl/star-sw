#ifndef TF1F_h
#define TF1F_h
#include "TF1.h"
#include <string.h>
class TF1F : public TF1 {
 public:
  TF1F() : TF1()     {fNpx       = 200;}
  TF1F(const char *name, const char *formula, Double_t xmin=0, Double_t xmax=1) : TF1(name, formula, xmin, xmax) , fdX(-1), fStep(-1) 
    {fNpx       = 200;}
  TF1F(const char *name, Double_t xmin, Double_t xmax, Int_t npar) : TF1(name, xmin, xmax, npar) , fdX(-1), fStep(-1) 
    {fNpx       = 200;}
  TF1F(const char *name, void *fcn, Double_t xmin, Double_t xmax, Int_t npar) : TF1(name, fcn, xmin, xmax, npar) , 
    fdX(-1), fStep(-1) {fNpx       = 200;}
  TF1F(const char *name, Double_t (*fcn)(Double_t *, Double_t *), Double_t xmin=0, Double_t xmax=1, Int_t npar=0) :
  TF1(name, fcn, xmin, xmax, npar) , fdX(-1), fStep(-1) {fNpx       = 200;};
  virtual ~TF1F() {}
  virtual void Save(Double_t xmin, Double_t xmax, Double_t ymin, Double_t ymax, Double_t zmin, Double_t zmax) {
    fXmin = xmin; fXmax = xmax; fStep = fNpx/(fXmax - fXmin); fNpx = fStep*(fNpx/fStep);  fdX   = (fXmax-fXmin)/ fNpx; 
    TF1::Save(xmin,xmax,ymin,ymax,zmin,zmax);
  }
  Double_t GetSaveL(Double_t *xx) {
    // Get value corresponding to X in array of fSave values
    if (xx[0] < fXmin || xx[0]  > fXmax || fdX <= 0) return 0.;
    Int_t bin     = Int_t((xx[0]-fXmin)/fdX);
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
  Double_t GetSaveL(Int_t N, Double_t x, Double_t *y) {
    // Get values y[N] corresponding to x+i, i = [0, ..., N-1];
    memset(y, 0, N*sizeof(Double_t));
    Int_t bin     = Int_t((x-fXmin)/fdX);
    for (Int_t i = 0; i < N && bin < fNsave - 3; i++, bin += fStep) {
      y[i] = fSave[bin];
    }
    return y[0];
  }
  Double_t GetSaveL(Int_t N, Double_t *x, Double_t *y) {
    // Get values y[N] corresponding to x[N] in array of fSave values
    memset(y, 0, N*sizeof(Double_t));
    if (fNsave <= 0) return 0.;
    for (Int_t i = 0; i < N; i++) {
      if (x[i] > fXmin) {
	Int_t bin     = Int_t((x[i]-fXmin)/fdX);
	if (bin < fNsave - 3) y[i] = fSave[bin];
      }
    }
    return y[0];
  }
 protected:
  Double_t fXmin;
  Double_t fXmax;
  Double_t fdX;
  Int_t    fStep;
  ClassDef(TF1F,1)
    
};
#endif
