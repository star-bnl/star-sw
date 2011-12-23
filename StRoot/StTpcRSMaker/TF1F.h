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
  virtual void Save(Double_t xmin, Double_t xmax, Double_t ymin, Double_t ymax, Double_t zmin, Double_t zmax);
  Double_t GetSaveL(Double_t *xx);
  Double_t GetSaveL(Int_t N, Double_t x, Double_t *y);
  Double_t GetSaveL(Int_t N, Double_t *x, Double_t *y);
 protected:
  Double_t fXmin;
  Double_t fXmax;
  Double_t fdX;
  Int_t    fStep;
  ClassDef(TF1F,1)
    
};
#endif
