#ifndef __TMDFParameters__
#define __TMDFParameters__
#include "TArrayI.h"
#include "TArrayD.h"
#include "TF2.h"
class TMDFParameters : public TObject {
 public:
  TMDFParameters(Double_t mean = 0, 
		 Int_t nvar = 0, Double_t *minV = 0, Double_t *maxV = 0, Double_t *meanV = 0, Int_t *maxPower = 0,
		 Int_t ncoef = 0, Int_t *code = 0, Double_t *coef = 0, Double_t *dcoef = 0) :
    fMean(mean),
    fNvar(nvar), fVmin(nvar,minV), fVmax(nvar,maxV), fVmean(nvar,meanV), fMaxPower(nvar,maxPower),
    fNcoef(ncoef), fCode(ncoef,code), fCoef(ncoef,coef), fdCoef(ncoef,dcoef)  {fgTMDFParameters = this;}
  virtual ~TMDFParameters() {fgTMDFParameters = 0;}
  static TMDFParameters *Instance() {return fgTMDFParameters;}
  TArrayD *GetTerms(Double_t *x);
  Double_t Eval(Double_t *x);
  Double_t dEval(Double_t *x);
  Double_t *Tcheb(Double_t x, Int_t N, Double_t *T);
  virtual void        Print(Option_t *option="") const;
  static Double_t  Func(Double_t *x, Double_t *p = 0);
  static TF1      *ProjectionX(Int_t code = 0);
  static TF2      *ProjectionXY(Int_t code1 = 0, Int_t code2 = 1);
  static Int_t     Nvar()              {return Instance()->fNvar;}
  static Double_t  Vmin(Int_t code)    {return Instance()->fVmin[code];}
  static Double_t  Vmax(Int_t code)    {return Instance()->fVmax[code];}
  static Double_t  Vmean(Int_t code)   {return Instance()->fVmean[code];}
  static void      SetCurrent(TMDFParameters *p) {fgTMDFParameters = p;}

 private:
  Double_t fMean;
  Int_t    fNvar;
  TArrayD  fVmin;
  TArrayD  fVmax;
  TArrayD  fVmean;
  TArrayI  fMaxPower;
  Int_t    fNcoef;
  TArrayI  fCode;
  TArrayD  fCoef;
  TArrayD  fdCoef;
  static TMDFParameters *fgTMDFParameters;
  ClassDef(TMDFParameters,1)
};
#endif
