#ifndef TF1F_h
#define TF1F_h
#include "TF1.h"
#include <string.h>
class TF1F : public TF1 {
 public:
#if ROOT_VERSION_CODE < 393216 /* = ROOT_VERSION(6,0,0) */
  TF1F() : TF1()     {fNpx       = 200;}
  TF1F(const char *name, const char *formula, Double_t xmin=0, Double_t xmax=1) : 
    TF1(name, formula, xmin, xmax) , fdX(-1), fStep(-1) {fNpx       = 200;}
  TF1F(const char *name, Double_t xmin, Double_t xmax, Int_t npar) :
    TF1(name, xmin, xmax, npar) , fdX(-1), fStep(-1) {fNpx       = 200;}
  TF1F(const char *name, void *fcn, Double_t xmin, Double_t xmax, Int_t npar) : 
    TF1(name, fcn, xmin, xmax, npar) , fdX(-1), fStep(-1) {fNpx       = 200;}
  TF1F(const char *name, Double_t (*fcn)(Double_t *, Double_t *), Double_t xmin=0, Double_t xmax=1, Int_t npar=0) :
    TF1(name, fcn, xmin, xmax, npar) , fdX(-1), fStep(-1) {fNpx       = 200;};
#else /* ROOT 6 */
   TF1F();
   TF1F(const char *name, const char *formula, Double_t xmin=0, Double_t xmax=1);
   TF1F(const char *name, void *fcn, Double_t xmin=0, Double_t xmax=1, Int_t npar=0);
#if !defined(__CINT__) && !defined(__CLING__)
   TF1F(const char *name, Double_t (*fcn)(Double_t *, Double_t *), Double_t xmin=0, Double_t xmax=1, Int_t npar=0, Int_t ndim=1 );
   TF1F(const char *name, Double_t (*fcn)(const Double_t *, const Double_t *), Double_t xmin=0, Double_t xmax=1, Int_t npar=0, Int_t ndim=1);
#endif

   // constructor using a functor
   TF1F(const char *name, ROOT::Math::ParamFunctor f, Double_t xmin = 0, Double_t xmax = 1, Int_t npar = 0, Int_t ndim=1);  

   // Template constructors from a pointer to any C++ class of type PtrObj with a specific member function of type 
   // MemFn. 
   template <class PtrObj, typename MemFn>
   TF1F(const char *name, const  PtrObj& p, MemFn memFn, Double_t xmin, Double_t xmax, Int_t npar, Int_t ndim, const char * c1, const char * c2) : 
      TF1(name,p,memFn,xmin,xmax,npar,c1,c2)
   {
      fNpx = 200; 
   } 
   // Template constructors from any  C++ callable object,  defining  the operator() (double * , double *) 
   // and returning a double.    
   template <typename Func> 
   TF1F(const char *name, Func f, Double_t xmin, Double_t xmax, Int_t npar, const char * tmp  ) : 
      TF1(name,f,xmin,xmax,npar,tmp)
   {
      fNpx = 200; 
   } 
#if 0
   // constructor used by CINT 
   TF1F(const char *name, void *ptr,  Double_t xmin, Double_t xmax, Int_t npar, const char *className ); 
   TF1F(const char *name, void *ptr, void *,Double_t xmin, Double_t xmax, Int_t npar, const char *className, const char *methodName = 0);
#endif
#endif /* ROOT 6 */
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
