#ifndef St_MDFCorrection3C_h
#define St_MDFCorrection3C_h
#include <string.h>
#include "TChair.h"
#include "tables/St_MDFCorrection3_Table.h"
#include "TF3.h"
#include "TF2.h"
#include "TF1.h"
class St_MDFCorrection3C : public TChair {
 public:
  enum EMDFPolyType {
    kMonomials,
    kChebyshev,
    kLegendre
  };
  St_MDFCorrection3C(St_MDFCorrection3 *table=0); 
  MDFCorrection3_st 	*Struct(Int_t k = 0) 	const {return ((St_MDFCorrection3*) Table())->GetTable()+k;}
  UInt_t     	getNumRows()                	const {return GetNRows();}
  UChar_t 	idx(Int_t k = 0)        	const {return Struct(k)->idx;}
  UChar_t 	nrows(Int_t k = 0) 	        const {return Struct(k)->nrows;}
  UChar_t 	PolyType(Int_t k = 0) 	        const {return Struct(k)->PolyType;}
  UChar_t 	NVariables(Int_t k = 0) 	const {return Struct(k)->NVariables;}
  UChar_t 	NCoefficients(Int_t k = 0) 	const {return Struct(k)->NCoefficients;}
  UChar_t* 	Powers(Int_t k = 0) 	        const {return Struct(k)->Power;}
  Double_t 	DMean(Int_t k = 0)           	const {return Struct(k)->DMean;}
  Double_t* 	XMin(Int_t k = 0)         	const {return Struct(k)->XMin;}
  Double_t* 	XMax(Int_t k = 0)       	const {return Struct(k)->XMax;}
  Double_t* 	Coefficients(Int_t k = 0) 	const {return Struct(k)->Coefficients;}
  Double_t* 	CoefficientsRMS(Int_t k = 0) 	const {return Struct(k)->CoefficientsRMS;}
  Double_t      Eval(Int_t k = 0, Double_t *x = 0) const;
  Double_t      Eval(Int_t k, Double_t x0, Double_t x1, Double_t x2) const;
  Double_t      EvalError(Int_t k = 0, Double_t *x = 0) const;
  Int_t         IsActiveChair() const;
  static Double_t MDFunc(Double_t *x = 0, Double_t *p = 0);
  static St_MDFCorrection3C *fgMDFCorrection3C;
 protected:
  virtual ~St_MDFCorrection3C();
 private:
  Double_t EvalFactor(Int_t k = 0, Int_t p = 0, Double_t x = 0) const;
  TF1         **fFunc;
  ClassDefChair(St_MDFCorrection3, MDFCorrection3_st )
  ClassDef(St_MDFCorrection3C,1) //C++ TChair for MDFCorrection3 table class
};
#endif
