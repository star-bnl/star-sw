#ifndef St_MDFCorrectionC_h
#define St_MDFCorrectionC_h

#include "TChair.h"
#include "tables/St_MDFCorrection_Table.h"

class St_MDFCorrectionC : public TChair {
 public:
  enum EMDFPolyType {
    kMonomials,
    kChebyshev,
    kLegendre
  };
  St_MDFCorrectionC(St_MDFCorrection *table=0) : TChair(table) {}
  MDFCorrection_st 	*Struct(Int_t k = 0) 	const {return ((St_MDFCorrection*) Table())->GetTable()+k;}
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
  Double_t      Eval(Int_t k = 0, const Double_t *x = 0) const;
  Double_t      Eval(Int_t k, const Double_t x0, Double_t x1) const;
  Double_t      EvalError(Int_t k = 0, const Double_t *x = 0) const;
 protected:
  virtual ~St_MDFCorrectionC() {}
 private:
  Double_t EvalFactor(Int_t k = 0, Int_t p = 0, Double_t x = 0) const;
  ClassDefChair(St_MDFCorrection, MDFCorrection_st )
  ClassDef(St_MDFCorrectionC,1) //C++ TChair for MDFCorrection table class
};
#endif
