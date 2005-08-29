#ifndef ROOT_TRSymMatrix
#define ROOT_TRSymMatrix
#include "TRArray.h"
class TRMatrix;
class TRVector;
class TRSymMatrix : public TRArray {
 public:
  TRSymMatrix(Int_t nrows=0);
  TRSymMatrix(Int_t nrows,const Double_t *Array);
  TRSymMatrix(Int_t nrows,const Float_t *Array);
  TRSymMatrix(Int_t nrows,const Char_t *s);
  TRSymMatrix(const TRSymMatrix& W,ETRMatrixCreatorsOp kop);
  TRSymMatrix(ETRMatrixCreatorsOp kop,Int_t nrows);
  TRSymMatrix(const TRMatrix& A);
  TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop,const TRSymMatrix& S);
  TRSymMatrix(const TRSymMatrix& Q,ETRMatrixCreatorsOp kop,const TRSymMatrix& T);
  TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop);
#ifndef __CINT__
  TRSymMatrix (Int_t nrows, Double_t a0, ...);
#endif
  virtual ~TRSymMatrix();
  Int_t GetNrows()  const       {return fNrows;} 
  Int_t GetNcols()  const       {return GetNrows();}
  virtual ETRMatrixType GetMatrixType() const {return kSemiPosDefinedSymMatrix;}
  virtual Double_t Product(const TRVector& A,ETRMatrixCreatorsOp kop);
  virtual void    Print(Option_t *opt="") const;
  Double_t       &operator()(Int_t i)                    {return TRArray::operator[](i);}
  Double_t       &operator()(Int_t i,Int_t j);
  Double_t       &operator()(Int_t i,Int_t j) const;
 protected:
  Int_t     fNrows;            // number of rows 
 public:
  ClassDef(TRSymMatrix,1)  // TRSymMatrix class (double precision)
};
ostream& operator<<(ostream& s,const TRSymMatrix &target);
inline Double_t &TRSymMatrix::operator()(Int_t i,Int_t j){
  if (j < 0 || j >= fNrows) {
    ::Error("TRSymMatrix::operator()", "index j %d out of bounds (size: %d, this: 0x%08x)", 
	    j, fNrows, this); 
    j = 0;
  }
  if (i < 0 || i >= fNrows) {
    ::Error("TRSymMatrix::operator()", "index i %d out of bounds (size: %d, this: 0x%08x)", 
	    i, fNrows, this); 
    i = 0;
  }
  Int_t m = i;
  Int_t l = j;
  if (i > j) {m = j; l = i;}
  return TArrayD::operator[](m + (l+1)*l/2);
}
#endif

