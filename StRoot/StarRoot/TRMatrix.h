#ifndef ROOT_TRMatrix
#define ROOT_TRMatrix
#include "TError.h"
#include "TRArray.h"
class TRSymMatrix;
#include "TRSymMatrix.h"
class TRMatrix : public TRArray {
 public:
  TRMatrix(Int_t nrows=0,Int_t ncols=0) : TRArray(nrows*ncols), fNrows(nrows), fNcols(ncols) {}
  TRMatrix(Int_t nrows,Int_t ncols,const Double_t *Array) : TRArray(nrows*ncols,Array), fNrows(nrows), fNcols(ncols) {}
  TRMatrix(Int_t nrows,Int_t ncols,const Float_t *Array) : TRArray(nrows*ncols,Array), fNrows(nrows), fNcols(ncols) {}
		   TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRMatrix& B);
  TRMatrix(ETRMatrixCreatorsOp kop,Int_t nrows);
  TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop);
  TRMatrix(Int_t nrows,Int_t ncols,const Char_t *s) : TRArray(nrows*ncols,s), fNrows(nrows), fNcols(ncols) {}
  TRMatrix(const TRSymMatrix &S, ETRMatrixCreatorsOp kop,const TRMatrix& A);
  TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRSymMatrix &S);
  TRMatrix(const TRSymMatrix &S);
  TRMatrix(const TRMatrix &S, Int_t NI, Int_t NJ=0, Int_t I=1, Int_t J=1);
#ifndef __CINT__
  TRMatrix(Int_t nrows,Int_t ncols,Double_t a0, ...);
#endif
  TRMatrix    &operator=(const TRMatrix &rhs);
  virtual ~TRMatrix(){;}
  Int_t GetNrows()  const       {return fNrows;} 
  Int_t GetNcols()  const       {return fNcols;}
  Int_t NI()        const       {return fNrows;} 
  Int_t NJ()        const       {return fNcols;}
  void  SetMatrix(Int_t nrows,Int_t ncols,const  Double_t *array=0);  
  ETRMatrixType GetMatrixType() const {return kRectangular;}
  Double_t       &operator()(Int_t i)                    {return TRArray::operator[](i);}
  Double_t        operator()(Int_t i) const              {return TRArray::operator[](i);}
  Double_t       &operator()(Int_t i,Int_t j);
  Double_t        operator()(Int_t i,Int_t j) const {return operator()(i,j);}
 protected:
  Int_t     fNrows;            // number of rows 
  Int_t     fNcols;            // number of columns
 public:
  void Add(const TRMatrix& A, ETRMatrixCreatorsOp kop,const  TRMatrix& B); 
  void Substruct(const TRMatrix& A, ETRMatrixCreatorsOp kop,const  TRMatrix& B); 
  void AddRow(const Double_t *row) {
    fNrows++; Set(fNrows*fNcols); TCL::ucopy(row, fArray+(fNrows-1)*fNcols, fNcols);
  }
  void AddRow(const Float_t *row) { 
    fNrows++; Set(fNrows*fNcols); TCL::ucopy(row, fArray+(fNrows-1)*fNcols, fNcols);
  }
  const Double_t *GetRow(UInt_t col = 0) const {return GetArray() + col*fNcols;}
  virtual void Print(Option_t *opt="") const;
  friend TRMatrix operator*(const TRMatrix &source, Double_t scalar) {TRMatrix s(source); s *= scalar; return s;}
  friend TRMatrix operator*(Double_t scalar, const TRMatrix &source) {TRMatrix s(source); s *= scalar; return s;}
  friend TRMatrix operator/(const TRMatrix &source, Double_t scalar) {TRMatrix s(source); s /= scalar; return s;}
  friend TRMatrix operator+(const TRMatrix &source, Double_t scalar) {TRMatrix s(source); s += scalar; return s;}
  friend TRMatrix operator+(Double_t scalar, const TRMatrix &source) {TRMatrix s(source); s += scalar; return s;}
  friend TRMatrix operator-(const TRMatrix &source, Double_t scalar) {TRMatrix s(source); s -= scalar; return s;}
  friend TRMatrix operator-(Double_t scalar, const TRMatrix &source) {TRMatrix s(source); s -= scalar; return s;}
  ClassDef(TRMatrix,1)  // TRMatrix class (double precision)
};
std::ostream& operator<<(std::ostream& s,const TRMatrix &target);
inline Double_t &TRMatrix::operator()(Int_t i,Int_t j){
  if (j < 0 || j >= fNcols) {
    ::Error("TRMatrix::operator()", "index j %d out of bounds (size: %d, this: %p)", 
	    j, fNcols, this); 
    j = 0;
  }
  if (i < 0 || i >= fNrows) {
    ::Error("TRMatrix::operator()", "index i %d out of bounds (size: %d, this: %p)", 
	    i, fNrows, this); 
    i = 0;
  }
  return TArrayD::operator[](j + i*fNcols);
}
#endif
