#ifndef ROOT_TRMatrix
#define ROOT_TRMatrix
#include "TError.h"
#include "TRArray.h"
class TRSymMatrix;
#include "TRSymMatrix.h"
class TRMatrix : public TRArray {
 public:
  TRMatrix(Int_t nrows=0,Int_t ncols=0);
  TRMatrix(Int_t nrows,Int_t ncols,const Double_t *Array);
  TRMatrix(Int_t nrows,Int_t ncols,const Float_t *Array);
  TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRMatrix& B);
  TRMatrix(ETRMatrixCreatorsOp kop,Int_t nrows);
  TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop);
  TRMatrix(Int_t nrows,Int_t ncols,const Char_t *s);
  TRMatrix(const TRSymMatrix &S, ETRMatrixCreatorsOp kop,const TRMatrix& A);
  TRMatrix(const TRMatrix& A, ETRMatrixCreatorsOp kop,const TRSymMatrix &S);
  TRMatrix(const TRSymMatrix &S);
  TRMatrix(const TRMatrix &S, Int_t NI, Int_t NJ=0, Int_t I=1, Int_t J=1);
#ifndef __CINT__
  TRMatrix(Int_t nrows,Int_t ncols,Double_t a0, ...);
#endif
  TRMatrix(const TRMatrix &matrix);
  TRMatrix    &operator=(const TRMatrix &rhs);
  virtual ~TRMatrix(){;}
  Int_t GetNrows()  const       {return fNrows;} 
  Int_t GetNcols()  const       {return fNcols;}
  Int_t NI()        const       {return fNrows;} 
  Int_t NJ()        const       {return fNcols;}
  void  SetMatrix(Int_t nrows,Int_t ncols,const  Double_t *array=0);  
  ETRMatrixType GetMatrixType() const {return kRectangular;}
  Double_t       &operator()(Int_t i)         {return TRArray::operator[](i);}
  Double_t       &operator()(Int_t i,Int_t j);
 protected:
  Int_t     fNrows;            // number of rows 
  Int_t     fNcols;            // number of columns
 public:
  void Add(const TRMatrix& A, ETRMatrixCreatorsOp kop,const  TRMatrix& B); 
  void Substruct(const TRMatrix& A, ETRMatrixCreatorsOp kop,const  TRMatrix& B); 
  virtual void Print(Option_t *opt="") const;
  ClassDef(TRMatrix,1)  // TRMatrix class (double precision)
};
ostream& operator<<(ostream& s,const TRMatrix &target);
inline Double_t &TRMatrix::operator()(Int_t i,Int_t j){
  if (j < 0 || j >= fNcols) {
    ::Error("TRMatrix::operator()", "index j %d out of bounds (size: %d, this: 0x%08x)", 
	    j, fNcols, this); 
    j = 0;
  }
  if (i < 0 || i >= fNrows) {
    ::Error("TRMatrix::operator()", "index i %d out of bounds (size: %d, this: 0x%08x)", 
	    i, fNrows, this); 
    i = 0;
  }
  return TArrayD::operator[](j + i*fNcols);
}
#endif
