#ifndef ROOT_TRSymMatrix
#define ROOT_TRSymMatrix
#include "TRArray.h"
class TRMatrix;
class TRVector;
class TRSymMatrix : public TRArray {
 public:
  TRSymMatrix(Int_t nrows=0);
  TRSymMatrix(Int_t nrows,Double_t *Array);
  TRSymMatrix(Int_t nrows,Float_t *Array);
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
 protected:
  Int_t     fNrows;            // number of rows 
 public:
  ClassDef(TRSymMatrix,1)  // TRSymMatrix class (double precision)
};
ostream& operator<<(ostream& s,const TRSymMatrix &target);
#endif

