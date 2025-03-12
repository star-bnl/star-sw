#ifndef ROOT_TRSymMatrix
#define ROOT_TRSymMatrix
#include "TRArray.h"
#include <assert.h>
class TRMatrix;
class TRVector;
class TDiagMatrix;
class TRSymMatrix : public TRArray {
 public:
  TRSymMatrix(Int_t nrows=0) : TRArray(nrows*(nrows+1)/2), fNrows(nrows) {}
  TRSymMatrix(Int_t nrows,const Double_t *Array);
  TRSymMatrix(Int_t nrows,const Float_t *Array);
  TRSymMatrix(Int_t nrows,const Char_t *s);
  TRSymMatrix(const TRSymMatrix& W,ETRMatrixCreatorsOp kop);
  TRSymMatrix(ETRMatrixCreatorsOp kop,Int_t nrows);
  TRSymMatrix(const TRMatrix& A);
  TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop,const TRSymMatrix& S);
  TRSymMatrix(const TRSymMatrix& Q,ETRMatrixCreatorsOp kop,const TRSymMatrix& T);
  TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop);
#if !defined(__CINT__) && !defined(__CLING__)
  TRSymMatrix (Int_t nrows, Double_t a0, ...);
#endif
  virtual ~TRSymMatrix() {}
  void  Inverse() { TCL::trsinv(fArray,fArray, fNrows);}
  Int_t GetNrows()  const       {return fNrows;} 
  Int_t GetNcols()  const       {return GetNrows();}
  virtual ETRMatrixType GetMatrixType() const {return kSemiPosDefinedSymMatrix;}
  virtual Double_t Product(const TRVector& A,ETRMatrixCreatorsOp kop=kAxSxAT);
  virtual Int_t    SpmInv(const TRSymMatrix &S, TRVector *B = 0);
  static  Int_t    spminv(Double_t *v, Double_t *b, Int_t n, 
			  Int_t &nrank, Double_t *diag, Bool_t *flag);
  virtual void    Print(Option_t *opt="") const;
  void            Print(Int_t I, Int_t N = -1) const;
  Double_t       &operator()(Int_t i)         {return TRArray::operator[](i);}
  Double_t        operator()(Int_t i) const   {return TRArray::operator[](i);}
  static Int_t    IJ(Int_t i, Int_t j) {return ( j<=i ) ? i*(i+1)/2+j :j*(j+1)/2+i;}
  Double_t       &operator()(Int_t i,Int_t j) {return TRArray::operator[](IJ(i,j));}
  Double_t        operator()(Int_t i,Int_t j) const {return TRArray::operator[](IJ(i,j));}
  void AddRow(const Double_t *row) {
    fNrows++; Set(fNrows*(fNrows+1)/2); memcpy(fArray+(fNrows-1)*fNrows/2, row, fNrows*sizeof(Double_t));
  }
  void AddRow(const Double_t row) {//Diagonal element only
    fNrows++; Set(fNrows*(fNrows+1)/2); fArray[(fNrows+1)*fNrows/2-1] = row;
  }
  static Int_t TrsInv(const Double_t *g, Double_t *gi, Int_t n);
  static Int_t TrInv(const Double_t *g, Double_t *gi, Int_t n);
  static Int_t TrchLU(const Double_t *g, Double_t *gi, Int_t n);
  static Int_t TrsmUL(const Double_t *g, Double_t *gi, Int_t n);
 protected:
  Int_t     fNrows;            // number of rows 
 public:
  ClassDef(TRSymMatrix,1)  // TRSymMatrix class (double precision)
};
std::ostream& operator<<(std::ostream& s,const TRSymMatrix &target);
#endif

