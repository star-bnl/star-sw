#ifndef ROOT_TRDiagMatrix
#define ROOT_TRDiagMatrix
#include "TRArray.h"
class TRMatrix;
class TRVector;
class TRDiagMatrix : public TRArray {
 public:
  TRDiagMatrix(Int_t nrows=0) : TRArray(nrows), fNrows(nrows) {}
  TRDiagMatrix(Int_t nrows,const Double_t *Array) : TRArray(nrows,Array), fNrows(nrows) {}
  TRDiagMatrix(Int_t nrows,const Float_t *Array) : TRArray(nrows,Array), fNrows(nrows) {}
  TRDiagMatrix(Int_t nrows,const Char_t *s) : TRArray(nrows,s), fNrows(nrows) {}
  TRDiagMatrix(const TRDiagMatrix& W,ETRMatrixCreatorsOp kop);
#ifndef __CINT__
  TRDiagMatrix (Int_t nrows, Double_t a0, ...);
#endif
  virtual ~TRDiagMatrix() {}
  void  Inverse() { TCL::trsinv(fArray,fArray, fNrows);}
  Int_t GetNrows()  const       {return fNrows;} 
  Int_t GetNcols()  const       {return GetNrows();}
  virtual ETRMatrixType GetMatrixType() const {return kDiagonalMatrix;}
  virtual Double_t Product(const TRVector& A,ETRMatrixCreatorsOp kop);
  virtual void    Print(Option_t *opt="") const;
  Double_t       &operator()(Int_t i)                    {return TRArray::operator[](i);}
  Double_t        operator()(Int_t i) const              {return TRArray::operator[](i);}
  Double_t       &operator()(Int_t i,Int_t j);
  Double_t        operator()(Int_t i,Int_t j) const {return operator()(i,j);}
  void AddRow(const Double_t *row) {
    fNrows++; Set(fNrows*(fNrows+1)/2); memcpy(fArray+(fNrows-1)*fNrows/2, row, fNrows*sizeof(Double_t));
  }
  void AddRow(const Double_t row) {//Diagonal element only
    fNrows++; Set(fNrows*(fNrows+1)/2); fArray[(fNrows+1)*fNrows/2-1] = row;
  }
 protected:
  Int_t     fNrows;            // number of rows 
 public:
  ClassDef(TRDiagMatrix,1)  // TRDiagMatrix class (double precision)
};
ostream& operator<<(ostream& s,const TRDiagMatrix &target);
inline Double_t &TRDiagMatrix::operator()(Int_t i,Int_t j){
  if (j < 0 || j >= fNrows) {
    ::Error("TRDiagMatrix::operator()", "index j %d out of bounds (size: %d, this: %p)", 
	    j, fNrows, this); 
    j = 0;
  }
  if (i < 0 || i >= fNrows) {
    ::Error("TRDiagMatrix::operator()", "index i %d out of bounds (size: %d, this: %p)", 
	    i, fNrows, this); 
    i = 0;
  }
  Int_t m = i;
  Int_t l = j;
  if (i > j) {m = j; l = i;}
  return TArrayD::operator[](m + (l+1)*l/2);
}
#endif

