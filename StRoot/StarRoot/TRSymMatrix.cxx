#include <iomanip>
#include "TRVector.h"
#include "TRSymMatrix.h"
#include "TError.h"
ClassImp(TRSymMatrix);
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows) :  
  TRArray(nrows*(nrows+1)/2), fNrows(nrows) {;}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,Double_t *Array) :  
  TRArray(nrows*(nrows+1)/2,Array), fNrows(nrows) {;}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,Float_t *Array) :  
  TRArray(nrows*(nrows+1)/2,Array), fNrows(nrows) {;}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,const Char_t *s) : 
  TRArray(nrows*(nrows+1)/2,s), fNrows(nrows) {;}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(Int_t nrows,Double_t a0, ...) : 
  TRArray(nrows*(nrows+1)/2), fNrows(nrows) {
  __VA_LIST__(a0);
}
//________________________________________________________________________________
TRSymMatrix::~TRSymMatrix(){;}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(ETRMatrixCreatorsOp kop,Int_t nrows) :
  TRArray(nrows*nrows), fNrows(nrows){
 switch (kop) {
  case kZero:
    break;
  case kUnit:
    for (int i=0; i<fNrows; i++) fArray[i*(i+1)/2] = 1;
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
  
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRSymMatrix& S,ETRMatrixCreatorsOp kop) {
  switch (kop) {
  case kInvertedPosDef:
  case kInverted:
    fNrows = S.GetNcols();
    Set(fNrows*(fNrows+1)/2);
    TCL::trsinv(S.GetArray(),fArray, fNrows);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRMatrix& A) {
  Int_t NI = A.GetNrows(); fNrows = NI;
  Int_t NJ = A.GetNcols();
  assert(NI == NJ);
  Set(fNrows*(fNrows+1)/2);
  TCL::trpck(A.GetArray(),fArray,NI);
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop,const TRSymMatrix& S) {
  Int_t M, N;
  switch (kop) { // 
  case kAxSxAT: //A[M,N]*S[N,N]*AT[M,N] => R[M,M]; 
    M = A.GetNrows();
    N = S.GetNrows();
    assert(N == A.GetNcols());
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::trasat(A.GetArray(),S.GetArray(),fArray,M,N);
    break;
  case kATxSxA: //BT[N,M]*S[N,N]*B[N,M] => R[M,M]; 
    M = A.GetNcols();
    N = S.GetNrows();
    assert(N == A.GetNrows());
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::tratsa(A.GetArray(),S.GetArray(),fArray,M,N);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRSymMatrix& Q,ETRMatrixCreatorsOp kop,const TRSymMatrix& T){
  assert (kop == kRxSxR);
  Int_t M = Q.GetNcols();
  assert(M == T.GetNcols());
  fNrows = M;
  Set(fNrows*(fNrows+1)/2);
  TCL::trqsq(Q.GetArray(),T.GetArray(),fArray,M);
}
//________________________________________________________________________________
TRSymMatrix::TRSymMatrix(const TRMatrix& A,ETRMatrixCreatorsOp kop) {
  Int_t M, N;
  switch (kop) {
  case kAxAT: // A[M,N]*AT[M,N] => S[M,M]
    M = A.GetNrows();    
    N = A.GetNcols();    
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::traat(A.GetArray(),fArray,M,N);
    break;
  case kATxA: // AT[N,M]*A[N,M] => S[M,M]
    N = A.GetNrows();    
    M = A.GetNcols();    
    fNrows = M;
    Set(fNrows*(fNrows+1)/2);
    TCL::trata(A.GetArray(),fArray,M,N);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
}
//________________________________________________________________________________
Double_t TRSymMatrix::Product(const TRVector& A,ETRMatrixCreatorsOp kop) {
  Int_t M, N; // N == 1
  Double_t Value;
  switch (kop) { // 
  case kAxSxAT: //A[M,N]*S[N,N]*AT[M,N] => R[M,M]; 
    M = A.GetNrows();
    N = GetNrows();
    assert(N == A.GetNcols());
    TCL::trasat(A.GetArray(),GetArray(),&Value,M,N);
    break;
  case kATxSxA: //BT[N,M]*S[N,N]*B[N,M] => R[M,M]; 
    M = A.GetNcols();
    N = GetNrows();
    assert(N == A.GetNrows());
    TCL::tratsa(A.GetArray(),GetArray(),&Value,M,N);
    break;
  default:
    Error("TRSymMatrix(ETRMatrixCreatorsOp)", "operation %d not yet implemented", kop);
  }
  return Value;
}
//________________________________________________________________________________
ostream& operator<<(ostream& s,const TRSymMatrix &target) {
  static const int width = 10;
  Int_t Nrows = target.GetNrows();
  const Double_t *Array = target.GetArray();
  s << "Semi Positive DefinedSymMatrix Size \t[" 
    << Nrows << "," << Nrows << "]" << endl;
  if (Array) {
    s.setf(ios::fixed,ios::scientific);
    s.setf(ios::showpos);
    for (int i = 0; i< Nrows; i++) {
      for (int j = 0; j <= i; j++)
	s << std::setw(width) << std::setprecision(width-3) << Array[i*(i+1)/2 + j] << ":\t";
      s << endl;
    }
    s.unsetf(ios::showpos);
  }
  else s << " Empty";   
  return s;
}
//________________________________________________________________________________
void TRSymMatrix::Print(Option_t *opt) const {if (opt) {}; cout << *this << endl;}
